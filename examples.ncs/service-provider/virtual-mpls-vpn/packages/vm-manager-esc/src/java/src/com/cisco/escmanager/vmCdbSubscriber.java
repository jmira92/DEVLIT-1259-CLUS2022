package com.cisco.escmanager;

import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.text.SimpleDateFormat;

import java.util.*;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import java.util.Properties;

import com.cisco.escmanager.namespaces.*;
import com.cisco.vmmanager.namespaces.*;
import com.tailf.conf.*;
import com.tailf.cdb.*;
import com.tailf.maapi.*;
import com.tailf.navu.*;
import com.tailf.ncs.*;
import com.tailf.ncs.ns.Ncs;
import com.tailf.dp.*;
import com.tailf.dp.annotations.*;
import com.tailf.dp.proto.*;
import com.tailf.dp.services.*;
import com.cisco.resourcemanager.*;
import com.cisco.resourcemanager.namespaces.*;

import java.net.HttpURLConnection;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketAddress;
import java.net.InetAddress;

import com.tailf.ncs.ResourceManager;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;

import com.tailf.ncs.ns.Ncs;
import com.tailf.ncs.template.Template;
import com.tailf.ncs.template.TemplateVariables;

public class vmCdbSubscriber implements ApplicationComponent {
    private static Logger LOGGER = LogManager.getLogger(vmCdbSubscriber.class);

    private CdbSubscription sub = null;
    private CdbSession wsess;
    private CdbSession presess;
    private CdbSession osess;

    public vmCdbSubscriber() {
    }

    @Resource(type=ResourceType.CDB, scope=Scope.INSTANCE,
              qualifier="esc-manager-start-cdb")
    private Cdb cdb;

    @Resource(type=ResourceType.CDB, scope=Scope.INSTANCE,
              qualifier="esc-manager-start-cdb2")
    private Cdb wcdb;

    @Resource(type=ResourceType.CDB, scope=Scope.INSTANCE,
              qualifier="esc-manager-start-cdb3")
    private Cdb ocdb;

    @Resource(type = ResourceType.CDB, scope = Scope.INSTANCE,
              qualifier="esc-manager-start-cdb4")
    private Cdb precdb;

    @Resource(type=ResourceType.MAAPI, scope=Scope.INSTANCE,
              qualifier="esc-manager-start-maapi")
    private Maapi maapi;

    public void init() {
        try {
            System.err.println("starting vm cdb subscriber");

            // system session, either we must pick up the NB ussername through
            // the fastmap data, or we must have a dedicated user that is
            // allowed
            // to do this. Authgroup and credentials are needed to to redeploy
            // since that might touch the network.

            maapi.startUserSession("admin",
                                   maapi.getSocket().getInetAddress(),
                                   "system",
                                   new String[] {"admin"},
                                   MaapiUserSessionFlag.PROTO_TCP);

            sub = cdb.newSubscription();
            String vmStartPath = "/vmm:vm-manager/vmm:start";

            int subid = sub.subscribe(1, new vmManager(), vmStartPath);

            sub.subscribeDone();
        }
        catch (Exception e) {
            LOGGER.error("", e);
        }
    }

    public void run() {
        try {

            while(true) {
                int[] points;

                try {
                    points = sub.read();
                }
                catch (Exception e) {
                    return;
                }

                LOGGER.info("Iterating subs ");
                EnumSet<DiffIterateFlags> enumSet =
                    EnumSet.<DiffIterateFlags>of(
                        DiffIterateFlags.ITER_WANT_PREV,
                        DiffIterateFlags.ITER_WANT_ANCESTOR_DELETE,
                        DiffIterateFlags.ITER_WANT_SCHEMA_ORDER);

                ArrayList<Request> reqs = new ArrayList<Request>();

                try {
                    try {
                        wsess = wcdb.startSession(CdbDBType.CDB_RUNNING);
                        osess = ocdb.startSession(CdbDBType.CDB_OPERATIONAL);
                        presess = precdb.startSession(
                            CdbDBType.CDB_PRE_COMMIT_RUNNING);

                        sub.diffIterate(points[0],
                                        new Iter(sub),
                                        enumSet, reqs);

                        // It's important that we return as early as
                        // possible here, This is a common technique,
                        // gather the work to do, tell CDB that we're
                        // done and then do the work.  It could be
                        // that the allocator needs to reach out (RPC)
                        // and that can be slow

                        ReqHandler req = new ReqHandler();
                        for (Request r : reqs) {
                            LOGGER.info("vm-start request received: " +
                                        r.toString());
                            req.addRequest(r);
                        }

                        Thread t = new Thread(req);
                        t.start();

                    } finally {
                        sub.sync(CdbSubscriptionSyncType.DONE_PRIORITY);

                        wsess.endSession();
                        osess.endSession();
                        presess.endSession();
                    }


                } catch (Exception e) {
                    // ignore
                    LOGGER.error("", e);
                }
            }

        } catch (Exception e) {
            LOGGER.error("", e);
        }
    }

    public void finish() {
        safeclose(cdb);
        try {
            wsess.endSession();
        } catch (Exception e) {
            ;
        }
        try {
            osess.endSession();
        } catch (Exception e) {
            ;
        }
        try {
            maapi.getSocket().close();
        }
        catch (Exception e) {
            ;
        }
    }

    private void safeclose(Cdb s) {
        try {s.getSocket().close();}
        catch (Exception ignore) {}
    }

    private enum Operation {
        CREATED,
        DELETED
    };

    private class Request {
        Operation op;
        ConfPath path;

        ConfKey   name;

        HashMap <String,ConfValue> varmap;

        ArrayList<RequestInterface> interfaces;
        ArrayList<RequestScalingPool> scalingPools;
        ArrayList<String> deletedDevices;

        public String toString() {
            return "REQ: vm: "+name+" op: "+op;
        }
    }

    private class RequestInterface {
        ConfValue id;
        ConfValue name;
        ConfValue ip;
        public String toString() {
            return "REQ Interface: id: "+id+" name: "+name+" ip: "+ip;
        }
    }

    private class RequestScalingPool {
        ConfValue name;
        ArrayList<ConfValue> addresses;
    }

    private class Iter implements CdbDiffIterate {
        CdbSubscription cdbSub;

        Iter(CdbSubscription sub) {
            this.cdbSub = sub;
        }

        public DiffIterateResultFlag iterate(
            ConfObject[] kp,
            DiffIterateOperFlag op,
            ConfObject oldValue,
            ConfObject newValue, Object initstate) {

            @SuppressWarnings("unchecked")
            ArrayList<Request> reqs = (ArrayList<Request>) initstate;

            try {
                Request r = new Request();
                ConfObject[] vmkp = new ConfObject[3];

                r.name = (ConfKey)kp[kp.length - 3];
                vmkp[0] = r.name;
                vmkp[1] = kp[kp.length-2];
                vmkp[2] = kp[kp.length-1];
                r.path = new ConfPath(vmkp);

                LOGGER.info("ITER " + op + " " + r.path +
                            " ("+new ConfPath(kp)+")");

                if (op == DiffIterateOperFlag.MOP_CREATED ||
                    op == DiffIterateOperFlag.MOP_VALUE_SET) {
                    r.op = Operation.CREATED;

                    HashMap<String, ConfValue> varmap =
                        new HashMap<String, ConfValue>();

                    String[] leaves = { "name",
                                        "deployment-name",
                                        "vm-device",
                                        "tenant",
                                        "service-name",
                                        "service-version",
                                        "vm-type",
                                        "vm-group",
                                        "mgmt-ip-address",
                                        "day0-url",
                                        "scaling-min",
                                        "scaling-max" };

                    for(String leaf: leaves) {
                        ConfValue val = wsess.getElem(r.path+"/"+leaf);
                        if (val != null)
                            varmap.put(leaf, val);
                    }

                    r.varmap         = varmap;
                    r.interfaces     = getInterfaces(r.path);
                    r.scalingPools   = getScalingPools(r.path);
                }
                else if (op == DiffIterateOperFlag.MOP_DELETED) {
                    r.op = Operation.DELETED;
                    /* extract list of devices started by this service */
                    String path = "/vmm:vm-manager/vmm:start"+r.name+
                        "/vmm:device";
                    int devCount = osess.getNumberOfInstances(path);
                    ArrayList<String> devices = new ArrayList<String>();
                    for(int i=0 ; i < devCount ; i++) {
                        devices.add(osess.getElem(path+"["+i+"]/name").
                                    toString());
                    }
                    r.deletedDevices = devices;
                    LOGGER.info("devices to be removed: "+devices);
                }
                else if (op == DiffIterateOperFlag.MOP_MODIFIED) {
                    return DiffIterateResultFlag.ITER_RECURSE;
                }

                reqs.add(r);

                return DiffIterateResultFlag.ITER_CONTINUE;
            }
            catch (Exception e) {
                LOGGER.error("", e);
                return DiffIterateResultFlag.ITER_CONTINUE;
            }
        }
    }

    private ArrayList<RequestInterface> getInterfaces(ConfPath path)
        throws Exception
    {
        int max = wsess.getNumberOfInstances(path+"/interface");

        ArrayList<RequestInterface> res = new ArrayList<RequestInterface>();

        for(int i=0 ; i < max ; i++) {
            RequestInterface iface = new RequestInterface();

            iface.id = wsess.getElem(path+"/interface["+i+"]/id");
            iface.name = wsess.getElem(path+"/interface["+i+"]/name");
            iface.ip = wsess.getElem(path+"/interface["+i+"]/ip");

            res.add(iface);
        }

        return res;
    }

    private ArrayList<RequestScalingPool> getScalingPools(ConfPath path)
        throws Exception
    {
        int max = wsess.getNumberOfInstances(path+"/scaling-pool");

        ArrayList<RequestScalingPool> res = new ArrayList<RequestScalingPool>();

        for(int i=0 ; i < max ; i++) {
            String ppath = path+"/scaling-pool["+i+"]";

            RequestScalingPool pool = new RequestScalingPool();
            pool.name = wsess.getElem(ppath+"/name");

            int amax = wsess.getNumberOfInstances(ppath+"/address");
            ArrayList<ConfValue> addresses = new ArrayList<ConfValue>();
            for(int j=0 ; j < amax ; j++) {
                ConfValue address = wsess.getElem(ppath+"/address["+j+"]/ip");
                addresses.add(address);
            }
            pool.addresses = addresses;

            res.add(pool);
        }

        return res;
    }

    // redeploy MUST be done in another thread, if not system hangs, since
    // the CDB subscriber cannot do its work


    private static void safeClose(Socket s) {
        try {
            s.close();
        }
        catch (Exception  e) {
            ;
        }
    }

    private class ReqHandler implements Runnable {

        private Maapi m;
        private Socket s, cs;
        private Request r;
        private Cdb rcdb;

        ArrayList<Request> reqs = new ArrayList<Request>();

        public void addRequest(Request request) {
            reqs.add(request);
        }

        public ReqHandler() {
            try {
                s = new Socket(NcsMain.getInstance().getNcsHost(),
                               NcsMain.getInstance()
                               .getNcsPort());
                m = new Maapi(s);

                cs = new Socket(NcsMain.getInstance().getNcsHost(),
                                NcsMain.getInstance()
                                .getNcsPort());
                rcdb = new Cdb("esc-manager-start-req", cs);

                m.startUserSession("admin", m.getSocket().getInetAddress(),
                                   "system", new String[] {"admin"},
                                   MaapiUserSessionFlag.PROTO_TCP);

            } catch (Exception e) {
                LOGGER.error("vm-start request handler exception: " + e);
            }
        }


        public void run() {

            int tHandle = 0;
            try {

                tHandle = m.startTrans(Conf.DB_RUNNING, Conf.MODE_READ_WRITE);
                while (!this.reqs.isEmpty()) {

                    LOGGER.info("Inside Run: No of reqs:" + reqs.size());
                    this.r = reqs.remove(0);
                    LOGGER.info("Req: " + r.toString());
                    try {
                        NavuContainer vmRoot =
                            new NavuContainer(m, tHandle,
                                              com.cisco.vmmanager.namespaces.
                                              vmManager.hash);

                        NavuList vmService =
                            vmRoot.container("vm-manager").
                            list("escm", "esc");

                        if (r.op == Operation.CREATED) {
                            NavuContainer vm = vmService.safeCreate(r.name);

                            LOGGER.info("varmap=" + r.varmap);

                            for (Map.Entry<String, ConfValue> entry :
                                     r.varmap.entrySet()) {
                                if (!entry.getKey().equals("name")) {
                                  vm.leaf(entry.getKey()).set(entry.getValue());
                                }
                            }

                            NavuList ifL = vm.list("interface");

                            for (RequestInterface iface : r.interfaces) {
                                NavuContainer ifC =
                                    ifL.safeCreate(new ConfKey(iface.id));
                                ifC.leaf("name").set(iface.name);
                                if (iface.ip != null)
                                    ifC.leaf("ip").set(iface.ip);
                            }

                            NavuList poolL = vm.list("scaling-pool");
                            for (RequestScalingPool pool : r.scalingPools) {
                                NavuContainer poolC =
                                    poolL.safeCreate(new ConfKey(pool.name));

                                NavuList addressL = poolC.list("address");

                                for (ConfValue address : pool.addresses)
                                    addressL.safeCreate(new ConfKey(address));
                            }

                        } else if (r.op == Operation.DELETED) {

                            LOGGER.info("deleting: " + r.name);
                            if (vmService.containsNode(r.name)) {
                                vmService.delete(r.name);
                                NavuList devL =
                                    (NavuList) vmService.getNavuNode(
                                        new ConfPath("/devices/device"));
                                String serviceName =
                                    r.name.elementAt(0).toString();

                                // deregisterLicenses(r, m, rcdb, LOGGER);
                                for(String dev: r.deletedDevices) {
                                    devL.delete(dev);
                                    try {
                                        maapi.safeDelete(
                                            tHandle,
                                            "/webui:webui/data-stores/l3vpnui:"+
                                            "static-map/device{%s}", dev);
                                    }
                                    catch (Exception e) {
                                        LOGGER.info(
                                            "No webui static map device "+
                                            "to delete: " + dev);
                                    }
                                }
                            }
                        }
                    } catch (Exception e) {
                        // ignore
                        LOGGER.error("", e);;
                    }
                }
                LOGGER.info("Done processing all requests");
                m.applyTrans(tHandle, false);
                LOGGER.info("Applied maapi transactions");
                LOGGER.info("Ending RequestHandler Thread for "+
                             "vm-manager/start: " + r.name);
            } catch (Exception e) {
                LOGGER.error(e.getMessage(), e);
            } finally {
                try {
                    m.finishTrans(tHandle);
                    LOGGER.info("Finished maapi transactions");
                    safeClose(s);
                } catch (Exception e) {
                    ;
                }
            }
        }
    }
}
