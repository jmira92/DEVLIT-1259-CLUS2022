package com.example.vlan2;

import java.util.Arrays;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import com.example.vlan2.namespaces.*;

import com.tailf.conf.*;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.cdb.*;
import com.tailf.maapi.*;
import com.tailf.ncs.annotations.*;

import java.net.Socket;
import java.net.SocketException;
import java.net.SocketAddress;
import java.util.EnumSet;
import java.util.Iterator;
import java.net.InetAddress;
import java.util.ArrayList;



public class ConfigCdbSub implements ApplicationComponent {
    private static Logger LOGGER = LogManager.getLogger(ConfigCdbSub.class);

    private CdbSubscription sub = null;
    private CdbSession wsess;

    public ConfigCdbSub() {
    }

    @Resource(type=ResourceType.CDB, scope=Scope.CONTEXT,
              qualifier="reactive-fm-loop-subscriber")
    private Cdb cdb;

    @Resource(type=ResourceType.CDB, scope=Scope.CONTEXT,
              qualifier="w-reactive-fm-loop")
    private Cdb wcdb;

    @Resource(type=ResourceType.MAAPI, scope=Scope.INSTANCE,
              qualifier="reactive-fm-m")
    private Maapi maapi;

    public void init() {
        try {
            wsess = wcdb.startSession(CdbDBType.CDB_OPERATIONAL);

            // system session, either we must pick up the NB ussername through
            // the fastmap data, or we must have a dedicated user that is
            // allowed
            // to do this. Authgroup and credentials are needed to to redeploy
            // since that might touch the network.
            maapi.startUserSession("admin",
                                   InetAddress.getByName("localhost"),
                                   "system",
                                   new String[] {"admin"},
                                   MaapiUserSessionFlag.PROTO_TCP);

            sub = cdb.newSubscription();
            int subid = sub.subscribe(1, new allocVlanService(),
                                      "/avl:alloc-vlan-data");
            // tell CDB we are ready for notifications
            sub.subscribeDone();
        }
        catch (Exception e) {
            LOGGER.error("", e);
        }
    }

    public void run() {
        try {
            while(true) {
                int[] points= sub.read();
                EnumSet<DiffIterateFlags> enumSet =
                    EnumSet.<DiffIterateFlags>of(
                        DiffIterateFlags.ITER_WANT_PREV,
                        DiffIterateFlags.ITER_WANT_ANCESTOR_DELETE,
                        DiffIterateFlags.ITER_WANT_SCHEMA_ORDER);
                ArrayList<Request> reqs = new ArrayList<Request>();
                try {
                    sub.diffIterate(points[0],
                                    new Iter(sub),
                                    enumSet, reqs);
                }
                catch (Exception e) {
                    reqs = null;
                }

                // It's important that we return as early as possible her,
                // This is a common technique, gather the work to do, tell
                // CDB that we're done and then do the work.
                // It could be that the allocator needs to reach out (RPC)
                // and that can be slow

                sub.sync(CdbSubscriptionSyncType.DONE_PRIORITY);


                for (Request req : reqs) {
                    if ((req.op == Operation.CREATE) &&
                        (req.t == Type.UNIT)) {


                    // This is the position in the code where the FM code
                    // requests that we allocate a unit, let's do that
                    int unit = alloc_unit();

                    // Write the result and redeploy
                    System.out.println("SET: " + req.path + "/unit ->" + unit);
                    wsess.setElem(new ConfBuf(unit+""), req.path + "/unit");

                    // redeploy the service that consumes this data, runs
                    // in separate thread
                    redeploy("/avl:alloc-vlan{%x}/reactive-re-deploy",
                             req.key, maapi);
                    }

                    else if ((req.op == Operation.CREATE) &&
                             (req.t == Type.VID)) {

                    // This is the position in the code where the FM code
                    // requests that we allocate a vlan-id, let's do that
                    int vid = alloc_vid();

                    // Write the result and redeploy
                    System.out.println("SET: " + req.path + "/vln-id ->" + vid);
                    wsess.setElem(new ConfUInt16(vid), req.path + "/vlan-id");

                    // redeploy the service that consumes this data
                    redeploy("/avl:alloc-vlan{%x}/reactive-re-deploy",
                             req.key, maapi);
                    }
                    else if (req.op == Operation.DELETE) {
                        // clean up oper data, and de-allocate

                        // Do we have a unit allocated?
                        try {
                            ConfValue v = wsess.getElem(req.path + "/unit");
                            deallocate_unit(v);
                            wsess.delete(req.path + "/unit");
                        }
                        // No we didn't
                        catch (Exception e) {
                            ;
                        }
                        // Do we have a vlan-id allocated
                        try {
                            ConfValue v = wsess.getElem(req.path + "/vlan-id");
                            deallocate_vid(v);
                            wsess.delete(req.path + "/vlan-id");
                        }
                        // No we didn't
                        catch (Exception e) {
                            ;
                        }
                    }

                }
            }
        }
        catch (SocketException e) {
            // silence here, normal close (redeploy/reload package)
            ;
        }
        catch (Exception e) {
            LOGGER.error("",e );
        }
    }

    public void finish() {
        safeclose(cdb);
        safeclose(wcdb);
        try {
            maapi.getSocket().close();
        }
        catch (Exception e) {
        }
    }

    private void safeclose(Cdb s) {
        try {s.close();}
        catch (Exception ignore) {}
    }


    private enum Operation { CREATE, DELETE}
    private enum Type { UNIT, VID}

    private class Request {
        ConfKey key;
        Operation op;
        Type t;
        ConfPath path;
    }

    private class Iter implements CdbDiffIterate {
        CdbSubscription cdbSub;

        Iter(CdbSubscription sub ) {
            this.cdbSub = sub;
        }

        public DiffIterateResultFlag iterate(
            ConfObject[] kp,
            DiffIterateOperFlag op,
            ConfObject oldValue,
            ConfObject newValue, Object initstate) {

            ArrayList<Request> reqs = (ArrayList<Request>) initstate;

            try {
                ConfPath p = new ConfPath(kp);
                System.out.println("ITER " + op + " " + p);
                ConfKey key = (ConfKey) kp[kp.length-2];
                Request r = new Request();
                r.path = p; r.key = key;

                if ((op == DiffIterateOperFlag.MOP_CREATED) &&
                    kp[0].toString().equals("avl:request-allocate-unit")) {
                    r.op = Operation.CREATE; r.t = Type.UNIT;
                    reqs.add(r);
                }
                else if ((op == DiffIterateOperFlag.MOP_CREATED) &&
                         kp[0].toString().equals("avl:request-allocate-vid")) {
                    r.op = Operation.CREATE; r.t = Type.VID;
                    reqs.add(r);
                }
                else if ((op == DiffIterateOperFlag.MOP_DELETED)) {
                    r.op = Operation.DELETE;
                    reqs.add(r);
                }
            }
            catch (Exception e) {
                LOGGER.error("", e);
            }
            return DiffIterateResultFlag.ITER_RECURSE;

        }
    }

    // Fake allocators
    private int[] units = null;
    private int[] vids = null;

    private int alloc_unit() {
        if (units == null) {
            units = new int[256];
            for (int i = 0; i < 256; i++)
                units[i]=-1;
        }

        for (int i =0; i < 256; i++) {
            if (units[i] == -1) {
                units[i] = i;
                return i;
            }
        }
        return -1;
    }

    private void deallocate_unit(ConfValue v) {
        int i = Integer.parseInt(v.toString());
        units[i] = -1;
    }


    private int alloc_vid() {
        if (vids == null) {
            vids = new int[256];
            for (int i =0; i < 256; i++)
                vids[i]=-1;
        }

        for (int i =0; i < 256; i++) {
            if (vids[i] == -1) {
                vids[i] = i;
                return i;
            }
        }
        return -1;
    }

    private void deallocate_vid(ConfValue v) {
        long i = ((ConfUInt16)v).longValue();
        vids[(int)i] = -1;
    }

    // redeploy MUST be done in another thread, if not system
    // hangs, since the CDB subscriber cannot do its work
    private void redeploy(String path, ConfKey k, Maapi m) {
        Redeployer r = new Redeployer(path, k, m);
        Thread t = new Thread(r);
        t.start();
    }


    private class Redeployer implements Runnable {
        private String path;
        private ConfKey k;
        private Maapi m;

        public Redeployer(String path, ConfKey k, Maapi m) {
            this.path = path; this.k = k; this.m = m;
        }

        public void run() {
            try {
                m.requestAction(new ConfXMLParam[] {},
                                "/avl:alloc-vlan{%x}/reactive-re-deploy",
                                k);
            } catch (Exception e) {
                LOGGER.error("error in reactive-re-deploy", e);
                throw new RuntimeException("error in reactive-re-deploy", e);
            }
        }
    }
}
