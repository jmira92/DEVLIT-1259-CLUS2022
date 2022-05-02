package com.cisco.escmanager;

import java.io.IOException;
import java.net.Socket;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Vector;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.cisco.vmmanager.namespaces.vmManager;
import com.example.weblog.WebUILogger;
import com.tailf.cdb.Cdb;
import com.tailf.cdb.CdbDBType;
import com.tailf.cdb.CdbDiffIterate;
import com.tailf.cdb.CdbSession;
import com.tailf.cdb.CdbSubscription;
import com.tailf.cdb.CdbSubscriptionSyncType;
import com.tailf.cdb.CdbSubscriptionType;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBool;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfDecimal64;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfKey;
import com.tailf.conf.ConfObject;
import com.tailf.conf.ConfObjectRef;
import com.tailf.conf.ConfPath;
import com.tailf.conf.ConfXMLParam;
import com.tailf.conf.ConfXMLParamValue;
import com.tailf.conf.DiffIterateFlags;
import com.tailf.conf.DiffIterateOperFlag;
import com.tailf.conf.DiffIterateResultFlag;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuContext;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuList;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.NcsMain;
import com.tailf.ncs.ResourceManager;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;
import com.tailf.ncs.ns.Ncs;

public class notifCdbSubscriber implements ApplicationComponent {
    private static Logger LOGGER = LogManager.getLogger(
            notifCdbSubscriber.class);

    private CdbSubscription sub = null;
    NavuContext context = null;

    // RequestHandler ( contains a queue)
    private Map<String,ReqHandler> requests =
        new Hashtable<String,ReqHandler>();

    public notifCdbSubscriber() {

    }

    @Resource(type=ResourceType.CDB, scope=Scope.INSTANCE,
              qualifier="esc-notif-fm-loop-subscriber")
              private Cdb cdb;

    @Resource(type=ResourceType.MAAPI, scope=Scope.INSTANCE,
              qualifier="esc-notif-reactive-fm-m")
              private Maapi maapi;


    public void init() {
        try {


            // system session, either we must pick up the NB ussername through
            // the fastmap data, or we must have a dedicated user that is
            // allowed
            // to do this. Authgroup and credentials are needed to to redeploy
            // since that might touch the network.

            maapi.startUserSession("system",
                                   maapi.getSocket().getInetAddress(),"system",
                                   new String[] {},
                                   MaapiUserSessionFlag.PROTO_TCP);
            context = new NavuContext(maapi);
            context.startOperationalTrans(Conf.MODE_READ);

            sub = cdb.newSubscription();
            String s ="/ncs:devices/device/netconf-notifications/"+
                "received-notifications";

            int subid = sub.subscribe(CdbSubscriptionType.SUB_OPERATIONAL,
                                      1, new Ncs(), s);

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

                System.out.println("Iterating subs ");
                EnumSet<DiffIterateFlags> enumSet =
                    EnumSet.<DiffIterateFlags>of(
                        DiffIterateFlags.ITER_WANT_PREV,
                        DiffIterateFlags.ITER_WANT_ANCESTOR_DELETE,
                        DiffIterateFlags.ITER_WANT_SCHEMA_ORDER);

                try {
                    sub.diffIterate(points[0],
                                    new Iter(sub, context),
                                    enumSet, null);

                    // It's important that we return as early as possible here,
                    // This is a common technique, gather the work to do, tell
                    // CDB that we're done and then do the work.
                    // It could be that the allocator needs to reach out (RPC)
                    // and that can be slow
                } catch (Exception ignore) {
                    //ignore
                } finally {
                    // Using the finally clause for the sync
                    // will make the sync robust even if the diff iteration
                    // fails
                    sub.sync(CdbSubscriptionSyncType.DONE_OPERATIONAL);
                }
            }
        }
        catch (Exception e) {
            LOGGER.error("",e );
        }
    }

    public void finish() {
        try {
            context.finishClearTrans();;
        } catch (Exception ignore) {}
        try {
            ResourceManager.unregisterResources(this);
        } catch (Exception ignore) {}
    }

    private void safeclose(Cdb s) {
        try {s.getSocket().close();}
        catch (Exception ignore) {}
    }

    private enum Operation {
        VM_ALIVE,
        VM_RECOVERY_COMPLETE,
        SERVICE_ALIVE,
        VM_RECOVERY_UNDEPLOYED,
        UNDEP_REDEP,
        VM_RECOVERY_COMPLETE_FAILURE
    };

    private class Request {
        String        serviceName;
        String        esc;
        Operation     op;
        ConfPath      path;
        NavuContainer notif;
        long          rcvdTime;
        String        depname;

        public String toString() {
            return
                "REQ: esc:" + esc +
                "op: " + op +
                "deployment: " + depname;
        }
    }

    private class Iter implements CdbDiffIterate {
        private CdbSubscription cdbSub;
        private NavuContext ctx;
        private NavuContainer base;

        Iter(CdbSubscription sub, NavuContext ctx) throws NavuException {
            this.cdbSub = sub;
            this.ctx = ctx;
            this.base = new NavuContainer(ctx);

        }

        public DiffIterateResultFlag iterate(
            ConfObject[] kp,
            DiffIterateOperFlag op,
            ConfObject oldValue,
            ConfObject newValue, Object initstate) {

            try {

                // if (Utils.isHaSlave()) {
                //     return DiffIterateResultFlag.ITER_STOP;
                // }
                ConfKey esc = (ConfKey)kp[kp.length - 3];
                String escStr = ((ConfBuf)esc.elementAt(0)).toString();

                ConfPath p = new ConfPath(kp);
                LOGGER.info("ITER " + op + " " + p);
                Request r = new Request();
                r.path = p;
                r.serviceName = null;

                if (op == DiffIterateOperFlag.MOP_CREATED) {
                    // netconf notif received
                    // here we must find which service is waiting for this VM
                    // to come alive

                    NavuContainer notif = (NavuContainer) base.getNavuNode(p);

                    NavuContainer e =
                        notif.container("data").container("esc", "escEvent");

                    String event =
                        e.container("event").leaf("type").valueAsString();
                    String depname = e.leaf("depname").valueAsString();

                    r.serviceName = findVMService(e);
                    r.depname = depname;
                    r.rcvdTime = new Date().getTime();

                    boolean notifDropped = false;

                    if (event.equals("VM_RECOVERY_UNDEPLOYED")) {
                        r.op = Operation.VM_RECOVERY_UNDEPLOYED;
                        r.notif = notif;
                        r.esc = escStr;
                    }

                    else if (event.equals("VM_ALIVE") &&
                             e.leaf("status").valueAsString().
                             equals("SUCCESS")) {


                        r.op = Operation.VM_ALIVE;
                        r.notif = notif;
                        r.esc = escStr;
                    }

                    else if (event.equals("VM_RECOVERY_COMPLETE") &&
                             e.leaf("status").valueAsString().
                             equals("SUCCESS")) {

                        r.op = Operation.VM_RECOVERY_COMPLETE;
                        r.notif = notif;
                        r.esc = escStr;
                    }
                    else if (event.equals("VM_RECOVERY_COMPLETE") &&
                             e.leaf("status").valueAsString().
                             equals("FAILURE")) {
                        r.op = Operation.UNDEP_REDEP;
                        r.notif = notif;
                        r.esc = escStr;
                    }

                    else if (event.equals("SERVICE_ALIVE") &&
                             ( e.leaf("status").valueAsString().
                               equals("FAILURE"))) {

                        r.op = Operation.UNDEP_REDEP;
                        r.notif = notif;
                        r.esc = escStr;
                    }
                    else {
                        notifDropped = true;
                        LOGGER.info("Dropping notif: " +
                                    e.container("event").leaf("type").
                                    valueAsString());
                    }

                    if (!notifDropped) {
                        LOGGER.debug("Notif Received");
                        if(requests.containsKey(depname)){
                            LOGGER.debug("Subsequent Notif for deployment:"
                                         +depname);
                            ReqHandler req = requests.get(depname);
                            req.addRequest(r);
                        }
                        else {
                            LOGGER.debug("First Notif for deployment:"+depname);
                            ReqHandler req = new ReqHandler();
                            req.addRequest(r);
                            requests.put(depname, req);
                            Thread t = new Thread(req);
                            t.start();
                        }
                    }

                    return DiffIterateResultFlag.ITER_CONTINUE;
                }
            }
            catch (Exception e) {
                LOGGER.error("", e);
            }
            return DiffIterateResultFlag.ITER_RECURSE;
        }

        public String findVMService(NavuContainer e)
            throws NavuException, ConfException
        {
            String tenant = e.leaf("tenant").valueAsString();
            if ("SystemAdminTenantId".equals(tenant)) {
                tenant = "admin";
            }
            String depName = e.leaf("depname").valueAsString();
            String vmGroup = e.leaf("vm_group").valueAsString();

            NavuList vmmList =
                (NavuList) e.getNavuNode(new ConfPath("/vmm:vm-manager/start"));

            for(NavuContainer vmStart: vmmList) {
                if (vmStart.leaf("deployment-name").valueAsString().
                    equals(depName) &&
                    vmStart.leaf("tenant").
                    valueAsString().equals(tenant) &&
                    vmStart.leaf("vm-group").
                    valueAsString().equals(vmGroup)) {
                    // found a match, return name
                    LOGGER.info("found vm-manager/start: "+
                                vmStart.leaf("name"));
                    return vmStart.leaf("name").valueAsString();
                }
            }

            LOGGER.info("no connected vm-manager/start service found: "+e);
            return null;
        }
    }

    // redeploy MUST be done in another thread, if not system hangs, since
    // the CDB subscriber cannot do its work

    private class ReqHandler implements Runnable {
        private String esc;
        private String vmId;
        private NavuContainer escEvent;
        private String depname;
        private Request req;
        private Maapi m;
        private Cdb rcdb;
        private Socket s, cs;

        // Event containers
        List<Request> queue = new Vector<Request>();

        public void addRequest(Request request) {
            queue.add(request);
        }

        public String getEventType(NavuContainer eventContainer)
            throws NavuException
        {
            return eventContainer.container("event").leaf("type")
                .valueAsString();
        }

        public List<NavuContainer> getVMAliveEvents()
            throws NavuException
        {
            Iterator itr = queue.iterator();
            List<NavuContainer> vmAliveEvents = new LinkedList<NavuContainer>();

            while(itr.hasNext())
                {
                    NavuContainer nc = (NavuContainer)itr.next();
                    String eventType = getEventType(nc);
                    if(eventType.equals("VM_ALIVE"))
                        vmAliveEvents.add(nc);
                }
            return vmAliveEvents;
        }

        public ReqHandler() {
            try {
                s = new Socket(NcsMain.getInstance().getNcsHost(),
                               NcsMain.getInstance().getNcsPort());
                m = new Maapi(s);

                cs = new Socket(NcsMain.getInstance().getNcsHost(),
                                NcsMain.getInstance().getNcsPort());
                rcdb = new Cdb("esc-manager" , cs);

                m.startUserSession("admin",
                                   m.getSocket().getInetAddress(),
                                   "system",
                                   new String[] {"admin"},
                                   MaapiUserSessionFlag.PROTO_TCP);

            } catch (Exception e) {
                System.err.println("redeployer exception: "+e);
            }
        }


        public String makeDevName(String tenant, String depname,
                                         String vmgroup, Logger LOGGER) {
            return makeDevName(tenant, depname, vmgroup, "0", LOGGER);
        }

        public String makeDevName(String tenant, String depname,
                                         String vmgroup, String num,
                                         Logger LOGGER) {
            return
                tenant + "_" +
                depname + "_" +
                vmgroup + "_" +
                num;        }

        public String mkVmName(NavuContainer e) throws NavuException {
            String tenant = escEvent.leaf("tenant").valueAsString();
            if ("SystemAdminTenantId".equals(tenant)) {
                tenant = "admin";
            }
            LOGGER.info("mkVmName tenant: " + tenant);

            return makeDevName(
                tenant,
                e.leaf("depname").valueAsString(),
                e.leaf("vm_group").valueAsString(),
                this.esc, LOGGER);
        }

        public String getNetsimPort(NavuContainer e) throws NavuException {
            // This is a netsim hack, the details contain port:num
            String[] tokens = e.container("event").
                leaf("details").valueAsString().split(":");
            return tokens[0];
        }

        public String getNedid(NavuContainer e) throws NavuException {

            // FIXME: this should be read from the vm-manager/start request
            // instead.

            String gr = e.leaf("vm_group").valueAsString();
            if (gr.equals("CSR")) {
                return new String("cisco-ios-cli-3.8");
            }
            return null;
        }

        private void printQueue() throws NavuException
        {
            LOGGER.debug("Print Queue:");
            for(Request r : queue) {
                try {
                    NavuContainer event = r.notif.container("data").
                        container("esc", "escEvent");
                    LOGGER.debug(" Operation:"+r.op.name()+
                                 " vm_group:"+event.leaf("vm_group").
                                 valueAsString());
                }
                catch (Exception e) {
                    ;
                }
            }
        }

        private Request containsOperation(String operation,
                                          String devName, int index)
            throws NavuException {

            LOGGER.info("Check contains operation "+operation +" for vnfName "
                        +devName +" appears after "+index);
            Iterator<Request> itr = queue.iterator();

            Request r = null;
            int i = 0;
            while(itr.hasNext())
                {
                    if (i <= index) {
                        i++;
                        itr.next();
                        continue;
                    }

                    Request req = itr.next();
                    String eventType =
                        req.notif.container("data").container("esc","escEvent")
                        .container("esc").leaf("type").valueAsString();
                    NavuContainer eEvent = req.notif.container("data").
                        container("esc", "escEvent");
                    String vnfName = mkVmName(eEvent);
                    if(eventType.equals(operation) && devName.equals(vnfName)){
                        LOGGER.info("Found Operation:"+operation);
                        r = req;
                        break;
                    }
                }
            return r;
        }

        private Request containsOperation(String operation,int index)
            throws NavuException {

            LOGGER.info("Check contains operation "+operation
                        +" appears after "+index);
            Iterator<Request> itr = queue.iterator();

            Request r = null;
            int i = 0;
            while(itr.hasNext())
                {
                    if(i<=index){
                        i++;
                        itr.next();
                        continue;
                    }

                    Request req = itr.next();
                    String eventType =
                        req.notif.container("data").container("esc","escEvent")
                        .container("esc").leaf("type").valueAsString();
                    NavuContainer eEvent = req.notif.container("data").
                        container("esc", "escEvent");
                    String vnfName = mkVmName(eEvent);
                    if(eventType.equals(operation)){
                        LOGGER.info("Found Operation:"+operation);
                        r = req;
                        break;
                    }
                }
            return r;
        }

        public boolean hasMoreElements(int i ) {
            LOGGER.info("Check queue has more elements after index:"+i);
            boolean empty = false;
            try{
                LOGGER.info("size of queue:"+queue.size());
                queue.get(i+1);
            }
            catch(IndexOutOfBoundsException e){
                empty = true;
            }

            return empty;
        }

        public void run() {
            int attempts = 0;
            try {

                while(!this.queue.isEmpty()){
                    if (attempts++ == 20) {
                        LOGGER.error(
                            "Giving up on this queue item, tried 20 times" +
                            "to process item");
                        queue.remove(0);
                        attempts = 0;
                        continue;
                    }

                    LOGGER.info("Inside Run: Size of Queue:"+queue.size());
                    printQueue();
                    this.req =  queue.get(0);
                    this.escEvent =
                        req.notif.container("data").
                        container("esc", "escEvent");
                    this.depname = req.depname;
                    this.esc = req.esc;

                    if (req.op == Operation.VM_ALIVE ||
                        req.op == Operation.VM_RECOVERY_COMPLETE) {

                        String vnfName = mkVmName(escEvent);
                        int tHandle = 0;

                        try {
                            tHandle = m.startTrans(
                                Conf.DB_RUNNING, Conf.MODE_READ_WRITE);

                            if (req.op == Operation.VM_ALIVE) {
                                LOGGER.info("Attempting VM_ALIVE");
                                vmAlive("VM_ALIVE", tHandle, req);
                                LOGGER.debug(" VM_ALIVE Done");
                            }
                            else {
                                LOGGER.info("Attempting VM_RECOVERY_COMPLETE");
                                vmAlive("VM_RECOVERY_COMPLETE", tHandle, req);
                                LOGGER.debug(" VM_RECOVERY_COMPLETE Done");
                            }
                        }
                        catch(Exception e) {

                            // FIXME, try to distinguish between
                            // communication failures
                            // and other errors

                            LOGGER.error("Received Exception during UP:"+
                                         e.getMessage(), e);
                            if(hasMoreElements(0)){
                                Request r  =
                                    containsOperation(
                                        Operation.VM_RECOVERY_UNDEPLOYED.name(),
                                        vnfName, 0);
                                if (r!=null) {
                                    queue.remove(0);
                                    queue.add(0, r);
                                }
                                else {
                                    LOGGER.info(
                                        "VmAlive failed and no disasters or"+
                                        " removals, "+
                                        "will retry  in 10 seconds");
                                    Thread.sleep(10000);
                                }
                            }
                            continue;
                        }
                        finally {
                            try{
                                m.finishTrans(tHandle);
                            }
                            catch(Exception e){
                                ;
                            }
                        }
                    }
                    else if (req.op == Operation.UNDEP_REDEP) {
                        LOGGER.info("Attempting UNDEP_REDEP - "+
                                    "not yet implemented");
                        // not yet implemented
                        // undepRedepAction();
                        queue.clear();
                    }
                    // Lets not bother with this at the moment.
                    //
                    // else if (req.op == Operation.VM_RECOVERY_UNDEPLOYED) {
                    //     LOGGER.info("Attempting VM_RECOVERY_UNDEPLOYED");
                    //     String devName = mkVmName(escEvent);
                    // }
                    else if (req.op == Operation.SERVICE_ALIVE) {
                        LOGGER.info("SERVICE_ALIVE, drop");
                    }

                    LOGGER.debug("Push Queue");
                    if (req.op != Operation.UNDEP_REDEP)
                        // we have cleared queue while handling undep_redep
                        if ( queue.size() > 0 )
                            queue.remove(0);

                }
                LOGGER.debug("Ending RequestHandler Thread for deployment: "+
                             depname);
            }
            catch(Exception e) {
                LOGGER.error(e.getMessage(),e);
            }
            finally{
                requests.remove(depname);
                safeClose(s);
                safeClose(cs);
            }
        }


        private void safeClose(Socket s) {
            try {
                s.close();
            }
            catch (Exception  e) {
                ;
            }
        }

        public void vmAlive(String eventName, int tHandle, Request req)
            throws Exception {

            // It might take 10 secs for oper data to arrive in oper tree
            // at the ESC side (buggy)

            NavuContainer ncsRoot = new NavuContainer(m, tHandle, Ncs.hash);

            boolean isNetsim =
                escEvent.container("event").leaf("details").exists();


            LOGGER.info("Device is netsim: " + isNetsim);

            LOGGER.info("serviceName: " + req.serviceName);


            // first check if domain still exists
            if (req.serviceName == null ||
                ncsRoot.getParent().
                container("http://cisco.com/yang/nso/vm-manager").
                container("vm-manager").
                list("start").elem(req.serviceName) == null) {
                LOGGER.info("vm-manager/start has been deleted while we "+
                            "were waiting, abort");
                return;
            }

            //TODO: Put this in a separate method
            String[] neededLeaves = new String[]{"svcname", "svcversion",
                                         "depname", "tenant", "vm_group"};

            Map<String, String> eventInfo = new HashMap<String, String>();
            for (String leaf : neededLeaves) {
                if (escEvent.leaf(leaf).exists()) {
                    eventInfo.put(leaf, escEvent.leaf(leaf).valueAsString());
                } else {
                    LOGGER.error(String.format("Required leaf %s not found",
                                               leaf));
                }
            }

            if (!isNetsim) {

                if (escEvent.leaf("vm_group").valueAsString().equals("CSR")) {
                    long timeout = 12000;
                    LOGGER.info("Sleeping "+timeout+
                                " millsecs waiting to ensure ESC opdata" +
                                " is in place");
                    Thread.sleep(timeout);
                }
            }

            NavuList d = ncsRoot.container("ncs", "devices").list("device");
            String devName = mkVmName(escEvent);

            if (d.elem(devName) != null) {
                LOGGER.info("Device "+ devName +
                            " already in tree, replaying?? or we're" +
                            "getting both VM_ALIVE and VM_RECOVERY_COMPLETE");
            }

            String mgmtIp;
            mgmtIp = null;

            if (isNetsim) {
                // netsim device, mount locally
                mgmtIp = "127.0.0.1";
                String targetOrSource = null;
                if (eventName.equals("VM_ALIVE")) {
                    targetOrSource = "vm_source";
                }
                else if (eventName.equals("VM_RECOVERY_COMPLETE")) {
                    targetOrSource = "vm_target";
                }
            } else {

                // IP adddress is allocated by ESC, and we must go down into
                // the stats tree to figure out which IP is the mgmt IP

                String targetOrSource = null;
                if (eventName.equals("VM_ALIVE")) {
                    targetOrSource = "vm_source";
                }
                else if (eventName.equals("VM_RECOVERY_COMPLETE")) {
                    targetOrSource = "vm_target";
                }

                String vmId = escEvent.container(targetOrSource).
                    leaf("vmid").valueAsString();
                String vmGroup = escEvent.leaf("vm_group").valueAsString();
                String tenant = escEvent.leaf("tenant").valueAsString();
                if ("SystemAdminTenantId".equals(tenant)) {
                    tenant = "admin";
                }

                String depName= escEvent.leaf("depname").valueAsString();
                String svcVersion = escEvent.leaf("svcversion").valueAsString();
                String svcName = escEvent.leaf("svcname").valueAsString();
                String depId = escEvent.leaf("depid").valueAsString();


                // At ESC side, they reload the whole oper db in one
                // chunk by delete, load, we may hit null pointers
                // while going down the live-status tree
                // we must retry a couple of times

                int attempts = 0;
                Exception escE = null;

                while (attempts++ < 5) {

                    try {
                        NavuList deps = d.elem(esc).container("live-status").
                            container("esc", "esc_datamodel").
                            container("opdata").
                            container("tenants").
                            list("tenant").
                            elem(tenant).
                            list("deployments");

                        // GO down into stats tree and search for
                        // mgmtIp, it's on the interface with nicid ==
                        // 0

                        ConfKey depKey = new ConfKey(
                            new ConfObject[] { new ConfBuf(depName),
                                               new ConfBuf(svcName),
                                               new ConfBuf(svcVersion)});
                        NavuContainer vmInst = deps.elem(depKey).
                            list("vm_group").
                            elem(vmGroup).
                            list("vm_instance").
                            elem(vmId);

                        if (vmInst == null) {
                            // We might get here on NC notif replay,
                            // we look at the old notifs, and they may
                            // refer to nonexisting VMs
                            return;
                        }

                        NavuContainer iface = vmInst.container("interfaces").
                            list("interface").
                            elem("0");

                        mgmtIp = iface.leaf("ip_address").valueAsString();
                        break;
                    }
                    catch ( Exception e ) {
                        LOGGER.error(
                            "Got error reading ESC oper tree, retrying in" +
                            " 5 secs", e);
                        Thread.sleep(5000);
                        escE = e;
                        continue;
                    }
                }
                if (mgmtIp == null) {
                    LOGGER.error(
                        "Failed 5 attempts to read the oper tree for " +
                        " the mgmtIP in ESC", escE);
                    throw escE;
                }
            }

            // create device in device tree

            NavuContainer vm = null;
            try {
                vm = d.create(devName);
            }
            catch (Exception e) {
                vm = d.elem(devName);
            }

            // TODO: Hard coded coordinates, need to retrieve connected devices
            //       and calculate a location in between
            double x = 0.62;
            double y = 0.458;

            try {
                m.safeCreate(tHandle,
                    "/webui:webui/data-stores/l3vpnui:static-map/device{%s}",
                    devName);
                m.setElem(tHandle, new ConfDecimal64(String.format(Locale.US,
                    "%1.18f", x)),
                    "/webui:webui/data-stores/l3vpnui:static-map/device{%s}/" +
                    "coord/x", devName);
                m.setElem(tHandle, new ConfDecimal64(String.format(Locale.US,
                    "%1.18f", y)),
                    "/webui:webui/data-stores/l3vpnui:static-map/device{%s}/" +
                    "coord/y", devName);
            } catch (Exception e) {
                LOGGER.error("Failed to create map icon for device", e);
            }

            if (isNetsim) {
                vm.leaf("address").set(mgmtIp);
                vm.leaf("port").set(getNetsimPort(escEvent));

                vm.leaf("authgroup").set("default");
                vm.container("ssh").leaf("host-key-verification").set("none");
                String nedId = getNedid(escEvent);

                NavuContainer cli = vm.container("device-type").
                    container("cli");

                cli.leaf("protocol").set("ssh");
                cli.leaf("ned-id").set(nedId);

                vm.container("state").
                    leaf("admin-state").set("unlocked");

            }
            // mount locally
            else {
                vm.leaf("address").set(mgmtIp);
                vm.leaf("authgroup").set("default");
                vm.container("ssh").leaf("host-key-verification").set("none");

                String nedId = getNedid(escEvent);
                NavuContainer cli = vm.container("device-type").
                    container("cli");

                cli.leaf("protocol").set("ssh");
                cli.leaf("ned-id").set(nedId);

                // FIXME: this should probably be stored in the
                // vm-manager/start request instead.
                if (nedId.equals("cisco-ios-cli-3.8")) {
                    vm.leaf("authgroup").set("csr");
                }
                vm.container("state").
                    leaf("admin-state").set("unlocked");

            }

            m.applyTrans(tHandle, false);
            LOGGER.info("Added dev " + devName + " to /devices");
            m.finishTrans(tHandle);
            addDeviceToList(req.serviceName, devName);
            setDeviceReadyFlag(devName, false);

            // device now in device tree, fetch ssh keys etc ..
            ConfXMLParam[] fetchSSHKeysParmsOut =  m.requestAction(
                new ConfXMLParam[] {},
                "/ncs:devices/device{%s}/ssh/fetch-host-keys",
                devName);
            LOGGER.info("Fetched ssh keys from " + devName);

            performSyncFrom(devName, m);

            LOGGER.info("Executed sync-from for " + devName);

            // Set Device Ready Flag to True
            setDeviceReadyFlag(devName, true);
            WebUILogger.log(LOGGER, "Device " + devName + " is ready");

            int vHandle = m.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);

            try {
                NavuContainer vmmRoot =
                    new NavuContainer(m, vHandle, vmManager.hash);

                NavuList vmmList =
                    vmmRoot.container("vm-manager").list("start");

                if (req.serviceName == null) {
                    LOGGER.error("no matching service found for event "+
                                 escEvent);
                    return;
                }

                for(NavuContainer serv:
                        vmmRoot.container("vm-manager").list("start").
                        elem(req.serviceName).list("allocators")) {
                    try {
                        ConfObjectRef oref =
                            (ConfObjectRef) serv.leaf("name").value();
                        String servPath =
                            new ConfPath(oref.getElems()).toString();

                        LOGGER.info(
                                    "dev " + devName + " added, redeploy: " +
                                    servPath);
                        m.requestAction(
                                    new ConfXMLParam[]{}, servPath +
                                    "/reactive-re-deploy");
                        LOGGER.info("Redeployed " + servPath);
                    } catch (Exception vex) {
                        LOGGER.error("", vex);
                    }
                }
            } finally {
                m.finishTrans(vHandle);
            }
        }

        private void setDeviceReadyFlag(String devName, boolean val)
            throws ConfException, IOException
        {
            CdbSession rsess = rcdb.startSession(CdbDBType.CDB_OPERATIONAL);
            try {
                rsess.setElem(new ConfBool(val),
                              new ConfPath(
                                  "/devices/device{"+devName+"}/vmm:ready"));
            } catch (Exception ex) {
                LOGGER.error("", ex);
            }
            finally {
                rsess.endSession();
            }
        }

        private void addDeviceToList(String serviceName, String devName)
            throws ConfException, IOException
        {
            LOGGER.info("adding to device list: "+devName);

            CdbSession rsess = rcdb.startSession(CdbDBType.CDB_OPERATIONAL);
            try {
                rsess.create(new ConfPath(
                                 "/vmm:vm-manager/start{"+serviceName+
                                 "}/device{"+devName+"}"));
            } catch (Exception ex) {
                LOGGER.error("", ex);
            }
            finally {
                rsess.endSession();
            }
        }

        private void performSyncFrom(String devName, Maapi maapiObj)
            throws IOException, ConfException, Exception {
            ConfXMLParam[] syncParamsOut =  maapiObj.requestAction(
                new ConfXMLParam[] {},
                "/ncs:devices/device{%s}/sync-from", devName);

            ConfXMLParamValue syncResult =
                (ConfXMLParamValue)syncParamsOut[0];

            ConfBool status = (ConfBool)syncResult.getValue();

            if ( !status.booleanValue() )
                throw new Exception (((ConfXMLParamValue)syncParamsOut[1])
                                     .getValue().toString());
            ConfObject val =
                (((ConfXMLParamValue)syncParamsOut[0])).getValue();
            LOGGER.info("SYNC RESULT "+val.toString());
            WebUILogger.log(LOGGER, "Sync-from device " + devName +
                            " response: " + val.toString());

            if(!val.toString().equals("true"))
                throw new Exception(" Sync returned false for device "+devName);
        }
    }

    private static void safeClose(Socket s) {
        try {
            s.close();
        }
        catch (Exception  e) {
            ;
        }
    }

    public static void updateVnfProgress(String vmId, String state) {

        Socket socket = null;
        Cdb cdb = null;
        CdbSession wsess = null;

        try {
            socket = new Socket(NcsMain.getInstance().getNcsHost(),
                                NcsMain.getInstance().getNcsPort());

            try {
                cdb = new Cdb("OperWriter", socket);
                wsess = cdb.startSession(CdbDBType.CDB_OPERATIONAL);

                String dPath = "/vmm:vm-manager/start{"+vmId+"}";

                String lPath = dPath+"/log{"+state+"}";

                if (!wsess.exists(lPath)) {
                    wsess.create(lPath);
                    String timestamp =
                        new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").
                        format(new Date());

                    wsess.setElem(new ConfBuf(timestamp),
                                  new ConfPath(lPath+"/timestamp"));
                }
            } finally {
                safeClose(socket);
            }
        } catch (Exception e) {
            LOGGER.error("", e);
        }
    }

}
