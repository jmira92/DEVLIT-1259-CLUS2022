package com.cisco.nso.ipaddressallocator;

import java.io.IOException;
import java.net.Inet6Address;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.nio.channels.ClosedChannelException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.cisco.nso.ipaddressallocator.namespaces.ipaddressAllocator;
import com.cisco.nso.ipam.Allocation;
import com.cisco.nso.ipam.IPAddressPool;
import com.cisco.nso.ipam.Subnet;
import com.cisco.nso.ipam.exceptions.AddressPoolException;
import com.cisco.nso.ipam.exceptions.InvalidNetmaskException;
import com.cisco.resourcemanager.namespaces.resourceAllocator;
import com.tailf.cdb.Cdb;
import com.tailf.cdb.CdbDBType;
import com.tailf.cdb.CdbDiffIterate;
import com.tailf.cdb.CdbSession;
import com.tailf.cdb.CdbSubscription;
import com.tailf.cdb.CdbSubscriptionSyncType;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfEnumeration;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfIPv4Prefix;
import com.tailf.conf.ConfIPv6Prefix;
import com.tailf.conf.ConfKey;
import com.tailf.conf.ConfObject;
import com.tailf.conf.ConfObjectRef;
import com.tailf.conf.ConfPath;
import com.tailf.conf.ConfUInt16;
import com.tailf.conf.ConfUInt8;
import com.tailf.conf.ConfValue;
import com.tailf.conf.ConfXMLParam;
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

public class IPAddressAllocator implements ApplicationComponent {
    private static Logger LOGGER = LogManager.getLogger(
            IPAddressAllocator.class);

    private CdbSubscription sub = null;
    private CdbSession wsess;

    private Set<Pool> pools = new HashSet<Pool>();

    public IPAddressAllocator() {
    }

    @Resource(type=ResourceType.CDB, scope=Scope.INSTANCE,
              qualifier="ip-address-allocator-subscriber")
    private Cdb cdb;

    @Resource(type=ResourceType.CDB, scope=Scope.INSTANCE,
              qualifier="ip-address-allocator-reactive-fm-loop")
    private Cdb wcdb;

    @Resource(type=ResourceType.MAAPI, scope=Scope.INSTANCE,
              qualifier="reactive-fm-ipaddressallocator-m")
    private Maapi maapi;

    private int tid;
    private int alloc_subid, subnet_subid, pool_subid, exclude_subid;

    private NavuList ipaddresspool;

    private boolean isMaster = true;

    public void init() {
        try {
            wsess = wcdb.startSession(CdbDBType.CDB_OPERATIONAL);
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

            tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);

            sub = cdb.newSubscription();

            // create subscriptions
            alloc_subid = sub.subscribe(
                     3, new resourceAllocator(),
                     "/"+
                     resourceAllocator.prefix+":"+
                     resourceAllocator._resource_pools_+"/"+
                     resourceAllocator._ip_address_pool_+"/"+
                     resourceAllocator._allocation_);

            subnet_subid = sub.subscribe(
                     2, new resourceAllocator(),
                     "/"+
                     resourceAllocator.prefix+":"+
                     resourceAllocator._resource_pools_+"/"+
                     resourceAllocator._ip_address_pool_+"/"+
                     ipaddressAllocator._subnet_);

            exclude_subid = sub.subscribe(
                     2, new resourceAllocator(),
                     "/"+
                     resourceAllocator.prefix+":"+
                     resourceAllocator._resource_pools_+"/"+
                     resourceAllocator._ip_address_pool_+"/"+
                     ipaddressAllocator._exclude_);

            pool_subid = sub.subscribe(
                     1, new resourceAllocator(),
                     "/"+
                     resourceAllocator.prefix+":"+
                     resourceAllocator._resource_pools_+"/"+
                     resourceAllocator._ip_address_pool_);

            // tell CDB we are ready for notifications
            sub.subscribeDone();

            loadState();

        }
        catch (Throwable e) {
            LOGGER.error("", e);
        }
    }

    private void loadState() throws NavuException, ConfException, IOException,
                                    UnknownHostException, AddressPoolException,
                                    InvalidNetmaskException {

        pools = new HashSet<Pool>();

        // ************************************************************
        // read existing config and create existing pools
        //
        NavuContext context = new NavuContext(maapi, tid);
        NavuContainer base = new NavuContainer(context);
        NavuContainer root = base.container(resourceAllocator.hash);
        NavuContainer resources = root.namespace(resourceAllocator.id)
                .container(resourceAllocator._resource_pools_);
        ipaddresspool = resources.list(resourceAllocator._ip_address_pool_);

        // Create IP address pools
        for(NavuContainer pool: ipaddresspool.elements()) {
            createPool(pool, true);
        }
    }

    public void run() {
        while(true) {
            int[] points;
            try {
                points = sub.read();
            } catch (Exception e) {
                if (e.getCause() instanceof java.io.EOFException) {
                    // silence here, normal close (redeploy/reload package)
                    ;
                }
                else {
                    LOGGER.error("",e );
                }
                return;
            }
            try {
                boolean ha_mode_exists =
                    maapi.exists(tid, "/tfnm:ncs-state/ha");

                if (ha_mode_exists) {
                    ConfEnumeration ha_mode_enum =  (ConfEnumeration)
                        maapi.getElem(tid, "/tfnm:ncs-state/ha/mode");

                    String ha_mode =
                        ConfEnumeration.getLabelByEnum(
                               "/tfnm:ncs-state/ha/mode",
                               ha_mode_enum);

                    if (!("none".equals(ha_mode) ||
                          "master".equals(ha_mode) ||
                          "normal".equals(ha_mode))) {
                        // slave or relay-slave
                        sub.sync(CdbSubscriptionSyncType.DONE_PRIORITY);
                        isMaster = false;
                        continue;
                    }
                    else {
                        if (isMaster == false) {
                            // we just became master, we need to re-read
                            // our state
                            loadState();
                            isMaster = true;
                        }
                    }
                }
            } catch (IOException e) {
                LOGGER.error("",e );
            } catch (ConfException e) {
                LOGGER.error("",e );
            } catch (AddressPoolException e) {
                LOGGER.error("",e );
            } catch (InvalidNetmaskException e) {
                LOGGER.error("",e );
            }
            try {
                ArrayList<Request> reqs = new ArrayList<Request>();
                EnumSet<DiffIterateFlags> enumSet =
                    EnumSet.<DiffIterateFlags>of(
                                    DiffIterateFlags.ITER_WANT_PREV,
                                    DiffIterateFlags.ITER_WANT_SCHEMA_ORDER);

                // process each subscription point
                for(int i=0 ; i < points.length ; i++) {
                    if (points[i] == alloc_subid) {
                        try {
                            sub.diffIterate(points[i],
                                            new Iter(sub, Type.ALLOC),
                                            enumSet, reqs);
                        }
                        catch (Exception e) {
                            LOGGER.error("", e);
                            reqs = null;
                        }
                    }
                    else if (points[i] == pool_subid) {
                        try {
                            sub.diffIterate(points[i],
                                            new Iter(sub, Type.POOL),
                                            enumSet, reqs);
                        }
                        catch (Exception e) {
                            LOGGER.error("", e);
                            reqs = null;
                        }
                    }
                    else if (points[i] == subnet_subid) {
                        try {
                            sub.diffIterate(points[i],
                                            new Iter(sub, Type.SUBNET),
                                            enumSet, reqs);
                        }
                        catch (Exception e) {
                            LOGGER.error("", e);
                            reqs = null;
                        }
                    }
                    else if (points[i] == exclude_subid) {
                        try {
                            sub.diffIterate(points[i],
                                            new Iter(sub, Type.EXCLUDE),
                                            enumSet, reqs);
                        }
                        catch (Exception e) {
                            LOGGER.error("", e);
                            reqs = null;
                        }
                    }
                }

                // It's important that we return as early as possible her,
                // This is a common technique, gather the work to do, tell
                // CDB that we're done and then do the work.
                // It could be that the allocator needs to reach out (RPC)
                // and that can be slow

                // If we are calling an external allocator we should do
                // the following call here and not after the for loop
                // sub.sync(CdbSubscriptionSyncType.DONE_PRIORITY);

                ArrayList<String> redeps = new ArrayList<String>();

                // System.err.println("reqs.length="+reqs.size());

                // first do all subnet deletes, then creates


                ArrayList<Request> reallocReqs = new ArrayList<Request>();

                // order to do process operations
                //
                // 1. process all subnet delete events
                // 2. process all pool modification events
                // 3. process all allocatoin release events
                // 4. retry previously failed allocations if there
                // 5. process all new allocations
                //

                for (Request req : reqs) {
                    if (req.t != Type.SUBNET || req.op != Operation.DELETE)
                        continue;

                    // We got subnet delete, process first

                    // Find proper pool
                    Pool p = null;

                    for(Pool pool: pools) {
                        if (pool.p.getName().equals(req.pool.elementAt(0).
                                                    toString())) {
                            p = pool;
                            break;
                        }
                    }

                    if (p == null) {
                        LOGGER.error("No matching pool found: "+
                                     req.pool.elementAt(0).toString());
                        System.err.println("No matching pool found!");
                        continue;
                    }

                    NavuList cdbSubnet =
                        ((NavuContainer) p.availables.cdbAvailables.
                         getParent()).list(ipaddressAllocator._subnet_);
                    System.err.println("req.key="+req.key);
                    String address = req.key.elementAt(0).toString();
                    String mask = req.key.elementAt(1).toString();
                    int imask =
                        (int) ((ConfUInt16) req.key.elementAt(1)).
                        longValue();
                    String[] key = new String[] {address, mask};
                    Subnet sub = new Subnet(address, imask);

                    // A subnet has been removed from the pool
                    if (cdbSubnet.elem(key) != null) {

                        reallocateSubnets(p, cdbSubnet, reallocReqs, sub);

                        /* first restore any excludes */
                        for(Subnet esub: p.excludes) {
                            if (sub.contains(esub)) {
                                p.p.addToAvailable(esub);
                            }
                            else if (esub.contains(sub)) {
                                p.p.addToAvailable(sub);
                            }
                        }

                        // now all subnets should have been released
                        // and we can remove the subnet

                        cdbSubnet.delete(key);
                        p.subnets.remove(sub);

                        try {
                            p.p.removeFromAvailable(sub);
                        } catch (Exception e) {
                            // ignore for now
                            LOGGER.error("",e );
                        }

                    } else {
                        System.err.println("already removed subnet: "+sub);
                    }

                    if (p.allocations.cdbAllocations != null)
                        p.allocations.cdbAllocations.stopCdbSession();

                    if (p.availables.cdbAvailables != null)
                        p.availables.cdbAvailables.stopCdbSession();

                    ipaddresspool.reset();
                }

                for (Request req : reqs) {
                    // Find proper pool

                    if (req.t == Type.SUBNET && req.op == Operation.DELETE)
                        continue;

                    Pool p = null;

                    for(Pool pool: pools) {
                        if (pool.p.getName().equals(req.pool.elementAt(0).
                                                    toString())) {
                            p = pool;
                            break;
                        }
                    }

                    if (p == null &&
                        !(req.t == Type.POOL && req.op == Operation.CREATE)) {
                        continue;
                    }

                    if (req.t == Type.POOL) {
                        if (req.op == Operation.CREATE) {
                            // A new pool has been added
                            try {
                                createPool(ipaddresspool.elem(req.pool), true);
                            }
                            catch (Exception e) {
                                LOGGER.error("Failed to create pool", e);
                            }
                        }
                        else {
                            // An existing pool has been removed, cleanup
                            try {
                                pools.remove(p);
                                // Delete CDB oper structures for pool
                                NavuContainer myPool =
                                    ((NavuContainer) p.availables.
                                     cdbAvailables.getParent());
                                ((NavuList) myPool.getParent()).
                                    delete(myPool.getKey());
                            }
                            catch (Exception e) {
                                LOGGER.error("Failed to delete pool", e);
                            }
                        }
                    } else if (req.t == Type.SUBNET) {
                        NavuList cdbSubnet =
                            ((NavuContainer) p.availables.cdbAvailables.
                             getParent()).list(ipaddressAllocator._subnet_);
                        // System.err.println("req.key="+req.key);
                        String address = req.key.elementAt(0).toString();
                        String mask = req.key.elementAt(1).toString();
                        int imask =
                            (int) ((ConfUInt16) req.key.elementAt(1)).
                            longValue();
                        String[] key = new String[] {address, mask};
                        Subnet sub = new Subnet(address, imask);

                        if (req.op == Operation.CREATE) {
                            // A subnet has been added to the pool
                            // Check if it was already created by the pool
                            // create
                            if (cdbSubnet.elem(key) == null) {
                                p.p.addToAvailable(sub);
                                cdbSubnet.create(key);
                                p.subnets.add(sub);

                                /* check all excludes */
                                for(Subnet esub: p.excludes) {
                                    if (sub.contains(esub)) {
                                        p.p.removeFromAvailable(esub);
                                    }
                                }
                            }
                        }
                        else {
                            // already taken care of above
                            continue;
                        }
                    }
                    else if (req.t == Type.EXCLUDE) {
                        // System.err.println("Exclude "+req.key);
                        NavuList cdbExclude =
                            ((NavuContainer) p.availables.cdbAvailables.
                             getParent()).list(ipaddressAllocator._exclude_);
                        // System.err.println("req.key="+req.key);
                        String address = req.key.elementAt(0).toString();
                        String mask = req.key.elementAt(1).toString();
                        int imask =
                            (int) ((ConfUInt16) req.key.elementAt(1)).
                            longValue();
                        String[] key = new String[] {address, mask};
                        Subnet sub = new Subnet(address, imask);

                        if (req.op == Operation.DELETE) {
                            // A exclude has been added to the pool
                            if (cdbExclude.elem(key) != null) {
                                for (Subnet osub: p.subnets) {
                                    if (osub.contains(sub))
                                        p.p.addToAvailable(sub);
                                    else if(sub.contains(osub)) {
                                        p.p.addToAvailable(osub);
                                    }
                                }
                                cdbExclude.delete(key);
                                p.excludes.remove(sub);
                            }
                            else {
                              System.err.println("already removed exclude: "
                                                 +sub);
                            }
                        }
                        else {
                            // An exclude has been added
                            // Check if it was already created by the pool
                            // create
                            if (cdbExclude.elem(key) == null) {

                                reallocateSubnets(p, cdbExclude, reallocReqs,
                                                  sub);

                                // now all subnets should have been released
                                // and we can remove the subnet

                                cdbExclude.create(key);
                                p.excludes.add(sub);

                                for (Subnet osub: p.subnets) {
                                    if (osub.contains(sub)) {
                                        try {
                                            p.p.removeFromAvailable(sub);
                                        } catch (Exception e) {
                                            // ignore for now
                                            LOGGER.error("",e );
                                        }
                                    }
                                    else if (sub.contains(osub)) {
                                        try {
                                            p.p.removeFromAvailable(sub);
                                        } catch (Exception e) {
                                            // ignore for now
                                            LOGGER.error("",e );
                                        }
                                    }
                                }
                            }
                        }
                    }
                    else if (req.t == Type.ALLOC) {
                        if (req.op == Operation.CREATE) {
                            continue;
                        }
                        else {
                            // delete
                            // clean up oper data, and de-allocate
                            try {
                                try {
                                    wsess.delete(
                                        req.path + "/"+resourceAllocator.
                                        _response_+"/"+resourceAllocator.
                                        _subnet_);
                                }
                                catch (Exception e) {
                                    ;
                                    // LOGGER.error("", e);
                                }
                                try {
                                    wsess.delete(
                                        req.path + "/"+resourceAllocator.
                                        _response_+"/"+resourceAllocator.
                                        _error_);
                                }
                                catch (Exception e) {
                                    ;
                                    // LOGGER.error("", e);
                                }

                                if (req.val != null) {
                                    Subnet sub = new Subnet(req.val.toString());
                                    p.p.release(sub.getAddress());
                                }
                            }
                            // No we didn't
                            catch (Exception e) {
                                LOGGER.error("", e);
                            }
                        }
                    }

                    if (p != null) {
                        if (p.allocations.cdbAllocations != null)
                            p.allocations.cdbAllocations.stopCdbSession();

                        if (p.availables.cdbAvailables != null)
                            p.availables.cdbAvailables.stopCdbSession();
                    }
                }

                for(Request reallocReq: reallocReqs)
                    allocateAddress(redeps, reallocReq);

                reallocReqs = new ArrayList<Request>();

                // finally do all new allocations

                Pool p = null;

                for (Request req : reqs) {

                    if (req.t == Type.POOL)
                        continue;
                    else if (req.t == Type.SUBNET)
                        continue;
                    else if (req.t == Type.EXCLUDE)
                        continue;
                    else if (req.t == Type.ALLOC) {
                        // Find proper pool

                        if (req.op == Operation.CREATE) {
                            for(Pool pool: pools) {
                                if (pool.p.getName().equals(req.pool.elementAt(0).
                                                            toString())) {
                                    p = pool;
                                    break;
                                }
                            }

                            if (p == null)  {
                                continue;
                            }

                            req.p = p;
                            allocateAddress(redeps, req);
                        }
                        else
                            continue;
                    }

                    if (p != null) {
                        if (p.allocations.cdbAllocations != null)
                            p.allocations.cdbAllocations.stopCdbSession();

                        if (p.availables.cdbAvailables != null)
                            p.availables.cdbAvailables.stopCdbSession();
                    }
                }

                // invoke redeploy
                for (String rep : redeps) {
                    System.err.println("Redeploying: "+rep);
                    redeploy(rep);
                }

            }
            catch (ConfException e) {
                if (e.getCause() instanceof java.io.EOFException) {
                    // silence here, normal close (redeploy/reload package)
                    LOGGER.error("",e );
                    ;
                }
                else {
                    LOGGER.error("",e );
                }
            }
            catch (SocketException e) {
                // silence here, normal close (redeploy/reload package)
                LOGGER.error("",e );
                ;
            }
            catch (ClosedChannelException e) {
                // silence here, normal close (redeploy/reload package)
                LOGGER.error("",e );
                ;
            }
            catch (Exception e) {
                LOGGER.error("",e );
            }
            catch (Throwable e) {
                LOGGER.error("", e);
            }

            // It's important that we return as early as possible her,
            // This is a common technique, gather the work to do, tell
            // CDB that we're done and then do the work.
            // It could be that the allocator needs to reach out (RPC)
            // and that can be slow

            // NOTE: if you are calling an external algorithm you must
            // do the below call earlier
            try {
                sub.sync(CdbSubscriptionSyncType.DONE_PRIORITY);
            } catch (Exception e) {
                LOGGER.error("",e );
            }
        }
    }

    private void allocateAddress(ArrayList<String> redeps,
                                 Request req)
        throws Exception, NavuException {
        Pool p = req.p;
        String owner = "";

        if (maapi.exists(tid, req.path+"/"+
                         resourceAllocator.
                         _allocating_service_)) {
            ConfObjectRef v =
                (ConfObjectRef) maapi.
                getElem(tid, req.path+"/"+
                        resourceAllocator.
                        _allocating_service_);

            owner = new ConfPath(v.getElems()).toString();
        }

        int cidr = (int) ((ConfUInt8)
                          maapi.
                          getElem(tid, req.path+"/"+
                                  resourceAllocator.
                                  _request_+
                                  "/"+
                                  resourceAllocator.
                                  _subnet_size_)).
            longValue();

        try {
            Allocation a = p.p.allocate(cidr, owner);

            // Write the result and redeploy
            Subnet net = a.getAllocated();

            Subnet fromNet = null;

            for(Subnet sub: p.subnets) {
                if (sub.contains(net)) {
                    fromNet = sub;
                    break;
                }
            }

            // System.out.println("SET: "+ req.path+"/"+
            //                    resourceAllocator.
            //                    _response_+"/"+
            //                    resourceAllocator.
            //                    _subnet_+" -> " + net +
            //                    " from " +
            //                    fromNet);

            if (a.getAllocated().getAddress() instanceof Inet6Address) {
                wsess.setElem(new ConfIPv6Prefix(net.toString()),
                              req.path+"/"+
                              resourceAllocator._response_+"/"+
                              resourceAllocator._subnet_);
            }
            else {
                wsess.setElem(new ConfIPv4Prefix(net.toString()),
                              req.path+"/"+
                              resourceAllocator._response_+"/"+
                              resourceAllocator._subnet_);
            }

            if (fromNet != null &&
                fromNet.getAddress() instanceof Inet6Address) {
                wsess.setElem(new ConfIPv6Prefix(fromNet.toString()),
                              req.path+"/"+
                              resourceAllocator._response_+"/"+
                              resourceAllocator._from_);
            }
            else if (fromNet != null) {
                wsess.setElem(new ConfIPv4Prefix(fromNet.toString()),
                              req.path+"/"+
                              resourceAllocator._response_+"/"+
                              resourceAllocator._from_);
            }
            /* we need to setCase after setElem due to a bug in NCS */
            wsess.setCase(resourceAllocator.
                          _response_choice_,
                          resourceAllocator._ok_,
                          req.path+"/"+
                          resourceAllocator._response_);
        }
        catch (AddressPoolException ex) {
            // System.out.println("SET: " + req.path +
            //                    "/response/error ->" +
            //                    ex);
            wsess.setElem(new ConfBuf(ex.toString()),
                          req.path + "/"+
                          resourceAllocator._response_+"/"+
                          resourceAllocator._error_);
            /* we need to setCase after setElem due to a bug in NCS */
            wsess.setCase(resourceAllocator.
                          _response_choice_,
                          resourceAllocator._error_,
                          req.path+"/"+
                          resourceAllocator._response_);
        }

        if (owner != "") {
            // redeploy the service that consumes this
            // data, runs in separate thread FIXME:
            // rewrite to only redeploy each service
            // once if it allocates multiple addresses

            if (!redeps.contains(owner)) {
                // System.err.println("adding "+owner+" to redeploy list");
                redeps.add(owner);
            }
        }
    }


    private void reallocateSubnets(Pool p, NavuList cdbSubnet,
                                   ArrayList<Request> reallocReqs, Subnet sub)
        throws Exception {
        // Loop over all allocations in the pool and reallocate all that
        // belong to this the subnet sub.

        LOGGER.info("reallocSubnets: "+sub);

        NavuContext context = new NavuContext(maapi, tid);
        NavuContainer base = new NavuContainer(context);
        NavuContainer root = base.container(resourceAllocator.hash);
        NavuContainer resources = root.namespace(resourceAllocator.id)
                .container(resourceAllocator._resource_pools_);
        NavuContainer ipaddressPool =
            resources.list(resourceAllocator._ip_address_pool_).
            elem(p.p.getName());
        NavuList allocations = ipaddressPool.list(resourceAllocator.
                                                  _allocation_);

        for(NavuContainer alloc : allocations.elements()) {

            NavuContainer cdbAlloc =
                (NavuContainer) cdbSubnet.
                getNavuNode(new ConfPath(alloc.getKeyPath()));

            // LOGGER.info("inspecting alloc: "+alloc.getKeyPath());

            NavuContainer response =
                cdbAlloc.container(resourceAllocator._response_);
            if (response.getSelectedCase(resourceAllocator._response_choice_).
                getTag().equals(resourceAllocator._ok_) &&
                response.leaf(resourceAllocator._subnet_).value() != null) {

                Subnet subnet =
                    new Subnet(response.leaf(resourceAllocator._subnet_).
                               value().toString());

                if (sub.contains(subnet) || subnet.contains(subnet)) {
                    // needs to be reallocated
                    int cidr = subnet.getCIDRMask();
                    response.leaf(resourceAllocator._subnet_).delete();
                    p.p.release(subnet.getAddress());
                    Request r = new Request();
                    r.path = new ConfPath(alloc.getKeyPath());
                    r.p = p;
                    // LOGGER.info("adding alloc: "+r);
                    reallocReqs.add(r);
                }
            }
        }
        // LOGGER.info("done reallocSubnets: "+sub);
    }

    public void finish() {
        try {
            for(Pool p: pools) {
                if (p.allocations.cdbAllocations != null)
                    p.allocations.cdbAllocations.context().finishClearTrans();

                if (p.availables.cdbAvailables != null)
                    p.availables.cdbAvailables.context().finishClearTrans();
            }
            wsess.endSession();
        }
        catch (ClosedChannelException e) {
            // silence here, normal close (redeploy/reload package)
            ;
        }
        catch (Exception e) {
            LOGGER.error("",e );
        }
        try {
            try {
                maapi.finishTrans(tid);
            }
            catch (Throwable ignore) {}
            ResourceManager.unregisterResources(this);
        }
        catch (Exception e) {
            LOGGER.error("",e );
        }
    }

    private void createPool(NavuContainer pool, boolean loadAll)
        throws NavuException, ConfException, IOException,
               UnknownHostException, AddressPoolException,
               InvalidNetmaskException {
        AllocationsSet allocations;
        AvailablesSet availables;
        Set<Subnet> subnets = new HashSet<Subnet>();
        Set<Subnet> excludes = new HashSet<Subnet>();

        String pname = pool.leaf("name").value().toString();

        NavuContext availContext = new NavuContext(maapi);
        availContext.startOperationalTrans(Conf.MODE_READ_WRITE);
        availables = new AvailablesSet(availContext, pname,
                                       new SubnetComparator());
        availContext.applyReplaceTrans();

        NavuContext allocContext = new NavuContext(maapi);
        allocContext.startOperationalTrans(Conf.MODE_READ_WRITE);
        allocations = new AllocationsSet(allocContext, pname);
        allocContext.applyReplaceTrans();

        // System.err.println("creating new pool");
        IPAddressPool p = new IPAddressPool(pname,
                                            availables,
                                            allocations,
                                            subnets);

        // compare configured subnets to known subnets and add/remove
        NavuList cdbSubnet = ((NavuContainer) availables.cdbAvailables.
                              getParent()).list(ipaddressAllocator._subnet_);

        NavuList poolSubnet = pool.namespace(ipaddressAllocator.id)
                .list(ipaddressAllocator._subnet_);

        if (loadAll) {
            // first add those that are new
            // System.err.println("Add subnets");
            for(NavuContainer subnet: poolSubnet.elements()) {
                String address = subnet.leaf(ipaddressAllocator._address_).
                    value().toString();
                String mask = subnet.leaf(ipaddressAllocator._cidrmask_).
                    value().toString();

                int imask =
                    (int) ((ConfUInt16) subnet.
                           leaf(ipaddressAllocator._cidrmask_).
                           value()).longValue();
                Subnet snet = new Subnet(address, imask);
                subnets.add(snet);

                if (cdbSubnet.elem(new String[] {address,mask}) == null) {
                    // System.err.println("add to available: "+snet);
                    p.addToAvailable(snet);
                    cdbSubnet.create(new String[] {address,mask});
                }
            }

            // System.err.println("Remove subnets");

            // then remove those that have been removed
            for(NavuContainer subnet: cdbSubnet.elements()) {
                String address = subnet.leaf(ipaddressAllocator._address_).
                    value().toString();
                String mask = subnet.leaf(ipaddressAllocator._cidrmask_).
                    value().toString();

                if (poolSubnet.elem(new String[] {address,mask}) == null) {
                    int imask = (int) ((ConfUInt16) subnet.
                                       leaf(ipaddressAllocator._cidrmask_).
                                       value()).longValue();
                    try {
                        p.removeFromAvailable(new Subnet(address, imask));
                    } catch (Exception e) {
                        LOGGER.error("",e );
                    }
                    cdbSubnet.delete(new String[] {address,mask});
                }
            }

            // System.err.println("Add exclusions");
            // compare configured excludess to known excludes and add/remove
            NavuList cdbExclude = ((NavuContainer) availables.cdbAvailables.
                                getParent()).list(ipaddressAllocator._exclude_);

            NavuList poolExclude = pool.namespace(ipaddressAllocator.id)
                    .list(ipaddressAllocator._exclude_);

            // first add those that are new
            for(NavuContainer subnet: poolExclude.elements()) {
                String address = subnet.leaf(ipaddressAllocator._address_).
                    value().toString();
                String mask = subnet.leaf(ipaddressAllocator._cidrmask_).
                    value().toString();

                int imask =
                    (int) ((ConfUInt16) subnet.
                           leaf(ipaddressAllocator._cidrmask_).
                           value()).longValue();
                Subnet snet = new Subnet(address, imask);
                excludes.add(snet);

                if (cdbExclude.elem(new String[] {address,mask}) == null) {

                    for (Subnet osub: subnets) {
                        if (osub.contains(snet)) {
                            try {
                                p.removeFromAvailable(snet);
                            } catch (Exception e) {
                                LOGGER.error("",e );
                            }
                        }
                        else if (snet.contains(osub)) {
                            try {
                                p.removeFromAvailable(osub);
                            } catch (Exception e) {
                                LOGGER.error("",e );
                            }
                        }
                    }
                    cdbExclude.create(new String[] {address,mask});
                }
            }

            // System.err.println("Remove exclustions");

            // then remove those that have been removed
            for(NavuContainer subnet: cdbExclude.elements()) {
                String address = subnet.leaf(ipaddressAllocator._address_).
                    value().toString();
                String mask = subnet.leaf(ipaddressAllocator._cidrmask_).
                    value().toString();

                if (poolExclude.elem(new String[] {address,mask}) == null) {
                    int imask = (int) ((ConfUInt16) subnet.
                                       leaf(ipaddressAllocator._cidrmask_).
                                       value()).longValue();
                    Subnet snet = new Subnet(address, imask);
                    for(Subnet osub: subnets) {
                        if (osub.contains(snet)) {
                            try {
                               // System.err.println("add to available: "+snet);
                                p.addToAvailable(snet);
                            } catch (Exception e) {
                                LOGGER.error("",e );
                            }
                        }
                        else if (snet.contains(osub)) {
                            try {
                               // System.err.println("add to available: "+snet);
                                p.addToAvailable(osub);
                            } catch (Exception e) {
                                LOGGER.error("",e );
                            }
                        }
                    }
                    cdbExclude.delete(new String[] {address,mask});
                }
            }
        }

        Pool po = new Pool();
        po.p = p;
        po.availables = availables;
        po.allocations = allocations;
        po.subnets = subnets;
        po.excludes = excludes;

        pools.add(po);
        // System.err.println("done");

    }

    private void safeclose(Cdb s) {
        try {s.close();}

        catch (Exception ignore) {}
    }

    private class Pool {
        IPAddressPool p;
        AvailablesSet availables;
        AllocationsSet allocations;
        Set<Subnet> subnets;
        Set<Subnet> excludes;
    }

    private enum Operation { CREATE, DELETE}
    private enum Type { ALLOC, SUBNET, EXCLUDE, POOL }

    private class Request {
        Pool p;
        ConfKey pool;
        ConfKey key;
        Operation op;
        Type t;
        ConfPath path;
        ConfValue val;

        public String toString() {
            return "Request: "+op+" "+t+" "+path+" key:"+key+" pool:"+pool;
        }
    }

    private class Iter implements CdbDiffIterate {
        CdbSubscription cdbSub;
        Type itype;

        Iter(CdbSubscription sub, Type itype) {
            this.cdbSub = sub;
            this.itype = itype;
        }

        public DiffIterateResultFlag iterate(
            ConfObject[] kp,
            DiffIterateOperFlag op,
            ConfObject oldValue,
            ConfObject newValue, Object initstate) {

            @SuppressWarnings("unchecked")
            ArrayList<Request> reqs = (ArrayList<Request>) initstate;

            try {
                ConfPath p = new ConfPath(kp);
                // System.out.println(itype+":ITER " + op + " " + p);

                if (itype == Type.POOL && kp.length > 3)
                    return DiffIterateResultFlag.ITER_RECURSE;

                Request r = new Request();

                r.path = p;
                r.pool = (ConfKey) kp[kp.length-3];
                r.val = null;

                // System.err.println("kp="+kp);
                // System.err.println("kp.length="+kp.length);

                if (kp.length >= 5)
                    r.key = (ConfKey) kp[kp.length-5];
                else
                    r.key = null;

                if ((op == DiffIterateOperFlag.MOP_CREATED)) {
                    r.op = Operation.CREATE;
                    r.t = itype;
                    reqs.add(r);
                }
                else if ((op == DiffIterateOperFlag.MOP_DELETED)) {
                    r.op = Operation.DELETE;

                    if (kp.length >= 5)
                        r.t = itype;
                    else
                        r.t = Type.POOL;

                    if (r.t == Type.ALLOC) {
                        ConfValue v =
                            wsess.getElem(r.path+"/"+
                                          resourceAllocator.
                                          _response_+"/"
                                          +resourceAllocator._subnet_);
                        r.val = v;
                    }

                    reqs.add(r);
                }
                else {
                    // ignore VALUE_SET etc
                }
            }
            catch (Exception e) {
                LOGGER.error("", e);
            }
            return DiffIterateResultFlag.ITER_RECURSE;
        }
    }

    // redeploy MUST be done in another thread, if not system
    // hangs, since the CDB subscriber cannot do its work
    private void redeploy(String path) {
        Redeployer r = new Redeployer(path);
        Thread t = new Thread(r);
        t.start();
    }



    private class Redeployer implements Runnable {
        private String path;
        private ConfKey k;
        private Maapi m;
        private Socket s;

        public Redeployer(String path) {
            this.path = path; this.k = k;

        }

        public void run() {

            try {
                s = new Socket(NcsMain.getInstance().getNcsHost(),
                               NcsMain.getInstance().getNcsPort());
                m = new Maapi(s);

                m.startUserSession("admin",
                                   m.getSocket().getInetAddress(),
                                   "system",
                                   new String[] {"admin"},
                                   MaapiUserSessionFlag.PROTO_TCP);
            } catch (Exception e) {
                LOGGER.error("", e);
                // System.err.println("redeployer exception: "+e);
                return;
            }



            try {
                // must be different, we want to redeploy owner if
                // he exists

                int tid = m.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
                // System.err.println("invoking redeploy on "+path);

                int counter = 0;
                while (true) {
                  Thread.sleep(50);
                  if (m.exists(tid, path))
                    break;
                  if (counter++ == 40) {
                    break;
                  }
                  Thread.sleep(1000);
                }

                m.requestAction(new ConfXMLParam[] {},
                                path+"/reactive-re-deploy");

                try {
                    m.finishTrans(tid);
                }
                catch (Throwable ignore) {
                }
                finally {
                  s.close();
                }
            } catch (Throwable e) {
                LOGGER.error("Failed to redeploy: "+path, e);
            }
        }
    }

    private static class SubnetComparator implements Comparator<Subnet> {
        public int compare(Subnet o1, Subnet o2) {
            //
            //      Order first by mask
            //
            if (o1.getCIDRMask() == o2.getCIDRMask()) {
                //
                //      Order by address next
                //
                byte[] bo1 = o1.getAddress().getAddress();
                byte[] bo2 = o2.getAddress().getAddress();
                for (int i = 0; i < bo1.length; i++) {
                    if (bo1[i] != bo2[i]) {
                        return (bo1[i] & 0xff) - (bo2[i] & 0xff);
                    }
                }
            }
            //
            //      Order subnets from narrowest to widest
            //
            return o2.getCIDRMask() - o1.getCIDRMask();
        }
    }
}
