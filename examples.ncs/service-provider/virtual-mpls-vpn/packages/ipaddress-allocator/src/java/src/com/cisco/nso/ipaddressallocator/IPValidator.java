package com.cisco.nso.ipaddressallocator;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.cisco.nso.ipaddressallocator.namespaces.*;
import com.cisco.resourcemanager.namespaces.*;

import com.tailf.conf.*;
import com.tailf.ncs.*;
import com.tailf.ncs.annotations.*;
import com.tailf.maapi.*;
import com.tailf.dp.*;
import com.tailf.dp.annotations.*;
import com.tailf.dp.proto.*;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.net.SocketAddress;
import com.tailf.conf.*;
import java.util.Hashtable;
import java.util.Vector;

import com.cisco.nso.ipam.*;
import com.cisco.nso.ipam.exceptions.*;

public class IPValidator {
    private static Logger LOGGER = LogManager.getLogger(IPValidator.class);

    private Hashtable<ConfKey,Vector<Entry>> cache;

    private class Entry {
        Subnet s;
        ConfKey k;
    }

    // Transaction handle
    private int th;

    @Resource(type=ResourceType.MAAPI, scope=Scope.INSTANCE,
              qualifier="reactive-fm-ipaddressallocator-validator-m")
    private Maapi maapi;

    @TransValidateCallback(callType=TransValidateCBType.INIT)
    public void init(DpTrans trans) throws DpCallbackException {
        // attach to the transaction
        try {
            maapi.startUserSession("admin",
                                   maapi.getSocket().getInetAddress(),
                                   "system",
                                   new String[] {"admin"},
                                   MaapiUserSessionFlag.PROTO_TCP);

            int th = trans.getTransaction();
            maapi.attach(th, 0,
                         trans.getUserInfo().getUserId());

            cache = new Hashtable<ConfKey,Vector<Entry>>();
            LOGGER.info("init");
        } catch (Exception e) { // IOException, MaapiException
            throw new DpCallbackException("failed to attach via maapi: "+
                                          e.getMessage());
        }
    }

    @TransValidateCallback(callType = { TransValidateCBType.STOP })
    public void stop(DpTrans trans) throws DpCallbackException {
        try {
            LOGGER.info("stop");
            maapi.detach(trans.getTransaction());
        } catch (Exception e) { // IOException, MaapiException
            /* never mind */
        }
    }

    void buildCache(ConfKey pool) {
        MaapiCursor mc;
        ConfKey idx;
        Vector<Entry> subnet = new Vector<Entry>();

        LOGGER.debug("buildCache for pool " + pool);

        try {
            mc = maapi.newCursor(th,
                                 "/resource-pools/ip-address-pool{%x}/subnet",
                                 pool);

            while ( (idx = maapi.getNext(mc)) != null) {
                String addr = idx.elementAt(0).toString();
                String mask = idx.elementAt(1).toString();
                Subnet sub = new Subnet(addr, mask);
                LOGGER.info("Found " + idx + " subnet: " + sub);
                Entry e = new Entry();
                e.s = sub;
                e.k = idx;
                subnet.add(e);
            }

        } catch (Exception e) {
            LOGGER.error("Exception " + e);
        }
        cache.put(pool, subnet);
    }

    @ValidateCallback(callPoint = "ipa_validate",
                      callType = { ValidateCBType.VALIDATE })
    public void validate(DpTrans trans, ConfObject [] kp,
                         ConfValue newval) throws DpCallbackException {
        ConfTag tag;
        ConfKey pool;
        String addr;
        String mask;
        int idx = kp.length - 1;

        LOGGER.debug("Validating " + new ConfPath(kp));
        th = trans.getTransaction();

        tag = (ConfTag) kp[idx--];
        if (tag.getTagHash() == resourceAllocator._resource_pools) {
            tag = (ConfTag) kp[idx--];
            if (tag.getTagHash() == resourceAllocator._ip_address_pool) {
                pool = (ConfKey) kp[idx--];

                tag = (ConfTag) kp[idx--];

                if (! cache.containsKey(pool))
                    buildCache(pool);

                Vector<Entry> c = cache.get(pool);

                switch (tag.getTagHash()) {
                    /* Subnets must not overlap and exclusions must be
                     * subsets of existing subnets. */
                case ipaddressAllocator._exclude:
                    ConfKey exclude = (ConfKey) kp[idx--];
                    addr = exclude.elementAt(0).toString();
                    mask = exclude.elementAt(1).toString();
                    boolean match = false;

                    try {
                        Subnet excl = new Subnet(addr, mask);

                        for (Entry e : c) {
                            if (e.s.contains(excl)) {
                                match = true;
                                break;
                            }
                        }
                        if (match == false) {
                            throw
                                new DpCallbackException("No subnet contains"
                                                        + " exclusion " +
                                                        excl +
                                                        " in ip pool.");
                        }
                    }
                    catch (UnknownHostException e) {
                        LOGGER.error("", e);
                    }
                    catch (InvalidNetmaskException e) {
                        LOGGER.error("", e);
                    }
                    return;
                case ipaddressAllocator._subnet:
                    LOGGER.info("Subnet");
                    ConfKey subn = (ConfKey) kp[idx--];
                    addr = subn.elementAt(0).toString();
                    mask = subn.elementAt(1).toString();
                    ConfKey overlap = null;

                    try {
                        Subnet subnet = new Subnet(addr, mask);

                        for (Entry e : c) {
                            if (e.k.equals(subn))
                                continue;

                            if (subnet.overlaps(e.s)) {
                                overlap = e.k;
                                break;
                            }
                        }

                        if (overlap != null) {
                            throw
                                new DpCallbackException("Overlapping subnets " +
                                                        subn + " and " +
                                                        overlap);

                        }
                    }
                    catch (UnknownHostException e) {
                        LOGGER.error("", e);
                    }
                    catch (InvalidNetmaskException e) {
                        LOGGER.error("", e);
                    }

                    return;
                }
            }
        }

        LOGGER.error("Unhandled validation: " + new ConfPath(kp));
    }

}
