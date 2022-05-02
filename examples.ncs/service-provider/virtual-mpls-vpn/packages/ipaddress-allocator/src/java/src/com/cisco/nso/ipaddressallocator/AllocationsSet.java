package com.cisco.nso.ipaddressallocator;

import java.io.IOException;
import java.util.HashSet;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.cisco.nso.ipaddressallocator.namespaces.ipaddressAllocator;
import com.cisco.nso.ipam.Allocation;
import com.cisco.nso.ipam.Subnet;
import com.tailf.cdb.CdbSession;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfUInt16;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuContext;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;


class AllocationsSet extends HashSet<Allocation> {
    private static Logger LOGGER = LogManager.getLogger(AllocationsSet.class);

    private CdbSession wsess;
    private String poolName;

    public NavuList cdbAllocations;

    public AllocationsSet(NavuContext operContext, String poolName) {
        super();

        // System.err.println("createing AllocationsSet");

        this.poolName = poolName;

        // pupulate from allocaitons stored in CDB

        try {
            NavuContainer base = new NavuContainer(operContext);
            NavuContainer root = base.container(ipaddressAllocator.hash);
            NavuContainer ipallocator =
                root.namespace(ipaddressAllocator.id)
                        .container(ipaddressAllocator._ip_allocator_);
            NavuList pool = ipallocator.list(ipaddressAllocator._pool_);
            NavuContainer myPool = pool.elem(poolName);

            if (myPool == null) {
                // missing create
                // System.err.println("pool missing, creating");
                pool.create(poolName);
                myPool = pool.elem(poolName);
            }

            cdbAllocations = myPool.list(ipaddressAllocator._allocation_);

            // System.err.println("Adding existing allocations");

            for(NavuContainer alloc: cdbAllocations.elements()) {
                String address = alloc.leaf(ipaddressAllocator._address_).
                    value().toString();
                int mask = (int) ((ConfUInt16) alloc.leaf(ipaddressAllocator.
                                                          _cidrmask_).
                                  value()).longValue();
                String owner = alloc.leaf(ipaddressAllocator._owner_).
                    value().toString();
                Subnet sub = new Subnet(address,mask);
                // System.err.println("Adding Allocation("+sub+","+owner+")");
                super.add(new Allocation(sub, owner));
            }
        }
        catch (Exception e) {
            LOGGER.error("Failed to setup up allocationsSet", e);
        }
    }

    public boolean add(Allocation e) {
        boolean res = super.add(e);

        if (res) {
            try {
                Subnet sub = e.getAllocated();

                NavuContainer alloc =
                    cdbAllocations.create(new String[]
                        {sub.getAddress().getHostAddress(),
                         Integer.toString(sub.getCIDRMask())});
                alloc.leaf(ipaddressAllocator._owner_).set(e.getOccupant());

                persistTrans(cdbAllocations);
            }
            catch (Exception ex) {
                LOGGER.error("", ex);
            }
        }

        return res;
    }

    public boolean remove(Object o) {
        boolean res = super.remove(o);

        Allocation e = (Allocation) o;

        if (res) {
            try {
                Subnet sub = e.getAllocated();

                cdbAllocations.delete(new String[]
                    {sub.getAddress().getHostAddress(),
                     Integer.toString(sub.getCIDRMask())});

                persistTrans(cdbAllocations);
            }
            catch (Exception ex ) {
                LOGGER.error("", ex);
            }

        }

        return res;
    }

    public void clear() {
        super.clear();

        try {
            for(NavuContainer sub: cdbAllocations.elements()) {
                cdbAllocations.delete(sub.getKey());
            }

            persistTrans(cdbAllocations);
        }
        catch (Exception ex ) {
            LOGGER.error("", ex);
        }
    }

    private void persistTrans(NavuNode node)
        throws NavuException, ConfException, IOException {
        NavuContext ctx = node.context();
        ctx.applyReplaceTrans();
    }
}
