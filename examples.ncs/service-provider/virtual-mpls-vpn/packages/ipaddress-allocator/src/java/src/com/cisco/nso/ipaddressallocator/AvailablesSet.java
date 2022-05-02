package com.cisco.nso.ipaddressallocator;

import java.io.IOException;
import java.util.Comparator;
import java.util.TreeSet;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.cisco.nso.ipaddressallocator.namespaces.ipaddressAllocator;
import com.cisco.nso.ipam.Subnet;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfUInt16;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuContext;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;

class AvailablesSet extends TreeSet<Subnet> {
    private static Logger LOGGER = LogManager.getLogger(AvailablesSet.class);

    private String poolName;

    public NavuList cdbAvailables;

    public AvailablesSet(NavuContext operContext,
                         String poolName,
                         Comparator<Subnet> comp) {
        super(comp);
        this.poolName = poolName;

        try {
            NavuContainer base = new NavuContainer(operContext);
            NavuContainer root = base.container(ipaddressAllocator.hash);
            NavuContainer ipallocator = root.namespace(ipaddressAllocator.id)
                    .container(ipaddressAllocator._ip_allocator_);
            NavuList pool = ipallocator.list(ipaddressAllocator._pool_);
            NavuContainer myPool = pool.elem(poolName);

            if (myPool == null) {
                // missing create
                // System.err.println("pool missing, creating");
                pool.create(poolName);
                myPool = pool.elem(poolName);
            }

            cdbAvailables = myPool.list(ipaddressAllocator._available_);

            // System.err.println("Adding existing availables");

            for(NavuContainer avail: cdbAvailables.elements()) {
                String address = avail.leaf(ipaddressAllocator._address_).
                    value().toString();
                int mask = (int) ((ConfUInt16) avail.leaf(ipaddressAllocator.
                                                          _cidrmask_).
                                  value()).longValue();
                Subnet sub = new Subnet(address,mask);
                // System.err.println("Adding Available("+sub+")");
                super.add(sub);
            }
        }
        catch (Exception e) {
            LOGGER.error("", e);
        }
    }

    public boolean add(Subnet sub) {
        boolean res = super.add(sub);

        if (res) {
            try {
                NavuContainer avail =
                    cdbAvailables.create(new String[]
                        {sub.getAddress().getHostAddress(),
                         Integer.toString(sub.getCIDRMask())});

                persistTrans(cdbAvailables);
            }
            catch (Exception ex) {
                LOGGER.error("", ex);
            }
        }

        return res;
    }

    public boolean remove(Object o) {
        boolean res = super.remove(o);
        Subnet sub = (Subnet) o;

        if (res) {
            try {
                cdbAvailables.delete(new String[]
                    {sub.getAddress().getHostAddress(),
                     Integer.toString(sub.getCIDRMask())});

                persistTrans(cdbAvailables);
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
            for(NavuContainer sub: cdbAvailables.elements()) {
                cdbAvailables.delete(sub.getKey());
            }

            persistTrans(cdbAvailables);
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
