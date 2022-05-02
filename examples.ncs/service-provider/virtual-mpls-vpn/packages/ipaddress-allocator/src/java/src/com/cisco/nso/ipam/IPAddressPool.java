package com.cisco.nso.ipam;

import java.io.Serializable;
import java.net.InetAddress;
import java.net.Inet6Address;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.cisco.nso.ipam.exceptions.AddressNotAllocatedException;
import com.cisco.nso.ipam.exceptions.AddressPoolEmptyException;
import com.cisco.nso.ipam.exceptions.AddressPoolException;
import com.cisco.nso.ipam.exceptions.AddressPoolMaskInvalidException;
import com.cisco.nso.ipam.exceptions.AddressRequestNotAvailableException;
import com.cisco.nso.ipam.exceptions.InvalidNetmaskException;
import com.cisco.nso.ipam.exceptions.UnreachableCodeException;
import com.cisco.nso.ipam.util.InetAddressRangeSet;


public class IPAddressPool implements Serializable {

    private Set<Subnet> subnets; /* Original Subnets, avoid handing out
                                  * /32 and /128 network and broadcast
                                  * addresses from these networks
                                  */
    private Set<Subnet> availables;
    private Set<Allocation> allocations;

    private String name;

    public IPAddressPool(String name,
                         Set<Subnet> availables,
                         Set<Allocation> allocations,
                         Set<Subnet> subnets) {
        this.name = name;
        this.availables = availables;
        this.allocations = allocations;
        this.subnets = subnets;
    }

    public String getName() {
        return name;
    }

    public synchronized Allocation allocate(int cidr, String owner)
        throws AddressPoolException {
        //
        // Iterate thru available subnets. The set is ordered from
        // narrowest to widest so we will choose the narrowest subnet
        // that fits the requested size.
        //
        for (Subnet availableSubnet : availables) {
            if (availableSubnet.getCIDRMask() == cidr &&
                notNetworkBroadcast(availableSubnet, cidr)) {
                availables.remove(availableSubnet);
                allocations.add(new Allocation(availableSubnet, owner));
                // System.out.println("add to allocated: " +
                // availableSubnet.toString());
                return new Allocation(availableSubnet, owner);
            } else if (availableSubnet.getCIDRMask() < cidr) {
                return allocateFrom(availableSubnet, cidr, owner);
            }
        }

        //
        // If we get here, then there is no room in the pool for the
        // requested subnet
        //
        StringBuffer availMasks = new StringBuffer();
        for (Subnet availSubnet : availables) {
            int msk = availSubnet.getCIDRMask();
            if ((msk != 30) && (msk != 31) && (msk != 32)) {
                availMasks.append(msk + " ");
            }
        }
        if (availMasks.length() == 0) { // empty pool
            throw new AddressPoolEmptyException();
        } else {
            throw new AddressPoolMaskInvalidException(availMasks.toString());
        }
    }

    private boolean notNetworkBroadcast(Subnet net, int cidr) {
        if (cidr != 32 && cidr != 128) {
            return true;
        }

        InetAddress a = net.getAddress();

        for(Subnet sub: subnets) {
            InetAddress na = sub.getAddress();
            InetAddress ba = sub.getBroadcast();
            if (na instanceof Inet6Address) {
                if (sub.getCIDRMask() > 126)
                    // not broadcast to worry about for such small networks
                    continue;
                if (na.equals(a) || ba.equals(a))
                    return false;
            }
            else {
                if (sub.getCIDRMask() > 30)
                    // not broadcast to worry about for such small networks
                    continue;
                if (na.equals(a) || ba.equals(a))
                    return false;
            }
        }

        return true;
    }

    private Allocation allocateFrom(Subnet source, int request, String owner) {
        // System.err.println("allocateFrom: "+source+" size: "+request);

        assert(source.getCIDRMask() <= request);
        //
        //      In any case, source subnet will no longer be available
        //
        availables.remove(source);
        if (source.getCIDRMask() == request) {
            Allocation a = new Allocation(source, owner);
            allocations.add(a);
            // System.out.println("add to allocated: " + source.toString());
            return a;
        }
        //
        //      Split source and put the two halves on the available list
        //      and recurse on the first half.
        //
        try {
            Subnet[] subs = source.split();

            // Special case here to handle subnet of just
            // two addresses, ie /31.

            boolean isIpv6 =
                source.getAddress() instanceof Inet6Address;

            if ((!isIpv6 && subs.length == 4 && request == 31 ) ||
                ( isIpv6 && subs.length == 4 && request == 127)) {
                // Split the four into two twos instead.
                Subnet[] newSubs = source.split4into2();
                // Add only single addresses to available.
                // add 3rd and 4th to available.
                availables.add(subs[2]);
                availables.add(subs[3]);
                // Allocate as a 2-address "Subnet".
                Allocation a =
                    new Allocation(new Subnet(subs[0].getAddress(), request),
                                   owner);
                allocations.add(a);
                return a;
            }
            else {

                for (int i = 0; i < subs.length; i++) {
                    availables.add(subs[i]);
                }

                int Sub0CIDR = subs[0].getCIDRMask();

                if ((!isIpv6 && request == 32 &&  Sub0CIDR == 32) ||
                    ( isIpv6 && request == 128 && Sub0CIDR == 128)) {

                    if (notNetworkBroadcast(subs[0], request))
                        return allocateFrom(subs[0], request, owner);
                    else
                        return allocateFrom(subs[1], request, owner);
                }
                else {
                    return allocateFrom(subs[0], request, owner);
                }
            }
        } catch (InvalidNetmaskException e) {
            throw new UnreachableCodeException("Unable to split subnet " +
                                               source.toString() + ": " +
                                               e.getMessage());
        } catch (UnknownHostException e) {
            throw new UnreachableCodeException("Unable to split subnet " +
                                               source.toString() + ": " +
                                               e.getMessage());
        }
    }

    public synchronized void addToAvailable(Subnet subnet) {
        if (subnet == null)
            return; // noop

        availables.add(subnet);

        /* With IP Address Reservation we now have the situation where
         * the user may allocate subnets and then free portions of the
         * subnets.  The original code (commented out below) did not
         * handle this situation correctly.  For example if the
         * available subnets were 10.1.0.0/32, 10.1.0.1/32,
         * 10.1.0.2/31, 10.1.0.4/30, 10.1.0.8/29, 10.1.0.16/28,
         * 10.1.0.32/27, 10.1.0.64/26 and 10.1.0.128/25, and subnet
         * 10.1.0.0/25 was added to the available pool, it would merge
         * 10.1.0.18/25 and 10.1.0.0/25 into 10.1.0.0/24, without
         * removing the others, so many subnets would be in the
         * available list twice.  Then when for instance 10.1.0.16/28
         * was reserved and removed, 10.1.0.0/24 would still be there,
         * and 10.1.0.0/24 includes 10.1.0.16/28
         */

        // Put all available addresses into a RangeSet.
        InetAddressRangeSet rangeSet = new InetAddressRangeSet(availables);

        // Now copy them into available as subnets.
        availables.clear();

        try {
            for (Subnet eachSubnet : rangeSet.asSubnetSet()) {
                assert(eachSubnet instanceof Subnet);
                availables.add(eachSubnet);
            }
        } catch (InvalidNetmaskException e1) {
            // Should not happen.
            // TODO: wrap in unchecked exception and throw?
            e1.printStackTrace();
        } catch (UnknownHostException e1) {
            // Should not happen.
            // TODO: wrap in unchecked exception and throw?
            e1.printStackTrace();
        }

    }

    public synchronized void removeFromAvailable(Subnet subnet)
        throws AddressPoolException {

        if (subnet == null)
            return; // noop

        // Must exactly match an available subnet or be contained in
        // another subnet.
        if (availables.contains(subnet)) {
            // System.err.println("found subnet removing: "+subnet);
            availables.remove(subnet);
        } else {
            // We did not find an exact match, look for subnet
            // that contains the desired subnet, split and call
            // recursively

            for (Subnet source: availables) {
                if (source.contains(subnet)) {
                    // Split subnet and remove the part we are
                    // looking for
                    availables.remove(source);
                    assert(source.getCIDRMask() < subnet.getCIDRMask());

                    // Split source and put the two halves on the available list
                    // recurse on the half that contains the subnet

                    try {

                        Subnet[] subs = source.split();

                        if (subs.length == 4 && subnet.size() == 0) {
                            // split the four into two twos instead.
                            Subnet[] newSubs = source.split4into2();
                            if (newSubs[0].contains(subnet)) {
                                // add 3rd and 4th to available
                                availables.add(subs[2]);
                                availables.add(subs[3]);
                            }
                            else {
                                // add 1st and 2nd to availables
                                availables.add(subs[0]);
                                availables.add(subs[1]);
                            }

                            return;
                        }
                        else {
                            Subnet next = null;
                            for(int i = 0 ; i < subs.length ; i++) {
                                availables.add(subs[i]);
                                if (subs[i].contains(subnet))
                                    next = subs[i];
                            }

                            removeFromAvailable(subnet);
                            return;
                        }
                    } catch (InvalidNetmaskException e) {
                        throw new AddressRequestNotAvailableException(
                          "Address " + subnet +
                          " is not an available subnet defined by the pool",
                          AddressPoolException.
                          MSG_ADDRESSPOOL_REQUESTED_ADDRESS_NOT_DEFINED_BY_POOL,
                          new String[] {subnet.toString()});
                    } catch (UnknownHostException e) {
                        throw new AddressRequestNotAvailableException(
                          "Address " + subnet +
                          " is not an available subnet defined by the pool",
                          AddressPoolException.
                          MSG_ADDRESSPOOL_REQUESTED_ADDRESS_NOT_DEFINED_BY_POOL,
                          new String[] {subnet.toString()});
                    }
                }
            }

            // No subnet found, throw error
            throw new AddressRequestNotAvailableException(
                    "Address " + subnet +
                    " is not an available subnet defined by the pool",
                    AddressPoolException.
                    MSG_ADDRESSPOOL_REQUESTED_ADDRESS_NOT_DEFINED_BY_POOL,
                    new String[] {subnet.toString()});
        }
    }

    public synchronized void release(Allocation allocation)
        throws AddressPoolException {
        if (!allocations.contains(allocation)) {
            throw new AddressNotAllocatedException(
                "Allocation " + allocation + " was not allocated from the pool",
                AddressPoolException.MSG_ADDRESSPOOL_ADDRESS_NOT_ALLOCATED,
                new String [] {allocation.toString()}
                                                   );
        }
        allocations.remove(allocation);
        addToAvailable(allocation.getAllocated());
    }

    public synchronized void release(InetAddress addr)
        throws AddressPoolException {
        //
        //      Need to find allocated with this network address.
        //      TODO: better way to do this?
        //
        for (Iterator<Allocation> i = allocations.iterator(); i.hasNext(); ) {
            Allocation allocated = i.next();
            if (allocated.getAllocated().getAddress().equals(addr)) {
                release(allocated);
                return;
            }
        }
        //
        //      If we make it here, then the address wasn't found
        //
        throw new AddressNotAllocatedException(
                  "Address " + addr + " was not allocated from the pool",
                  AddressPoolException.MSG_ADDRESSPOOL_ADDRESS_NOT_ALLOCATED,
                  new String [] {addr.toString()}
                                               );
    }

    public synchronized void releaseAll() {
        for (Allocation next : allocations) {
            addToAvailable(next.getAllocated());
        }
        allocations.clear();
    }

    public Collection<Subnet> getAvailables() {
        return availables;
    }

    public Collection<Allocation> getAllocations() {
        return allocations;
    }

    public synchronized void addAllocation(Allocation a) {
        this.allocations.add(a);
    }

    public synchronized void clearAllocation() {
        this.allocations.clear();
    }

    private static class SubnetComparator implements Comparator<Subnet>,
                                                     Serializable {
        private static final long serialVersionUID = -670763473271091265L;

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
