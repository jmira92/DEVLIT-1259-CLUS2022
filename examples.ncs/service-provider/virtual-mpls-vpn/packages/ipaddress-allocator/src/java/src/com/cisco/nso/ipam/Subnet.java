package com.cisco.nso.ipam;

import java.math.BigInteger;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.Comparator;

import com.cisco.nso.ipam.exceptions.InvalidNetmaskException;
import com.cisco.nso.ipam.util.BitString;


/**
 * Used to represent an IP Subnet in the same spirit as the
 * java.net.InetAddress class.
 *
 * <p>
 * The class is "read only" and can therefore be referred to without worrying
 * about values being changed by another reference holder.
 * </p>
 *
 * <p>
 * Constructors are provided to easily instantiate the class using CIDR
 * notation or classic address/mask notation. Note : Subnets must always use
 * valid CIDR masks, whichever way they are instantiated.
 * </p>
 *
 * <p>
 * Refer to {@link InvalidNetmaskException} for a better definition of a CIDR
 * mask.
 * </p>
 */
public class Subnet implements java.io.Serializable {
    private static final long serialVersionUID = 2324848105420911407L;
    private static final String RESOURCE_ETYPE = "entity.resourcepool.resource";
    //private static final long serialVersionUID = -7690811045910877065L;

    /*
     * HACK !!
     */
    private String uuid;
    private String etype;
    protected static final int
        SIZEOF_INET4  = 4,      // size of an IPv4 address in bytes
        SIZEOF_INET6  = 16,     // size of an IPv6 address in bytes
        SIZEOF_INT    = 4,      // size of the int type in bytes
        BITS_PER_BYTE = 8;      // size of a byte in bits

    protected static final int
        MAX_PREFIX4   = SIZEOF_INET4 * BITS_PER_BYTE,
        MAX_PREFIX6   = SIZEOF_INET6 * BITS_PER_BYTE;

    /**********************************************************************/
    private InetAddress           address;
    private int                   cidrmask;

    private transient InetAddress mask, broadcast;

    public static final Subnet ANY;
    // TODO define ANY6

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public String getEtype() {
        return etype;
    }

    public void setEtype(String etype) {
        this.etype = etype;
    }

    static {
        Subnet any = null;

        try {
            any = new Subnet(InetAddress.getByName("0.0.0.0"), 0);
        } catch (Exception e) {
            // shouldn't happen
            assert(false);
        } finally {
            ANY = any;
        }
    }

    /**********************************************************************/

    /**
     * creates a new Subnet given a subnet expression, ie a String containing :
     * &lt;IP address&gt;'&thinsp;/&thinsp;'(&lt;netmask expressed in dotquad
     * notation&gt; | &lt;int mask length&gt;)
     *
     * @param expression DOCUMENT ME!
     *
     * @throws UnknownHostException if the &lt;IP address&gt; is not valid or
     *         if the netmask.
     * @throws InvalidNetmaskException if the expression trailing the
     *         '&thinsp;/&thinsp;' is not     recognized as a valid netmask.
     */
    public Subnet(String expression)
        throws UnknownHostException, InvalidNetmaskException {
        this(expression.contains("/") ?
             expression.substring(0, expression.indexOf('/')) : expression,
             expression.contains("/") ?
             expression.substring(expression.indexOf('/') + 1) : "32");
    }

    /**
     * creates a new Subnet given an expression for the the subnet address and
     * its netmask. <br>
     * The mask argument may be specified either as a CIDR mask length, or as
     * a netmask in dotquad notation.
     *
     * @param address IP Address or host name
     * @param mask Dot-decimal notation OR integer mask length ( 1 to 31 )
     *
     * @throws UnknownHostException if the &lt;IP address&gt; is not valid or
     *         if the netmask.
     * @throws InvalidNetmaskException if the expression is not     recognized
     *         as a valid netmask.
     */
    public Subnet(String address, String mask)
        throws UnknownHostException, InvalidNetmaskException {
        this(InetAddress.getByName(address), parseMask(mask));
    }

    public Subnet() {}

    /**
     * creates a subnet with the specified network address and mask.
     *
     * @param address DOCUMENT ME!
     * @param mask DOCUMENT ME!
     *
     * @throws InvalidNetmaskException if the expression is not     recognized
     *         as a valid netmask.
     */
    public Subnet(InetAddress address, InetAddress mask)
        throws InvalidNetmaskException {
        if (! mask.getClass().equals(address.getClass())) {
            throw new InvalidNetmaskException(
                     "Netmask and address must share " +
                     "the same address family");
        }
        this.cidrmask     = mask2prefix(mask);
        this.address      = networkOf(address, cidrmask);
        this.mask         = mask;
        this.etype = RESOURCE_ETYPE;
    }

    /**
     * creates a subnet with the specified network address and mask.
     *
     * @param address DOCUMENT ME!
     * @param mask DOCUMENT ME!
     *
     * @throws InvalidNetmaskException if the mask is not valid,     i.e. ! ( 0
     *         &lt;= mask &lt;= 32)
     */
    public Subnet(InetAddress address, int mask)
        throws InvalidNetmaskException {
        if (address instanceof Inet4Address) {
            checkPrefix4(mask);
        } else if (address instanceof Inet6Address) {
            checkPrefix6(mask);
        } else {
            assert(false);
        }
        this.address      = networkOf(address, mask);
        this.cidrmask     = mask;
        this.etype = RESOURCE_ETYPE;
    }

    public Subnet(String address, int mask)
        throws UnknownHostException, InvalidNetmaskException {
        this(InetAddress.getByName(address), mask);
    }

    private static int parseMask(String smask)
        throws InvalidNetmaskException {
        try {
            int cidrmask;
            if (smask.indexOf('.') == -1) {
                // interpret the mask as an int
                cidrmask = Integer.parseInt(smask);
            } else {
                // interpret the mask as an address
                InetAddress mask = InetAddress.getByName(smask);
                cidrmask = mask2prefix(mask);
            }
            return cidrmask;
        } catch (NumberFormatException nfe) {
            throw new InvalidNetmaskException(smask +
                   " is not a valid netmask expression");
        } catch (UnknownHostException uhe) {
            throw new InvalidNetmaskException(
                   smask +" is not a valid netmask expression");
        }
    }

    private static void checkPrefix4(int prefix)
        throws InvalidNetmaskException {
        if ((prefix < 0) || (prefix > MAX_PREFIX4)) {
            throw new InvalidNetmaskException(
                   "IPv4 prefix must be within [0-" + MAX_PREFIX4 + "]");
        }
    }

    private static void checkPrefix6(int prefix)
        throws InvalidNetmaskException {
        if ((prefix < 0) || (prefix > MAX_PREFIX6)) {
            throw new InvalidNetmaskException(
                          "IPv6 netmask must be within [0-" + MAX_PREFIX6 +
                          "]");
        }
    }

    public static int mask2prefix(InetAddress addr)
        throws InvalidNetmaskException {
        byte[] baddr = addr.getAddress();
        int prefix = 0;
        int curbyte = 0;
        for(;;) {
            boolean nextByteMustBe0 = (baddr[curbyte] != -1);
            while (baddr[curbyte] != 0) {
                if (baddr[curbyte] > 0) {
                    // hibit (ie negative value) must be set if the
                    // byte is non 0
                    throw new InvalidNetmaskException(
                              addr.getHostAddress() +
                              " is not a valid netmask");
                }
                baddr[curbyte] <<= 1;
                prefix++;
            }
            curbyte++;
            if (curbyte >= baddr.length) {
                break;
            }
            if (nextByteMustBe0 && (baddr[curbyte] != 0)) {
                throw new InvalidNetmaskException(addr.getHostAddress() +
                                                  " is not a valid netmask");
            }
        }
        return prefix;
    }

    public static Inet4Address prefix2mask4(int prefix)
        throws InvalidNetmaskException {
        checkPrefix4(prefix);
        ByteBuffer bb = ByteBuffer.allocate(SIZEOF_INET4);
        return (Inet4Address) prefix2mask(bb, prefix);
    }

    public static Inet6Address prefix2mask6(int prefix)
        throws InvalidNetmaskException {
        checkPrefix6(prefix);
        ByteBuffer bb = ByteBuffer.allocate(SIZEOF_INET6);
        return (Inet6Address) prefix2mask(bb, prefix);
    }

    private static InetAddress prefix2mask(ByteBuffer bb, int prefix) {
        try {
            int tmpPrefix = prefix;
            while (tmpPrefix > 0) {
                int shiftWidth = tmpPrefix > SIZEOF_INT * BITS_PER_BYTE ?
                    SIZEOF_INT * BITS_PER_BYTE :
                    tmpPrefix;
                tmpPrefix -= shiftWidth;
                bb.putInt(-1 << (SIZEOF_INT * BITS_PER_BYTE - shiftWidth));
            }
            while(bb.hasRemaining()) {
                bb.putInt(0);
            }
            return InetAddress.getByAddress(bb.array());
        } catch (UnknownHostException uhe) {
            // should be valid by construction
            assert(false);
            return null;
        }
    }

    /**
     * normalizes a subnets address based on a CIDR mask length. <br>
     * the resulting InetAddress is the one passed in as a parameter where
     * only the network part is retained.
     *
     * @param a DOCUMENT ME!
     * @param prefix DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static InetAddress networkOf(InetAddress a, int prefix) {
        try {
            byte[] baddr = a.getAddress();
            int tmpPrefix = prefix;
            for (int i = 0 ; i < baddr.length ; i++) {
                if (tmpPrefix > BITS_PER_BYTE) {
                    tmpPrefix -= BITS_PER_BYTE;
                } else if (tmpPrefix > 0) {
                    int shiftWidth = tmpPrefix;
                    tmpPrefix -= shiftWidth;
                    baddr[i] &= (-1 << (BITS_PER_BYTE - shiftWidth));
                } else {
                    baddr[i] = 0;
                }
            }
            return InetAddress.getByAddress(baddr);
        } catch (UnknownHostException uhe) {
            // should never happen since the built address is based
            // on an already valid one
            assert(false);
            return null;
        }
    }

    /**
     * returns the network address of the Subnet.
     *
     * @return DOCUMENT ME!
     */
    public InetAddress getAddress() {
        return address;
    }

    /**
     * returns the Subnet's netmask in dotquad notation.
     *
     * @return DOCUMENT ME!
     */
    public synchronized InetAddress getMask() {
        if (mask == null) {
            try {
                if (address instanceof Inet4Address) {
                    mask = prefix2mask4(cidrmask);
                } else if (address instanceof Inet6Address) {
                    mask = prefix2mask6(cidrmask);
                } else {
                    assert(false);
                }
            } catch (InvalidNetmaskException ine) {
                // can't happen since it is checked upon construction
                // of the Subnet object
                assert(false);
            }
        }
        return mask;
    }

    /**
     * returns the Subnet's netmask in CIDR notation.
     *
     * @return DOCUMENT ME!
     */
    public int getCIDRMask() {
        return cidrmask;
    }

    /**
     * returns the broadcast address of the subnet
     *
     * @return DOCUMENT ME!
     */
    public synchronized InetAddress getBroadcast() {
        if (broadcast == null) {
            try {
                // or the address with the complement of the netmask
                byte[] bmask = getMask().getAddress();
                byte[] baddr = address.getAddress();
                for (int i = 0 ; i < baddr.length ; i++) {
                    baddr[i] |= (~ bmask[i]);
                }
                broadcast = InetAddress.getByAddress(baddr);
            } catch (UnknownHostException uhe) {
                // shouldn't happen
                assert(false);
            }
        }
        return broadcast;
    }

    /**
     * Does this subnet contain another ?
     *
     * @param s DOCUMENT ME!
     *
     * @return true if the subnet passed in as a parmeter is contained in the
     *         one the method was called upon.
     */
    public boolean contains(Subnet s) {
        if (cidrmask > s.cidrmask) {
            return false;
       }

        return networkOf(s.getAddress(), cidrmask).equals(address);
    }

    /**
     * Does the subnet contain a specific address.
     *
     * @param a DOCUMENT ME!
     *
     * @return true if the address is contained in the subnet.
     */
    public boolean contains(InetAddress a) {
        return networkOf(a, cidrmask).equals(address);
    }

    /**
     * Compare the width, i.e. which one has more addresses.
     * If this is narrower than the other (i.e. the cidrMask
     * is greater) then return -1, else if the width is equal
     * return 0, else return 1.
     *
     * @param other  Another subnet
     * @return -1 if this is narrower, 0 if same, 1 if wider.
     */
    public int compareWidth(Subnet other) {
        return this.cidrmask > other.cidrmask ? -1 :
            this.cidrmask == other.cidrmask ? 0 :
            1;
    }

    public boolean overlaps(Subnet other) {
        if (equals(other))
            return true;

        if (contains(other) || other.contains(this)) {
            return true;
        }

        /*
         * This can't happen with simple prefix-ranges (since they are always
         * subsets when overlapping) but prepares us for the future.
         *
        InetAddress myFirst    = this.iterator(false).next();
        InetAddress otherFirst = other.iterator(false).next();
        InetAddress myLast     = this.iterator(true).next();
        InetAddress otherLast  = other.iterator(true).next();
        */
        /* Overlap:
         *  myFirst <= otherLast && myLast >= otherFirst
         *
         *
        /
        /*
        if ((compare(myFirst, otherLast) < 1) &&
            (compare(myLast, otherFirst) > -1))
            return true;
        */

        return false;
    }

    /**
     * retrieve an Iterator on the addresses contained in the set the "first"
     * and last addresses are discarded since they are broadcast addresses. <br>
     * The iteration will only contain non broadcast addresses, furthermore
     * the call will only behave correctly if called on a /30 or larger
     * network.
     *
     * @return DOCUMENT ME!
     */
    public Iterator<InetAddress> iterator() {
        return new SubnetIterator();
    }

    public Iterator<InetAddress> iterator(boolean reverse) {
        return new SubnetIterator(reverse);
    }

    /**
     * get an iterator positioned on the specified address a. The first call to
     * next on the iterator will return the next address following a that is
     * inside the subnet.
     *
     * @param a DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws IllegalArgumentException DOCUMENT ME!
     */
    public Iterator<InetAddress> iterator(InetAddress a) {
        if (!contains(a)) {
            throw new IllegalArgumentException("Address " + a.getHostAddress() +
                                               " is not part of the " + this +
                                               " subnet.");
        }

        return new SubnetIterator(a);
    }

    public int getSingleHostPrefix() {
        if (address instanceof Inet4Address) return MAX_PREFIX4;
        if (address instanceof Inet6Address) return MAX_PREFIX6;
        return -1; // Who knows?
    }

    public boolean isSingleHost() {
        return ((address instanceof Inet4Address) &&
                (cidrmask == MAX_PREFIX4)) ||
            ((address instanceof Inet6Address) &&
             (cidrmask == MAX_PREFIX6));
    }

    /**
     * the number of non-broadcast addresses available in the subnet. the /32
     * subnets are a special case because they have a size of 1.
     * warning the method returns a long which should serve most purposes
     * but some IPv6 subnets can actually be bigger than that.
     *
     * TODO : maybe throw an exception if the long is overflowed ... ?
     *
     * @return DOCUMENT ME!
     */
    public long size() {
        if (isSingleHost()) {
            return 1;
        } else {
            int shiftWidth = 0;
            if (address instanceof Inet4Address) {
                shiftWidth = MAX_PREFIX4 - cidrmask;
            } else if (address instanceof Inet6Address) {
                shiftWidth = MAX_PREFIX6 - cidrmask;
            } else {
                assert(false);
            }
            BigInteger res = BigInteger.ONE.shiftLeft(shiftWidth).
                subtract(BigInteger.ONE).
                subtract(BigInteger.ONE);
            BigInteger max = new BigInteger(Long.toString(Long.MAX_VALUE));
            if (res.compareTo(max) > 0)
                // return plenty
                return Long.MAX_VALUE;
            else
                return res.longValue();
        }
    }

    /*
     * Correct behavior for Object.equals()
     */
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }

        try {
            Subnet other = (Subnet) o;

            return (cidrmask == other.cidrmask) &&
                address.equals(other.address);
        } catch (ClassCastException cce) {
            return false;
        } catch (NullPointerException npe) {
            return false;
        }
    }

    /*
     * Correct behavior when used in a HashTable
     */
    public int hashCode() {
        return address.hashCode() ^ cidrmask;
    }

    /**********************************************************************/
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append(getAddress().getHostAddress());
        sb.append("/");
        sb.append(cidrmask);

        return sb.toString();
    }

    private class SubnetIterator implements Iterator<InetAddress> {
        BigInteger i;
        BigInteger max;
        BigInteger min;
        boolean reverse = false;

        SubnetIterator() {
            this(address);
        }

        SubnetIterator(boolean reverse) {
            this(address);
            this.reverse = reverse;
        }

        SubnetIterator(InetAddress a) {
            min = new BigInteger(1, a.getAddress());
            max = new BigInteger(1, getBroadcast().getAddress());
            i = reverse ? max : min;

            // special case, there is a single valid address
            // in the Subnet. In other cases we must make sure
            // to skip the first address in the subnet.
            if ((! isSingleHost()) && a.equals(address)) {
                i = i.add(BigInteger.ONE);
            }
        }

        public boolean hasNext() {
            return isSingleHost() ?
                i.equals(max) :
                reverse ?
                i.compareTo(min) > 0 :
                i.compareTo(max) < 0;
        }

        public InetAddress next() {
            try {
                byte[] naddr = null;
                int targetSize =
                    (address instanceof Inet4Address) ? SIZEOF_INET4 :
                    (address instanceof Inet6Address) ? SIZEOF_INET6 :
                    0; // can't happen
                byte[] baddr = i.toByteArray();
                if (baddr.length == targetSize) {
                    naddr = baddr;
                } else if (baddr.length < targetSize) {
                    // big int is smaller than what we need
                    naddr = new byte[targetSize];
                    System.arraycopy(baddr, 0, naddr,
                                     targetSize - baddr.length,
                                     baddr.length);
                } else {
                    // first byte is 0 and should be ignored
                    assert((baddr[0] == 0) &&
                           (baddr.length == targetSize + 1));
                    naddr = new byte[targetSize];
                    System.arraycopy(baddr, 1, naddr, 0, naddr.length);
                }
                InetAddress nextAddress = InetAddress.getByAddress(naddr);
                i = reverse ? i.subtract(BigInteger.ONE) :
                    i.add(BigInteger.ONE);
                return nextAddress;
            } catch (UnknownHostException uhe) {
                assert(false);
                return null;
            }
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    public boolean isBroadcastFor(Subnet addr) {
        //
        // The given address ANDed with the subnet's broadcast address
        // equal to the subnet's broadcast address
        //
        InetAddress broadcast = getBroadcast();

        byte[] bbcast = broadcast.getAddress();
        byte[] baddr = addr.getAddress().getAddress();
        for (int i = 0 ; i < baddr.length ; i++) {
            if (!(bbcast[i] == (baddr[i] & bbcast[i]))) return false;
        }
        return true;
    }

    /**
     * Special case code for IPAddressReservation.
     *
     * Normally a 4-address Subnet cannot be split into two "Subnets"
     * but has to be split into four single addresses.  When
     * attempting to reserve a two-address block, we create what
     * appears to be a "Subnet" with only two addresses.  When
     * breaking down a four-address subnet, we call this special case
     * to break it into 2.
     * @return
     * @throws InvalidNetmaskException
     * @throws UnknownHostException
     */
    public Subnet[] split4into2()
        throws InvalidNetmaskException, UnknownHostException {
        if (size() != 2) {
            throw new InvalidNetmaskException("Not a four-address subnet.");
        }

        Subnet[] result = new Subnet[2];
        InetAddress network = getAddress();
        result[0] = new Subnet(network, cidrmask + 1);
        BitString addr = new BitString(network.getAddress());
        addr.setBit(cidrmask, 1);
        result[1] = new Subnet(InetAddress.getByAddress(addr.getData()),
                               cidrmask + 1);
        return result;
    }

    public Subnet[] split()
        throws InvalidNetmaskException, UnknownHostException {


        // MSL 3/23/2012: One-address "Subnet" is already treated as a
        // special case.  Treat a two-address "Subnet" as a special
        // case too: allow it to be split into two one-address
        // "Subnets".  There really aren't two-address Subnets, but
        // for IP Address Reservation we use the Subnet class to
        // represent an address range, and a range of two addresses is
        // reasonable.

        // One-address "Subnet".size() == 1.  Two-address "Subnet".size() == 0.
        // Four-address Subnet (smallest REAL subnet).size() == 2.
        // Eight-address Subnet.size() == 6 and so on.

        boolean isIpv6 = (address instanceof Inet6Address);

        if (size() > 1 || size() == 0) {
            if (size() > 2) {
                Subnet[] result = new Subnet[2];
                InetAddress network = getAddress();
                //
                // First subnet has the same network address with a
                // wider mask
                //
                result[0] = new Subnet(network, cidrmask + 1);
                //
                // Second subnet has the last bit of the wider mask set
                //
                BitString addr = new BitString(network.getAddress());
                addr.setBit(cidrmask, 1);
                result[1] = new Subnet(InetAddress.getByAddress(addr.getData()),
                                       cidrmask + 1);
                return result;
            } else if (size() == 0 && isIpv6) {
                // Split two-address "Subnet" into 2 /128 addresses.
                Subnet[] result = new Subnet[2];
                InetAddress network = getAddress();
                byte[] addr = network.getAddress();
                result[0] = new Subnet(InetAddress.getByAddress(addr),128);
                addr[addr.length-1]++;
                result[1] = new Subnet(InetAddress.getByAddress(addr),128);
                return result;
            } else if (size() == 0 && !isIpv6) {
                // Split two-address "Subnet" into 2 /32 addresses.
                Subnet[] result = new Subnet[2];
                InetAddress network = getAddress();
                byte[] addr = network.getAddress();
                result[0] = new Subnet(InetAddress.getByAddress(addr),32);
                addr[addr.length-1]++;
                result[1] = new Subnet(InetAddress.getByAddress(addr),32);
                return result;
            } if (isIpv6) {
                //
                //   Must split the subnet into 4 /128 addresses at this point
                //
                Subnet[] result = new Subnet[4];
                InetAddress network = getAddress();
                byte[] addr = network.getAddress();
                for (int i = 0; i < result.length; i++) {
                    //addr[addr.length-1] |= i;
                    result[i] = new Subnet(InetAddress.getByAddress(addr), 128);
                    addr[addr.length-1]++;
                }
                return result;
            } else {
                //
                //      Must split the subnet into 4 /32 addresses at this point
                //
                Subnet[] result = new Subnet[4];
                InetAddress network = getAddress();
                byte[] addr = network.getAddress();
                for (int i = 0; i < result.length; i++) {
                    //addr[addr.length-1] |= i;
                    result[i] = new Subnet(InetAddress.getByAddress(addr), 32);
                    addr[addr.length-1]++;
                }
                return result;
            }
        }
        throw new InvalidNetmaskException("Can't split subnet further");
    }

    public static Subnet createSubnet(String expression)
        throws UnknownHostException, InvalidNetmaskException {
        return new Subnet(expression);
    }

    public String jsonValue() {
        return this.toString();
    }

    public int compare(InetAddress adr1, InetAddress adr2) {
        byte[] ba1 = adr1.getAddress();
        byte[] ba2 = adr2.getAddress();

        // general ordering: ipv4 before ipv6
        if(ba1.length < ba2.length) return -1;
        if(ba1.length > ba2.length) return 1;

        // we have 2 ips of the same type, so we have to compare each byte
        for(int i = 0; i < ba1.length; i++) {
            int b1 = unsignedByteToInt(ba1[i]);
            int b2 = unsignedByteToInt(ba2[i]);
            if(b1 == b2)
                continue;
            if(b1 < b2)
                return -1;
            else
                return 1;
            }
            return 0;
    }

    private int unsignedByteToInt(byte b) {
        return (int) b & 0xFF;
    }

}
/*
 * vim:sw=4:smarttab:expandtab:
 */
