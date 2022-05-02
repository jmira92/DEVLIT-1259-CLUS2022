package com.cisco.nso.ipam;

import java.io.Serializable;

public class Allocation implements Serializable {
    private static final long serialVersionUID = -7671139904147340643L;
    private static final String RESOURCE_TYPE =
        "entity.resourcepool.resource.allocation";

    /*
     * HACK
     */

    private String uuid;
    private String etype;
    private Subnet allocated;
    private String occupant;

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

    public Allocation() {
    }

    public Allocation(Subnet allocated, String occupant) {
        this.allocated = allocated;
        this.occupant = occupant;
        this.etype = RESOURCE_TYPE;
    }

    public Allocation(Allocation that) {
        super();
        this.allocated = that.allocated;
        this.occupant = that.occupant;
        this.etype = RESOURCE_TYPE;
    }

    public Subnet getAllocated() {
        return allocated;
    }

    public void setAllocated(Subnet allocated) {
        this.allocated = allocated;
    }

    public String getOccupant() {
        return occupant;
    }

    public void setOccupant(String occupant) {
        this.occupant = occupant;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "{\"allocated\":" + allocated + ",\"occupant\":" +
            occupant + "}";
    }

    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + allocated.hashCode();
        result = prime * result + occupant.hashCode();
        return result;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Allocation other = (Allocation) obj;
        if (allocated != other.allocated)
            return false;
        if (occupant != other.occupant)
            return false;
        return true;
    }
}
