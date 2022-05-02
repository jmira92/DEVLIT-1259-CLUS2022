package com.cisco.nso.idallocator;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.cisco.nso.idallocator.namespaces.idAllocator;
import com.tailf.navu.NavuContext;

class AvailablesSet extends ReservationsSet {
    private static Logger LOGGER = LogManager.getLogger(ReservationsSet.class);

    public AvailablesSet(NavuContext operContext,
                         String poolName) {
        super(operContext, poolName, idAllocator._available_);
    }
}
