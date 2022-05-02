package com.cisco.nso.ipam.exceptions;

public class AddressPoolEmptyException extends AddressPoolException {
    private static final long serialVersionUID = -4859884788810786983L;

    public AddressPoolEmptyException() {
        super(
              "No addresses available",
              AddressPoolException.MSG_ADDRESSPOOL_EMPTY,
              new String[]{}
              );
    }
}
