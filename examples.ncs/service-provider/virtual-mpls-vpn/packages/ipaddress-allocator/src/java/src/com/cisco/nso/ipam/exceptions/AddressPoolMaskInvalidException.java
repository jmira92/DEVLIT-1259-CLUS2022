package com.cisco.nso.ipam.exceptions;

public class AddressPoolMaskInvalidException extends AddressPoolException {
    private static final long serialVersionUID = -4859884788810786893L;

    public AddressPoolMaskInvalidException(String msg) {
        super(
              msg,
              AddressPoolException.MSG_ADDRESSPOOL_MASK_INVALID,
              new String[]{}
              );
    }
}
