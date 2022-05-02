package com.cisco.nso.ipam.exceptions;

public class AddressNotAllocatedException extends AddressPoolException {
    private static final long serialVersionUID = -7535186658459851094L;


    public AddressNotAllocatedException() {
        super(
              "Address was not allocated from the pool",
              AddressPoolException.MSG_ADDRESSPOOL_ADDRESS_NOT_ALLOCATED,
              new String [] {""}
              );
    }

    /*
      public AddressNotAllocatedException(String message) {
      super(message);
      }
    */

    /*
      public AddressNotAllocatedException(Throwable cause) {
      super(cause);
      }
    */

    /*
      public AddressNotAllocatedException(String message, Throwable cause) {
      super(message, cause);
      }
    */

    public AddressNotAllocatedException(String message, String etype,
                                        String [] eparms)
    {
        super(message, etype, eparms);
    }
}
