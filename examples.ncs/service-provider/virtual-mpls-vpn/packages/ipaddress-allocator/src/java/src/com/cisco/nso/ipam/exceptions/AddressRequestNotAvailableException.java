package com.cisco.nso.ipam.exceptions;

public class AddressRequestNotAvailableException extends AddressPoolException {
    private static final long serialVersionUID = -6025511785288058348L;

    public AddressRequestNotAvailableException() {
        super(
              "requested address is not available from the pool",
              AddressPoolException.
              MSG_ADDRESSPOOL_REQUESTED_ADDRESS_NOT_AVAILABLE_FROM_POOL,
              new String[] {""}
              );
    }

    /*
      public AddressRequestNotAvailableException(String message,
      Throwable cause) {
      super(message, cause);
      }
    */

    /*
      public AddressRequestNotAvailableException(String message) {
      super(message);
      }
    */

    /*
      public AddressRequestNotAvailableException(Throwable cause) {
      super(cause);
      }
    */

    public AddressRequestNotAvailableException(String message, String etype,
                                               String [] eparms)
    {
        super(message, etype, eparms);
    }

}
