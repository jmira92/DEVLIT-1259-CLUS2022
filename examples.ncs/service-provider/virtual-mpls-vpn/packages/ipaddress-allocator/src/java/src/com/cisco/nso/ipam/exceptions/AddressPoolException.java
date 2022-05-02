package com.cisco.nso.ipam.exceptions;

public class AddressPoolException extends Exception
    implements I18NableException {

    // Message types, copied from AppResources (because Oscar does
    // not depend on AppResources)
    private static final String ERROR_STR           = "error.";
    private static final String SERVER_STR  = "server.";
    private static final String VALIDATION_STR              = "validation.";
    private static final String LIMIT_STR                   = "limit.";
    private static final String CONFLICT_STR                = "conflict.";
    private static final String INVALID_STR                 = "invalid.";
    public static final String ERROR_SERVER_VALIDATION =
        ERROR_STR + SERVER_STR + VALIDATION_STR;
    public static final String ERROR_SERVER_VALIDATION_CONFLICT =
        ERROR_SERVER_VALIDATION + CONFLICT_STR;
    public static final String ERROR_SERVER_VALIDATION_LIMIT =
        ERROR_SERVER_VALIDATION + LIMIT_STR;
    private static final String INVALID_PARAMETER =
        INVALID_STR + "parameter.";
    public static final String ERROR_SERVER_VALIDATION_INVALID_PARAMETER =
        ERROR_SERVER_VALIDATION + INVALID_PARAMETER;
    private static final String INVALID_NOT_ALLOWED =
        INVALID_STR + "notAllowed.";
    public static final String ERROR_SERVER_VALIDATION_INVALID_NOT_ALLOWED =
        ERROR_SERVER_VALIDATION + INVALID_NOT_ALLOWED;


    public static final String
        MSG_ADDRESSPOOL_REQUESTED_ADDRESS_NOT_AVAILABLE_FROM_POOL =
        ERROR_SERVER_VALIDATION_CONFLICT +
        "address.requested.not.available.from.pool";
    public static final String
        MSG_ADDRESSPOOL_SUBNET_ALREADY_RESERVED =
        ERROR_SERVER_VALIDATION_CONFLICT +
        "addresspool.subnet.already.reserved";
    public static final String
        MSG_ADDRESSPOOL_ERROR_ALLOCATING_HOST_ADDRESS =
        ERROR_SERVER_VALIDATION_CONFLICT +
        "addresspool.error.allocating.host.address";

    public static final String MSG_ADDRESSPOOL_EMPTY =
        ERROR_SERVER_VALIDATION_LIMIT + "addresspool.empty";

    public static final String MSG_ADDRESSPOOL_ADDRESS_NOT_ALLOCATED =
        ERROR_SERVER_VALIDATION_INVALID_PARAMETER +
        "addresspool.address.not.allocated";
    public static final String
        MSG_ADDRESSPOOL_REQUESTED_ADDRESS_NOT_DEFINED_BY_POOL =
        ERROR_SERVER_VALIDATION_INVALID_PARAMETER +
        "addresspool.requested.address.not.defined.by.pool";
    public static final String MSG_ADDRESSPOOL_INVALID_ADDRESS_POOL_ID =
        ERROR_SERVER_VALIDATION_INVALID_PARAMETER +
        "addresspool.address.pool.id";
    public static final String MSG_ADDRESSPOOL_MASK_INVALID =
        ERROR_SERVER_VALIDATION_INVALID_PARAMETER + "addresspool.mask.invalid";

    public static final String MSG_ADDRESSPOOL_COULD_NOT_ALLOCATE_SUBNET =
        ERROR_SERVER_VALIDATION_INVALID_NOT_ALLOWED +
        "addresspool.could.not.allocate.subnet";
    public static final String MSG_ADDRESSPOOL_CANNOT_ALLOCATE_SUBNET =
        ERROR_SERVER_VALIDATION_INVALID_NOT_ALLOWED +
        "addresspool.cannot.allocate.subnet";

    private static final long serialVersionUID = -7644180480959376634L;
    protected String etype = null;
    protected String [] eparms = null;
    protected String originalMessage = null;

    /*
      public AddressPoolException() {
      // TODO Auto-generated constructor stub
      }
    */

    /*
      public AddressPoolException(String message) {
      super(message);
      // TODO Auto-generated constructor stub
      }
    */

    public AddressPoolException(Throwable cause) {
        super(cause instanceof I18NableException ?
              (((I18NableException) cause).getOriginalMessage() == null ?
               "" : ((I18NableException) cause).getOriginalMessage())
              :       "",
              cause);
        if (cause instanceof I18NableException) {
            I18NableException theCause = (I18NableException) cause;
            this.setEtype(theCause.getEtype());
            this.setEparms(theCause.getEparms());
            this.setOriginalMessage(theCause.getOriginalMessage());
        }
    }

    /*
      public AddressPoolException(String message, Throwable cause) {
      super(message, cause);
      // TODO Auto-generated constructor stub
      }
    */

    public AddressPoolException(String message, String etype,
                                String [] eparms) {
        super(message);
        this.setEtype(etype);
        this.setEparms(eparms);
        this.setOriginalMessage(message);
    }

    public AddressPoolException(String message, String etype, String [] eparms,
                                Throwable cause) {
        super(message, cause);
        this.setEtype(etype);
        this.setEparms(eparms);
        this.setOriginalMessage(message);
    }

    public void setEtype(String etype) {
        this.etype = etype;
    }

    public String getEtype() {
        return this.etype;
    }

    public void setEparms(String [] parms) {
        this.eparms = parms;
    }

    public void setEparms (String parm) {
        this.eparms = new String[] {parm};
    }

    public String [] getEparms() {
        return this.eparms;
    }

    public void setOriginalMessage(String msg) {
        this.originalMessage = msg;
    }

    public String getOriginalMessage() {
        return this.originalMessage;
    }

    // tack on a cause to super.toString(), if any
    public String toString() {
        Throwable t = getCause();
        String str = super.toString();
        if (t!=null && t!=this)
            str += t.toString();
        return str;
    }
}
