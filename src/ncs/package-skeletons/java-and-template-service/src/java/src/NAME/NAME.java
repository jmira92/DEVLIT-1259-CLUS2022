package %JAVAPACKAGE%;

import %JAVAPACKAGE%.namespaces.*;
import java.util.List;
import java.util.Properties;
import com.tailf.conf.*;
import com.tailf.navu.*;
import com.tailf.ncs.ns.Ncs;
import com.tailf.dp.*;
import com.tailf.dp.annotations.*;
import com.tailf.dp.proto.*;
import com.tailf.dp.services.*;
import com.tailf.ncs.template.Template;
import com.tailf.ncs.template.TemplateVariables;

public class %CLASS% {


    /**
     * Create callback method.
     * This method is called when a service instance committed due to a create
     * or update event.
     *
     * This method returns a opaque as a Properties object that can be null.
     * If not null it is stored persistently by Ncs.
     * This object is then delivered as argument to new calls of the create
     * method for this service (fastmap algorithm).
     * This way the user can store and later modify persistent data outside
     * the service model that might be needed.
     *
     * @param context - The current ServiceContext object
     * @param service - The NavuNode references the service node.
     * @param ncsRoot - This NavuNode references the ncs root.
     * @param opaque  - Parameter contains a Properties object.
     *                  This object may be used to transfer
     *                  additional information between consecutive
     *                  calls to the create callback.  It is always
     *                  null in the first call. I.e. when the service
     *                  is first created.
     * @return Properties the returning opaque instance
     * @throws ConfException
     */

    @ServiceCallback(servicePoint="%NAME%-servicepoint",
        callType=ServiceCBType.CREATE)
    public Properties create(ServiceContext context,
                             NavuNode service,
                             NavuNode ncsRoot,
                             Properties opaque)
                             throws ConfException {

        Template myTemplate = new Template(context, "%NAME%-template");
        TemplateVariables myVars = new TemplateVariables();

        try {
            // set a variable to some value
            myVars.putQuoted("DUMMY", "10.0.0.1");
            // apply the template with the variable
            myTemplate.apply(service, myVars);

        } catch (Exception e) {
            throw new DpCallbackException(e.getMessage(), e);
        }
        return opaque;
    }


    /**
     * Init method for selftest action
     */
    @ActionCallback(callPoint="%NAME%-self-test", callType=ActionCBType.INIT)
    public void init(DpActionTrans trans) throws DpCallbackException {
    }

    /**
     * Selftest action implementation for service
     */
    @ActionCallback(callPoint="%NAME%-self-test", callType=ActionCBType.ACTION)
    public ConfXMLParam[] selftest(DpActionTrans trans, ConfTag name,
                                   ConfObject[] kp, ConfXMLParam[] params)
    throws DpCallbackException {
        try {
            // Refer to the service yang model prefix
            String nsPrefix = "%NAME%";
            // Get the service instance key
            String str = ((ConfKey)kp[0]).toString();

          return new ConfXMLParam[] {
              new ConfXMLParamValue(nsPrefix, "success", new ConfBool(true)),
              new ConfXMLParamValue(nsPrefix, "message", new ConfBuf(str))};

        } catch (Exception e) {
            throw new DpCallbackException("self-test failed", e);
        }
    }
}
