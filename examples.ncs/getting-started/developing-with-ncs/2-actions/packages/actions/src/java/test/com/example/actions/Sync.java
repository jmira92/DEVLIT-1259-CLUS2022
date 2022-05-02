package com.example.actions;

import java.net.Socket;
import java.net.InetAddress;

import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.conf.*;
import com.tailf.ncs.ns.Ncs;
import org.w3c.dom.Document;
import com.tailf.util.*;

/**
 *
 * The example will show how to invoke already existing
 * actions from Java code. In the
 * CLI we can sync from devices, we can always invoke any action defined
 * in any data model, also from the Java code
 * It also shows how to make debug printouts of the XMLParams arrays
 * that are used as input/output parameters to actions
 */



public class Sync {
    public static void main( String[] args ) throws Exception {

        try {
            Socket maapisock =
                new Socket("localhost", Conf.NCS_PORT);
            Maapi maapi = new Maapi(maapisock);
            maapi.startUserSession("admin",
                                   InetAddress.getByName("localhost"),
                                   "maapi",
                                   new String[] { "admin" },
                                   MaapiUserSessionFlag.PROTO_TCP);

            ConfNamespace ncs = new Ncs();

            ConfXMLParam[] params = new ConfXMLParam[0];
            // ConfXMLParamToXML transf = new ConfXMLParamToXML();
            // Document doc = transf.toXML(params);
            // transf.serialize(doc,System.out);

            String inputParameters =
                ConfXMLParam.toXML(params,"input",
                                   "http://tail-f.com/ns/ncs");
            System.out.println(inputParameters);

            ConfXMLParam[] res =
                maapi.requestAction(params, "/ncs:devices/sync-from");
            // ConfXMLParamToXML transf2 = new ConfXMLParamToXML();
            // Document doc2 = transf2.toXML(res);
            // transf2.serialize(doc2,System.out);

            String outputParameters =
                ConfXMLParam.toXML(res,"output",
                                   "http://tail-f.com/ns/ncs");
            System.out.println(outputParameters);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
