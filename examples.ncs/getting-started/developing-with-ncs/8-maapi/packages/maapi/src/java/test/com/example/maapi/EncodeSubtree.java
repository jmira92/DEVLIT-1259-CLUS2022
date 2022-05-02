package com.example.maapi;


import java.net.InetAddress;
import java.net.Socket;
import java.util.List;

import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;

import com.tailf.examples.router.namespaces.router;

import com.tailf.ncs.ns.Ncs;
import com.tailf.conf.ConfXMLParam;

import com.tailf.conf.Conf;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuContext;
import com.tailf.navu.NavuNode;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

public class EncodeSubtree{
    private Socket socket;
    private Maapi  maapi;
    private static Logger log = LogManager.getLogger(SetElem.class);

    public static void main(String[] arg){ new EncodeSubtree();}

    public EncodeSubtree(){
        try{
            socket = new Socket("127.0.0.1", Conf.NCS_PORT);
            maapi = new Maapi(socket);
            maapi.startUserSession("admin",
                                   InetAddress.getByName("localhost"),
                                   "maapi",
                                   new String[] { "admin" },
                                   MaapiUserSessionFlag.PROTO_TCP);

            int th   = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
            NavuContext ctx = new NavuContext(maapi, th);
            NavuList devices =
                new NavuContainer(ctx)
                .container(new Ncs().hash())
                .container(Ncs._devices).list(Ncs._device);

            for(NavuNode device: devices.elements()){
                encodeDeviceTree(device);
            }
        }catch(Exception e){
            log.info("",e);
        }
    }


    private void encodeDeviceTree(NavuNode deviceNode) throws Exception{

        NavuContainer deviceEntry = (NavuContainer) deviceNode;

        NavuContainer routerSys =
            deviceEntry.container(Ncs._config).container(router._sys);

        List<ConfXMLParam> deviceXMLGetParams = routerSys.encodeXML();

        ConfXMLParam[] deviceData = routerSys.getParent()
            .getValues(deviceXMLGetParams.toArray(new ConfXMLParam[0]));

        String XMLResult =
            ConfXMLParam.toXML(deviceData,Ncs._config_, new Ncs().uri());

        log.info("XML PRINT FOR PATH:" + deviceNode.getKeyPath() +
                 "\n" + XMLResult);
    }

}



