package com.example.slowsrv;

import com.example.slowsrv.namespaces.*;
import java.util.List;
import java.util.Properties;
import com.tailf.conf.*;
import com.tailf.navu.*;
import com.tailf.ncs.ns.Ncs;
import com.tailf.dp.*;
import com.tailf.dp.annotations.*;
import com.tailf.dp.proto.*;
import com.tailf.dp.services.*;
import com.tailf.maapi.*;
import java.net.Socket;
import java.net.InetAddress;

public class slowTrans {



    @ActionCallback(callPoint="a1", callType=ActionCBType.INIT)
    public void init(DpActionTrans trans) throws DpCallbackException {
    }


    @ActionCallback(callPoint="a1", callType=ActionCBType.ACTION)
    public ConfXMLParam[] a1(DpActionTrans trans, ConfTag name,
                             ConfObject[] kp, ConfXMLParam[] params)
        throws DpCallbackException {
        try {
            boolean systemSession =
                params[1].getValue().toString().equals("true");
            int sleepSecs = ((ConfInt32)params[0].getValue()).intValue();
            setData(systemSession, sleepSecs);
            return null;

        } catch (Exception e) {
            throw new DpCallbackException("a1 failed", e);
        }
    }

    private void setData(boolean systemSession,int sleepSecs) throws Exception {

        Socket s = new Socket("localhost", Conf.NCS_PORT);
        Maapi maapi = new Maapi(s);
        if (systemSession == true)
            maapi.startUserSession("",
                                   InetAddress.getByName("localhost"),
                                   "system",
                                   new String[] {  },
                                   MaapiUserSessionFlag.PROTO_TCP);
        else
            maapi.startUserSession("admin",
                                   InetAddress.getByName("localhost"),
                                   "example",
                                   new String[] { "admin" },
                                   MaapiUserSessionFlag.PROTO_TCP);
        int th = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ_WRITE);

        maapi.setElem(th, new ConfInt32(sleepSecs), "/extern/sleep-secs");
        maapi.applyTrans(th, false);
        maapi.finishTrans(th);
        s.close();
    }


}
