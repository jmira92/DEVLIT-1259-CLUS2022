package com.example.vlan;

import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;

import com.example.vlan.namespaces.vlanService;
import com.tailf.cdb.Cdb;
import com.tailf.cdb.CdbDBType;
import com.tailf.cdb.CdbLockType;
import com.tailf.cdb.CdbSession;
import com.tailf.cdb.CdbUpgradeSession;
import com.tailf.conf.*;
import com.tailf.maapi.Maapi;

public class UpgradeService {

    public UpgradeService() {
    }

    public static void main(String[] args) throws Exception {
        Socket s1 = new Socket("localhost", Conf.NCS_PORT);
        Cdb cdb = new Cdb("cdb-upgrade-sock", s1);
        cdb.setUseForCdbUpgrade();
        CdbUpgradeSession cdbsess =
            cdb.startUpgradeSession(
                    CdbDBType.CDB_RUNNING,
                    EnumSet.of(CdbLockType.LOCK_SESSION,
                               CdbLockType.LOCK_WAIT));


        Socket s2 = new Socket("localhost", Conf.NCS_PORT);
        Maapi maapi = new Maapi(s2);
        int th = maapi.attachInit();

        int no = cdbsess.getNumberOfInstances("/services/vlan");
        for(int i = 0; i < no; i++) {
            Integer offset = Integer.valueOf(i);
            ConfBuf name = (ConfBuf)cdbsess.getElem("/services/vlan[%d]/name",
                                                    offset);
            ConfBuf iface = (ConfBuf)cdbsess.getElem("/services/vlan[%d]/iface",
                                                    offset);
            ConfInt32 unit =
                (ConfInt32)cdbsess.getElem("/services/vlan[%d]/unit",
                                           offset);
            ConfUInt16 vid =
                (ConfUInt16)cdbsess.getElem("/services/vlan[%d]/vid",
                                            offset);

            String nameStr = name.toString();
            System.out.println("SERVICENAME = " + nameStr);

            String globId = String.format("%1$s-%2$s-%3$s", iface.toString(),
                                          unit.toString(), vid.toString());
            ConfPath gidpath = new ConfPath("/services/vlan{%s}/global-id",
                                            name.toString());
            maapi.setElem(th, new ConfBuf(globId), gidpath);
        }

        s1.close();
        s2.close();
    }
}
