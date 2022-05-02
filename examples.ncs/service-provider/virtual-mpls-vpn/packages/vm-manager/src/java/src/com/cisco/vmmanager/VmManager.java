/* Author: Johan Bevemyr <jbevemyr@cisco.com> */

package com.cisco.vmmanager;

import com.tailf.navu.*;
import com.tailf.cdb.*;
import com.tailf.maapi.*;
import com.tailf.conf.*;

import java.net.Socket;

import com.tailf.dp.*;
import com.tailf.dp.annotations.*;
import com.tailf.dp.proto.*;
import com.tailf.dp.services.*;

import com.tailf.ncs.*;
import com.tailf.ncs.ns.Ncs;
import com.tailf.ncs.template.Template;
import com.tailf.ncs.template.TemplateVariables;

import com.cisco.vmmanager.namespaces.*;

public class VmManager {

    public static void registerStartRequest(
        ServiceContext context,
        NavuNode service,
        NavuNode root,
        String name) throws ConfException {

        root.getParent().container(vmManager.hash).
            container("vm-manager").
            list("start").
            elem(name).list("allocators").
            sharedCreate(new ConfObjectRef(new ConfPath(service.getKeyPath())));

    }

    public static boolean deviceReady(Cdb cdb, String devName) {
        CdbSession sess = null;
        try {
            sess = cdb.startSession(CdbDBType.CDB_OPERATIONAL);
            ConfBool b =
                (ConfBool)sess.getElem("/devices/device{%s}/vmm:ready",
                                       devName);
            return b.booleanValue();
        }
        catch (Exception e) {
            return false;
        }
        finally {
            try {
                sess.endSession();
            }
            catch (Exception e) {
                ;
            }
        }
    }
}
