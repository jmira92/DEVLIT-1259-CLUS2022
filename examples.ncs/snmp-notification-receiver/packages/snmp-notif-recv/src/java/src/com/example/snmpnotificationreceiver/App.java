/*    -*- Java -*-
 *
 *  Copyright 2010 Tail-F Systems AB. All rights reserved.
 *
 *  This software is the confidential and proprietary
 *  information of Tail-F Systems AB.
 *
 *  $Id$
 *
 */
package com.example.snmpnotificationreceiver;

import org.snmp4j.log.Log4jLogFactory;
import org.snmp4j.log.LogFactory;

import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.NcsMain;
import com.tailf.ncs.snmp.snmp4j.NotificationReceiver;

/**
 * This class starts the Snmp-notification-receiver.
 */
public class App implements ApplicationComponent {

    private ExampleHandler handl = null;
    private NotificationReceiver notifRec = null;

    static {
        LogFactory.setLogFactory(new Log4jLogFactory());
    }


    public void run() {
        try {
            notifRec.start();
            synchronized (notifRec) {
                notifRec.wait();
            }
        } catch (Exception e) {
            NcsMain.reportPackageException(this, e);
        }
    }

    public void finish() throws Exception {
        if (notifRec == null) {
            return;
        }
        synchronized (notifRec) {
            notifRec.notifyAll();
        }
        notifRec.stop();
        NotificationReceiver.destroyNotificationReceiver();
    }

    public void init() throws Exception {
        handl = new ExampleHandler();
        notifRec =
            NotificationReceiver.getNotificationReceiver();
        // register example filter
        notifRec.register(handl, null);
    }
}
