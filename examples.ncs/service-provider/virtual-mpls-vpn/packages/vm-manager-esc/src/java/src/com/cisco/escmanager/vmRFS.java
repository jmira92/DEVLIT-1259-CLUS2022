package com.cisco.escmanager;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import java.util.Properties;
import java.net.InetAddress;
import java.io.IOException;

import java.net.Socket;

import java.util.Properties;
import com.tailf.conf.*;
import com.tailf.cdb.*;
import com.tailf.navu.*;
import com.tailf.ncs.ns.Ncs;
import com.tailf.dp.*;
import com.tailf.dp.annotations.*;
import com.tailf.dp.proto.*;
import com.tailf.dp.services.*;
import java.util.ArrayList;
import com.cisco.resourcemanager.*;
import com.cisco.resourcemanager.namespaces.*;

import com.tailf.ncs.*;
import com.tailf.ncs.ns.Ncs;

import java.util.*;

import java.net.Socket;
import java.net.SocketException;
import java.net.SocketAddress;
import java.net.InetAddress;
import java.text.SimpleDateFormat;

import java.util.Iterator;

import com.tailf.ncs.ResourceManager;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;

import com.tailf.ncs.ns.Ncs;
import com.tailf.ncs.template.Template;
import com.tailf.ncs.template.TemplateVariables;

import com.cisco.escmanager.namespaces.*;

import com.tailf.ncs.ns.Ncs;
import com.tailf.ncs.template.Template;
import com.tailf.ncs.template.TemplateVariables;

public class vmRFS {

    @Resource(type=ResourceType.CDB, scope=Scope.CONTEXT, qualifier="reactive")
    public  Cdb cdb;
    public  CdbSession cdbSess;

    private static Logger LOGGER = LogManager.getLogger(vmRFS.class);

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
     * @throws DpCallbackException
     */

    @ServiceCallback(servicePoint="vm-esc-servicepoint",
                     callType=ServiceCBType.CREATE)
    public Properties create(ServiceContext context,
                             NavuNode vm,
                             NavuNode ncsRoot,
                             Properties opaque)
                             throws DpCallbackException {

        String vmPath = null;
        LOGGER.info("Running vm start service");

        try {
            vmPath = vm.getKeyPath();

            String name           = vm.leaf("name").valueAsString();
            String deploymentName = vm.leaf("deployment-name").valueAsString();
            String vmDevice       = vm.leaf("vm-device").valueAsString();
            String tenant         = vm.leaf("tenant").valueAsString();
            String serviceName    = vm.leaf("service-name").valueAsString();
            String serviceVersion = vm.leaf("service-version").valueAsString();
            String vmType         = vm.leaf("vm-type").valueAsString();
            String vmGroup        = vm.leaf("vm-group").valueAsString();
            String mgmtIpAddress  = vm.leaf("mgmt-ip-address").valueAsString();
            String day0Url        = vm.leaf("day0-url").valueAsString();
            String scalingMin     = vm.leaf("scaling-min").valueAsString();
            String scalingMax     = vm.leaf("scaling-max").valueAsString();

            updateVmProgress(name, "defined");

            TemplateVariables variables = new TemplateVariables();

            Template template = new Template(context, "vm-manager-esc-tenant");

            variables.putQuoted("ESC", vmDevice);
            variables.putQuoted("TENANT", tenant);
            variables.putQuoted("DEPNAME", deploymentName);
            variables.putQuoted("SERVICE_NAME", serviceName);
            variables.putQuoted("SERVICE_VERSION", serviceVersion);
            variables.putQuoted("VM_GROUP", vmGroup);
            variables.putQuoted("DAY0_URL", day0Url);
            variables.putQuoted("SCALING_MIN", scalingMin);
            variables.putQuoted("SCALING_MAX", scalingMax);

            if ("csr".equals(vmType)) {
                variables.putQuoted("DAY0_FILENAME", "iosxe_config.txt");
            } else {
                LOGGER.error("unknown device type: "+vmType);
            }

            template.apply(vm, variables);

            template = new Template(context, "vm-manager-esc-scaling");

            for(NavuContainer pool:  vm.list("scaling-pool")) {
                variables.putQuoted("SCALING_POOL",
                                    pool.leaf("name").valueAsString());

                for(NavuContainer address: pool.list("address")) {
                    variables.putQuoted("IP_ADDRESS",
                                        address.leaf("ip").valueAsString());
                    template.apply(vm, variables);
                }
            }

            template = new Template(context, "vm-manager-esc-interface");

            for(NavuContainer iface: vm.list("interface")) {
                variables.putQuoted("INTERFACE_ID",
                                    iface.leaf("id").valueAsString());
                variables.putQuoted("NETWORK",
                                    iface.leaf("name").valueAsString());
                variables.putQuoted("INTERFACE_IP_ADDRESS",
                                    iface.leaf("ip").valueAsString());
                template.apply(vm, variables);
            }

            updateVmProgress(name, "esc configured to start vm");

            LOGGER.info("Applied esc template to "+ vmDevice);

        } catch (Exception e) {
            LOGGER.info("", e);
            throw new DpCallbackException("Cannot create service " +
                                          vm, e);
        }

        return opaque;
    }

    public static void updateVmProgress(String name, String state) {

        Socket socket = null;
        Cdb cdb = null;
        CdbSession wsess = null;

        try {
            socket = new Socket(NcsMain.getInstance().getNcsHost(),
                                NcsMain.getInstance().getNcsPort());

            try {
                cdb = new Cdb("OperWriter", socket);
                wsess = cdb.startSession(CdbDBType.CDB_OPERATIONAL);

                state = state.replaceAll(" ", "-");

                String dPath = "/vmm:vm-manager/start{"+name+"}";

                String lPath = dPath+"/log{"+state+"}";

                if (!wsess.exists(lPath)) {
                    wsess.create(lPath);
                    String timestamp =
                        new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").
                        format(new Date());

                    wsess.setElem(new ConfBuf(timestamp),
                                  new ConfPath(lPath+"/timestamp"));
                }
            } finally {
                try {
                    socket.close();
                } catch (Exception e) {
                    ;
                }
            }
        } catch (Exception e) {
            LOGGER.error("", e);
        }
    }

}
