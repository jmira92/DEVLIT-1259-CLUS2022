package com.example.l3vpn;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.cisco.resourcemanager.ResourceAllocator;
import com.cisco.resourcemanager.ResourceWaitException;
import com.cisco.vmmanager.VmManager;
import com.example.l3vpn.namespaces.l3vpn;
import com.example.weblog.WebUILogger;
import com.tailf.cdb.Cdb;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfIPPrefix;
import com.tailf.conf.ConfIPv4AndPrefixLen;
import com.tailf.conf.ConfObject;
import com.tailf.conf.ConfUInt32;
import com.tailf.conf.ConfValue;
import com.tailf.dp.DpCallbackException;
import com.tailf.dp.annotations.ServiceCallback;
import com.tailf.dp.proto.ServiceCBType;
import com.tailf.dp.services.ServiceContext;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuLeaf;
import com.tailf.navu.NavuLeafList;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.PlanComponent;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;
import com.tailf.ncs.template.Template;
import com.tailf.ncs.template.TemplateVariables;

public class l3vpnRFS {
  private static Logger LOGGER = LogManager.getLogger(l3vpnRFS.class);

  @Resource(type = ResourceType.CDB, scope = Scope.INSTANCE,
            qualifier = "l3vpn-cdb")
  private Cdb cdb;


  /**
   * Create callback method. This method is called when a service instance
   * committed due to a create or update event.
   *
   * This method returns a opaque as a Properties object that can be null.
   * If not null it is stored persistently by Ncs.
   * This object is then delivered as
   * argument to new calls of the create method for this service (fastmap
   * algorithm). This way the user can store and later modify persistent data
   * outside the service model that might be needed.
   *
   * @param context
   *          - The current ServiceContext object
   * @param service
   *          - The NavuNode references the service node.
   * @param ncsRoot
   *          - This NavuNode references the ncs root.
   * @param opaque
   *          - Parameter contains a Properties object. This object may be used
   *          to transfer additional information between consecutive calls to
   *          the create callback. It is always null in the first call. I.e.
   *          when the service is first created.
   * @return Properties the returning opaque instance
   * @throws ConfException
   */


  @ServiceCallback(servicePoint = "l3vpn-servicepoint",
                   callType = ServiceCBType.CREATE)
  public Properties create(ServiceContext context,
                           NavuNode service,
                           NavuNode ncsRoot, Properties opaque)
    throws ConfException
  {
    WebUILogger.log(LOGGER, "***** Create/Reactive-re-deploy ************");

    Properties rfmProgress = new Properties();


    try {
      // check if it is reasonable to assume that devices
      // initially has been sync-from:ed
      NavuList managedDevices = ncsRoot.container("devices").list("device");

      for (NavuContainer device : managedDevices) {
        if (device.list("capability").isEmpty()) {
          String mess = "Device %1$s has no known capabilities, "
            + "has sync-from been performed?";
          String key = device.getKey().elementAt(0).toString();

          throw new DpCallbackException(String.format(mess, key));
        }
      }
    } catch (DpCallbackException e) {
      throw e;
    } catch (Exception e) {
      throw new DpCallbackException("Not able to check devices", e);
    }

    // Contains list of CE routers that reside on the virtual PE
    List<String> CEonVPE = getCEonVPE(ncsRoot);

    Template peTemplate = new Template(context, "l3vpn-pe");
    Template ceTemplate = new Template(context, "l3vpn-ce");

    NavuList endpoints = service.list(l3vpn._endpoint);
    NavuContainer topology = ncsRoot.getParent().container(l3vpn.uri)
      .container(l3vpn._topology);


    StringBuffer stb = new StringBuffer();
    for (NavuContainer endpoint : endpoints.elements()) {
       if (stb.length() > 0) {
           stb.append(",");
       }
       stb.append(endpoint.leaf(l3vpn._id).valueAsString());
    }
    rfmProgress.setProperty("endpoints", stb.toString());

    String tenant = service.leaf(l3vpn._name).valueAsString();
    String deploymentName = "vpn";

    String virtualPEName =
            Helper.makeDevName(tenant, deploymentName, "CSR", "esc0");

    for (NavuContainer endpoint : endpoints.elements()) {
      try {
        String endpointId = endpoint.leaf(l3vpn._id).valueAsString();

        String ceName = endpoint.leaf(l3vpn._ce_device).valueAsString();

        if (CEonVPE.contains(ceName)) {
          rfmProgress.setProperty(endpointId + ".ONVPE", "true");


          if (!createVirtualPE(context, service, ncsRoot, ceName, tenant,
                               deploymentName)) {
            // We cannot continue with this CE until redeploy
            continue;
          }
          rfmProgress.setProperty(endpointId + ".pe-created", "DONE");

          LOGGER.info("device ready, continue: " + virtualPEName);

          addToTopologyRole(ncsRoot, virtualPEName, "pe");

          // Add CE-VPE topology, reuse old topology connection if available
          NavuContainer conn = getConnection(
            topology, endpoint.leaf(l3vpn._ce_device).valueAsString(), "pe");
          if (!addToTopology(service, ncsRoot, conn,
                ceName, "GigabitEthernet0/8",
                virtualPEName, "GigabitEthernet2",
                ceName, Settings.ipPoolPE_CE)) {
            // We cannot continue with this CE until redeploy
            continue;
          }
          rfmProgress.setProperty(endpointId + ".ce-vpe-topo-added", "DONE");


          // Add VPE-P3 topology
          if (!addToTopology(service, ncsRoot, null,
                "p3", "GigabitEthernet0/8",
                virtualPEName, "GigabitEthernet1",
                "p3" + "_" + virtualPEName, Settings.ipPoolP_PE)) {
            // We cannot continue with this CE until redeploy
            continue;
          }
          rfmProgress.setProperty(endpointId + ".vpe-p0-topo-added", "DONE");

        }
        // Get the PE connection for this endpoint router
        NavuContainer conn = getConnection(
          topology, endpoint.leaf(l3vpn._ce_device).valueAsString(), "pe");
        NavuContainer peEndpoint = getConnectedEndpoint(conn, ceName);
        NavuContainer ceEndpoint = getMyEndpoint(conn, ceName);
        NavuLeaf vlan = conn.leaf("link-vlan");

        TemplateVariables vpnVar = new TemplateVariables();

        vpnVar.putQuoted("PE", peEndpoint.leaf("device").valueAsString());
        vpnVar.putQuoted("CE", endpoint.leaf("ce-device").valueAsString());
        vpnVar.putQuoted("VLAN_ID", vlan.valueAsString());
        vpnVar.putQuoted("LINK_PE_ADR",
                         getIPAddress(peEndpoint.leaf(
                                        "ip-address").valueAsString()));
        vpnVar.putQuoted("LINK_CE_ADR",
                         getIPAddress(ceEndpoint.leaf(
                                        "ip-address").valueAsString()));
        vpnVar.putQuoted("LINK_MASK", getNetMask(ceEndpoint.leaf("ip-address")
                                                 .valueAsString()));
        vpnVar.putQuoted("LINK_PREFIX",
                         getIPPrefix(ceEndpoint.
                                     leaf("ip-address").valueAsString()));
        vpnVar.putQuoted("PE_INT_NAME", peEndpoint.leaf("interface")
                         .valueAsString());
        vpnVar.putQuoted("CE_INT_NAME", ceEndpoint.leaf("interface")
                         .valueAsString());
        vpnVar.putQuoted("CE_LOCAL_INT_NAME", endpoint.leaf("ce-interface")
                         .valueAsString());
        vpnVar.putQuoted("LOCAL_CE_ADR",
                         getIPAddress(
                           getNextIPV4Address(endpoint.leaf("ip-network")
                                              .valueAsString())));
        vpnVar.putQuoted("LOCAL_CE_NET",
                         getIPAddress(endpoint.leaf("ip-network").
                                      valueAsString()));
        vpnVar.putQuoted("CE_MASK", getNetMask(endpoint.leaf("ip-network")
                                               .valueAsString()));
        vpnVar.putQuoted("BW", endpoint.leaf("bandwidth").valueAsString());

        peTemplate.apply(service, vpnVar);
        ceTemplate.apply(service, vpnVar);

        configureQoS(context, service, endpoint, ceEndpoint, peEndpoint, vlan);
        rfmProgress.setProperty(endpointId + ".qos-configured", "DONE");

        LOGGER.info("done");

      } catch (Exception e) {
        throw new DpCallbackException(e.getMessage(), e);
      }
    }

    writePlanData(service, rfmProgress);

    return opaque;
  }

  private NavuContainer getConnectedEndpoint(NavuContainer conn,
                                             String deviceName)
    throws NavuException
  {
    if (deviceName.equals(conn.container("endpoint-1").leaf("device")
                          .valueAsString()))
      return conn.container("endpoint-2");
    else
      return conn.container("endpoint-1");
  }

  private NavuContainer getMyEndpoint(NavuContainer conn, String deviceName)
    throws NavuException
  {
    if (deviceName.equals(conn.container("endpoint-1").leaf("device")
                          .valueAsString()))
      return conn.container("endpoint-1");
    else
      return conn.container("endpoint-2");
  }

  /**
   * Adds the device to the /topology/role{roleName}/device list
   *
   * @param root
   *          // NCS root
   * @param deviceName
   *          // e.g. volvo_vpn_CSR0
   * @param roleName
   *          // e.g. pe
   * @throws NavuException
   */
  private void addToTopologyRole(NavuNode root, String deviceName,
                                 String roleName)
    throws NavuException
  {
    NavuContainer topology = root.getParent().container(l3vpn.uri)
      .container(l3vpn._topology);
    NavuLeafList devicesLeafList = topology.list(l3vpn._role).elem(roleName)
      .leafList(l3vpn._device);
    devicesLeafList.sharedCreate(new ConfBuf(deviceName));
  }

  private boolean addToTopology(NavuNode service, NavuNode root,
                                NavuContainer connection,
                                String endPoint1, String ep1If,
                                String endPoint2, String ep2If,
                                String connectionName,
                                String ipPoolName)
    throws Exception
  {
    // We need an IP address for the devices
    final int prefixLength = 30;

    ResourceAllocator.subnetRequest(service, root, ipPoolName,
            prefixLength, connectionName);

    ResourceAllocator.idRequest(service, root, Settings.idPoolName,
                                connectionName);

    try {
      NavuContainer topology = root.getParent().container(l3vpn.uri)
        .container(l3vpn._topology);

      NavuList connections = topology.list(l3vpn._connection);

      if (null == connection) {
        LOGGER.info("Creating new topology connection");
        connection = connections.sharedCreate(connectionName);
        NavuLeaf link_vlan = connection.leaf(l3vpn._link_vlan);
        ConfUInt32 vlanID =
            ResourceAllocator.idRead(service.context().getMaapi(),
                                     Settings.idPoolName,
                                     connectionName);
        link_vlan.sharedSet(vlanID);
      } else
        LOGGER.info("Reusing existing topology connection");

      NavuContainer ep1 = connection.container(l3vpn._endpoint_1);
      ConfIPPrefix net =
          ResourceAllocator.subnetRead(service.context().getMaapi(),
                                       ipPoolName,
                                       connectionName);

      InetAddress ep1_addr = Helper.increment(net.getAddress());
      configureEndpoint(ep1, endPoint1, ep1If, ep1_addr,
                        24);

      NavuContainer ep2 = connection.container(l3vpn._endpoint_2);
      InetAddress ep2_addr = Helper.increment(ep1_addr);
      configureEndpoint(ep2, endPoint2, ep2If, ep2_addr,
                        24);

      WebUILogger.log(
        LOGGER,
        String.format("%s address %s, %s address %s", endPoint1,
                      ep1_addr.toString(), endPoint2, ep2_addr.toString()));

    } catch (ResourceWaitException e) {
      WebUILogger.logRedeploy(LOGGER, connectionName + " network and VLAN");
      // done, wait for reactive-re-deploy
      return false;
    } catch (Exception e) {
      LOGGER.error(e.getMessage(), e);
      throw e;
    }
    return true;
  }

  private void configureEndpoint(NavuContainer ep, String device,
                                 String networkInterface, InetAddress addr,
                                 int prefixLen)
    throws NavuException
  {

    ep.leaf(l3vpn._device).sharedSet(new ConfBuf(device));
    ep.leaf(l3vpn._interface).sharedSet(new ConfBuf(networkInterface));
    ep.leaf(l3vpn._ip_address).sharedSet(
      new ConfIPv4AndPrefixLen(addr, prefixLen));
  }

  private NavuContainer getConnection(NavuContainer topology,
                                      String deviceName,
                                      String remoteDeviceRole)
    throws NavuException
  {

    NavuList connections = topology.list(l3vpn._connection);

    for (NavuContainer conn : connections.elements()) {
      String remoteName;

      if (deviceName.equals(conn.container(l3vpn._endpoint_1)
                            .leaf(l3vpn._device).valueAsString()))
        remoteName = conn.container(l3vpn._endpoint_2).leaf(l3vpn._device)
          .valueAsString();
      else if (deviceName.equals(conn.container("endpoint-2").leaf("device")
                                   .valueAsString()))
        remoteName = conn.container(l3vpn._endpoint_1).leaf(l3vpn._device)
          .valueAsString();
      else
        continue;

      NavuLeafList devicesWithRole = topology.list(l3vpn._role)
        .elem(remoteDeviceRole).leafList(l3vpn._device);

      if (devicesWithRole.containsNode(new ConfBuf(remoteName)))
        return conn;
    }
    return null;
  }

  private String getIPAddress(String prefix) {
    String[] parts = prefix.split("/");
    return parts[0];
  }

  private String getIPPrefix(String prefix) {
    String[] parts = prefix.split("/");
    return parts[1];
  }

  private String getNetMask(String addr) throws UnknownHostException {
    String[] parts = addr.split("/");
    int prefix;

    if (parts.length < 2) {
      prefix = 0;
    } else {
      prefix = Integer.parseInt(parts[1]);
    }
    int mask = 0xffffffff << (32 - prefix);

    int value = mask;
    byte[] bytes = new byte[] { (byte) (value >>> 24),
                                (byte) (value >> 16 & 0xff),
                                (byte) (value >> 8 & 0xff),
                                (byte) (value & 0xff) };

    InetAddress netAddr = InetAddress.getByAddress(bytes);
    return netAddr.getHostAddress();
  }

  private String getNextIPV4Address(String ip) {
    String ipAddr = ip.split("/")[0];
    String mask = ip.split("/")[1];

    String[] nums = ipAddr.split("\\.");
    int i = (Integer.parseInt(nums[0]) << 24 |
             Integer.parseInt(nums[2]) << 8 |
             Integer.parseInt(nums[1]) << 16 |
             Integer.parseInt(nums[3])) + 1;

    // If you wish to skip over .255 addresses.
    if ((byte) i == -1)
      i++;

    return String.format("%d.%d.%d.%d",
                         i >>> 24 & 0xFF,
                         i >> 16 & 0xFF,
                         i >> 8 & 0xFF,
                         i >> 0 & 0xFF) + "/" + mask;
  }

  private boolean createVirtualPE(ServiceContext context, NavuNode service,
                                  NavuNode ncsRoot, String ceName,
                                  String tenant, String deploymentName)
    throws UnknownHostException, IOException, ConfException {
    Maapi m = null;
    try {

      LOGGER.info(ceName + " should be on the virtual PE");

      Socket socket =
        new Socket(service.context().getMaapi().getSocket().getInetAddress(),
                   service.context().getMaapi().getSocket().getPort());
      m = new Maapi(socket);
      m.startUserSession("admin",
                         m.getSocket().getInetAddress(), "system",
                         new String[] { "admin" },
                         MaapiUserSessionFlag.PROTO_TCP);

      // Hardcoded
      String virtualPEName =
              Helper.makeDevName(tenant, deploymentName, "CSR", "esc0");
      WebUILogger.log(LOGGER, "Checking " + virtualPEName + " status");

      Template escTenantTemplate = new Template(context, "l3vpn-vm-manager");
      ESCParameters vmParams = getEscVM(service);
      TemplateVariables vars = new TemplateVariables();
      String vmName = tenant+"_"+deploymentName+"_"+vmParams.vm_group;
      vars.putQuoted("NAME", vmName);
      vars.putQuoted("ESC", "esc0"); // TODO: Hardcoded
      vars.putQuoted("TENANT", tenant);
      vars.putQuoted("DEPNAME", deploymentName);
      vars.putQuoted("SERVICE_NAME", vmParams.service_name);
      vars.putQuoted("SERVICE_VERSION", vmParams.service_version);
      vars.putQuoted("VM_GROUP", vmParams.vm_group);

      escTenantTemplate.apply(service, vars);

      VmManager.registerStartRequest(context, service, ncsRoot, vmName);

      WebUILogger.log(LOGGER, "Starting vm with parameters: " + vmParams);

      return VmManager.deviceReady(cdb, virtualPEName);

    } finally {
      Helper.safeclose(m);
    }
  }

  public static ESCParameters getEscVM(NavuNode service) {
    try {
      NavuContainer esc_service = service.container(l3vpn._esc_service);
      if (esc_service.exists()) {
        return new ESCParameters(
          esc_service.leaf(l3vpn._name).valueAsString(),
          esc_service.leaf(l3vpn._version).valueAsString(),
          esc_service.leaf(l3vpn._vm_group).valueAsString());

      }
    } catch (Exception e) {
      LOGGER.error(e.getMessage(), e);
    }

    LOGGER.info("Using default VM settings");
    return new ESCParameters();
  }

  public static List<String> getCEonVPE(NavuNode ncsRoot) throws NavuException {
    ArrayList<String> CEonVPE = new ArrayList<String>();
    NavuLeafList onvpe = ncsRoot.getParent().container(l3vpn.uri)
      .leafList(l3vpn._onvpe);

    for (ConfValue o : onvpe) {
      CEonVPE.add(o.toString());
    }
    return CEonVPE;
  }

  private void configureQoS(ServiceContext context, NavuNode service,
                            NavuContainer endpoint, NavuContainer ceEndpoint,
                            NavuContainer peEndpoint, NavuLeaf vlan)
    throws ConfException, UnknownHostException
  {
    LOGGER.info("configuring QoS");

    if (service.container("qos").leaf("qos-policy").exists()) {
      Map<String, List<String>> qosClassMap =
        new HashMap<String, List<String>>();

      TemplateVariables qosVar = new TemplateVariables();
      qosVar.putQuoted("POLICY_NAME",
                       service.container("qos").leaf("qos-policy").
                       valueAsString());
      qosVar.putQuoted("CE_INT_NAME", ceEndpoint.leaf("interface")
                       .valueAsString());
      qosVar.putQuoted("PE_INT_NAME", peEndpoint.leaf("interface")
                       .valueAsString());
      qosVar.putQuoted("VLAN_ID", vlan.valueAsString());
      qosVar.putQuoted("PE", peEndpoint.leaf("device").valueAsString());
      qosVar.putQuoted("CE", endpoint.leaf("ce-device").valueAsString());

      // Find the globally defined QOS policy our service is
      // referring to.
      NavuNode n = service.container("qos").leaf("qos-policy").deref().get(0);
      NavuContainer qosPolicy = (NavuContainer) ((NavuLeaf) n).getParent();
      NavuList policyClass = qosPolicy.list("class");

      // Iterate over all classes for this policy and its
      // settings.
      Template aclTemplate = new Template(context, "l3vpn-acl");
      int classCounter = 0;
      for (NavuContainer c : policyClass.elements()) {
        NavuNode qosClass = c.leaf("qos-class").deref().get(0);

        qosClassMap.put(c.leaf("qos-class").valueAsString(),
                        new ArrayList<String>());
        NavuContainer cl = (NavuContainer) ((NavuLeaf) qosClass).getParent();

        if (cl.leaf("dscp-value").exists()) {
          qosVar.putQuoted("CLASS_DSCP",cl.leaf("dscp-value").
                  valueAsString());
          if (cl.leaf("dscp-value").valueAsString().
                  equals("ef") ||
                  cl.leaf("dscp-value").valueAsString().
                          equals("af31")) {
            qosVar.putQuoted("CLASS_PRIORITY","high");
          }
          else {
            qosVar.putQuoted("CLASS_PRIORITY","low");
          }
        }
        else {
          qosVar.putQuoted("CLASS_PRIORITY","low");
          qosVar.putQuoted("CLASS_DSCP", "");
        }

        qosVar.putQuoted("CLASS_NAME", c.leaf("qos-class").valueAsString());
        qosVar.putQuoted("CLASS_BW", c.leaf("bandwidth-percentage")
                         .valueAsString());
        qosVar.putQuoted("CLASS_COUNTER",String
                .valueOf(classCounter));

        if (c.leaf("priority").exists()) {
          Template qosPrioTemplate = new Template(context, "l3vpn-qos-prio");
          qosPrioTemplate.apply(service, qosVar);
          Template qosPePrioTemplate = new Template(context,
                                                    "l3vpn-qos-pe-prio");
          qosPePrioTemplate.apply(service, qosVar);

        } else {
          Template qosTemplate = new Template(context, "l3vpn-qos");
          qosTemplate.apply(service, qosVar);
          Template qosPeTemplate = new Template(context, "l3vpn-qos-pe");
          qosPeTemplate.apply(service, qosVar);
        }
        Template peClassTemplate = new Template(context,
                                                "l3vpn-qos-pe-class");
        peClassTemplate.apply(service, qosVar);

        // Also list all the globally defined traffic match
        // statements for this class and add them to a arraylist
        // to use for processing.
        for (NavuContainer match : cl.list("match-traffic").elements()) {
          qosClassMap.get(c.leaf("qos-class").valueAsString()).add(
            "GLOBAL-" + match.leaf("name").valueAsString());

          TemplateVariables aclVar = setAclVars(match, "GLOBAL");
          aclVar.putQuoted("CE", endpoint.leaf("ce-device").valueAsString());
          aclTemplate.apply(service, aclVar);
        }

        classCounter++;
      }

      // Create ACL entries for all service specific match rules

      NavuList matchRules = service.container("qos").list("custom-qos-match");

      for (NavuContainer match : matchRules.elements()) {
        String namePrefix = service.leaf("name").valueAsString();
        if (qosClassMap.containsKey(match.leaf("qos-class").valueAsString())) {
          qosClassMap.get(match.leaf("qos-class").valueAsString()).add(
            namePrefix + "-" + match.leaf("name").valueAsString());
        }
        TemplateVariables aclVar = setAclVars(match, namePrefix);

        aclVar.putQuoted("CE", endpoint.leaf("ce-device").valueAsString());
        aclTemplate.apply(service, aclVar);
      }

      for (Map.Entry<String, List<String>> entry : qosClassMap.entrySet()) {
        for (String matchEntry : entry.getValue()) {
          TemplateVariables classVar = new TemplateVariables();
          classVar.putQuoted("CLASS_NAME", entry.getKey());
          classVar.putQuoted("MATCH_ENTRY", matchEntry);
          classVar.putQuoted("CE", endpoint.leaf("ce-device").valueAsString());
          Template classTemplate = new Template(context, "l3vpn-qos-class");
          classTemplate.apply(service, classVar);
        }
      }
    }

  }

  private TemplateVariables setAclVars(NavuContainer match, String namePrefix)
    throws NavuException, UnknownHostException {
    TemplateVariables aclVar = new TemplateVariables();

    aclVar.putQuoted("ACL_NAME", namePrefix + "-" +
                     match.leaf("name").valueAsString());
    aclVar.putQuoted("PROTOCOL", match.leaf("protocol").valueAsString());
    aclVar.putQuoted("SOURCE_IP", match.leaf("source-ip").valueAsString());

    if ("any".equals(match.leaf("source-ip").valueAsString())) {
      aclVar.putQuoted("SOURCE_IP_ADR", "any");
      aclVar.putQuoted("SOURCE_WMASK", " ");
    } else {
      aclVar.putQuoted("SOURCE_IP_ADR", getIPAddress(match.leaf("source-ip")
                                                     .valueAsString()));
      aclVar.putQuoted("SOURCE_WMASK", prefixToWildcardMask(
                         getIPPrefix(match.leaf("source-ip").valueAsString())));
    }

    if ("any".equals(match.leaf("destination-ip").valueAsString())) {
      aclVar.putQuoted("DEST_IP_ADR", "any");
      aclVar.putQuoted("DEST_WMASK", " ");
    } else {
      aclVar.putQuoted("DEST_IP_ADR", getIPAddress(match.leaf("destination-ip")
                                                   .valueAsString()));
      aclVar.putQuoted("DEST_WMASK", prefixToWildcardMask(
                         getIPPrefix(match.leaf("destination-ip").
                                 valueAsString())));
    }

    aclVar.putQuoted("PORT_START", match.leaf("port-start").valueAsString());
    aclVar.putQuoted("PORT_END", match.leaf("port-end").valueAsString());
    return aclVar;
  }

  private String prefixToWildcardMask(String pre) throws UnknownHostException {
    int prefix = Integer.parseInt(pre);
    int mask = 0xffffffff << (32 - prefix);
    int value = mask;
    byte[] bytes = new byte[] { (byte) (~(value >>> 24) & 0xFF),
                                (byte) (~(value >> 16 & 0xff) & 0xFF),
                                (byte) (~(value >> 8 & 0xff) & 0xFF),
                                (byte) (~(value & 0xff) & 0xFF) };

    InetAddress netAddr = InetAddress.getByAddress(bytes);
    return netAddr.getHostAddress();
  }


  private void writePlanData(NavuNode service, Properties rfmProgress)
      throws NavuException {
      try {

          PlanComponent self = new PlanComponent(service, "self", "ncs:self");
          // Initial plan
          self.appendState("ncs:init").
               appendState("ncs:ready");
          self.setReached("ncs:init");

          String eps = rfmProgress.getProperty("endpoints");
          String ep[] = eps.split(",");

          boolean ready = true;
          for (String p : ep) {
              boolean onvpe = false;
              if (rfmProgress.containsKey(p + ".ONVPE")) {
                  onvpe = true;
              }
              PlanComponent pcomp = new PlanComponent(service,
                                                      "endpoint-" + p,
                                                      "l3vpn:l3vpn");
              // Initial plan
              pcomp.appendState("ncs:init");
              pcomp.setReached("ncs:init");
              if (onvpe) {
                  pcomp.appendState("l3vpn:pe-created").
                  appendState("l3vpn:ce-vpe-topo-added").
                  appendState("l3vpn:vpe-p0-topo-added");
              }
              pcomp.appendState("l3vpn:qos-configured").
                    appendState("ncs:ready");

              boolean p_ready = true;
              if (onvpe) {
                  if (rfmProgress.containsKey(p + ".pe-created")) {
                      pcomp.setReached("l3vpn:pe-created");
                  } else {
                      p_ready = false;
                  }
                  if (rfmProgress.containsKey(p + ".ce-vpe-topo-added")) {
                      pcomp.setReached("l3vpn:ce-vpe-topo-added");
                  } else {
                      p_ready = false;
                  }
                  if (rfmProgress.containsKey(p + ".vpe-p0-topo-added")) {
                      pcomp.setReached("l3vpn:vpe-p0-topo-added");
                  } else {
                      p_ready = false;
                  }
              }
              if (rfmProgress.containsKey(p + ".qos-configured")) {
                  pcomp.setReached("l3vpn:qos-configured");
              } else {
                  p_ready = false;
              }

              if (p_ready) {
                  pcomp.setReached("ncs:ready");
              } else {
                  ready = false;
              }
          }

          if (ready) {
              self.setReached("ncs:ready");
          }
      } catch (Exception e) {
          throw new NavuException("could not update plan.", e);
      }

  }

}
