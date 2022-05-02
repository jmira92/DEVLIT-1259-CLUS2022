package com.example.vlan2;


import java.util.Properties;

import com.example.vlan2.namespaces.allocVlanService;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBool;
import com.tailf.conf.ConfPath;
import com.tailf.conf.ConfValue;
import com.tailf.dp.DpCallbackException;
import com.tailf.dp.annotations.ServiceCallback;
import com.tailf.dp.proto.ServiceCBType;
import com.tailf.dp.services.ServiceContext;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuContext;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;

/**
 * This class implement the create logic for the Vlan Service.
 *
 * The example will show how to navigate with NAVU (Navigatinal Utillity).
 *
 * When we set values to our vlan service (example from the NCS CLI):
 *
 * % set alloc-vlan s1 unit 1 if eth0 vid 44
 *
 * % commit
 *
 * Then this mapping logic will run i.e the create() method will be
 * invoked by NCS.
 *
 */

public class AllocVlanServiceRFS {

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
     * @param root    - This NavuNode references the ncs root.
     * @param opaque  - Parameter contains a Properties object.
     *                  This object may be used to transfer
     *                  additional information between consecutive
     *                  calls to the create callback.  It is always
     *                  null in the first call. I.e. when the service
     *                  is first created.
     * @return Properties the returning opaque instance
     * @throws DpCallbackException
     */
    @ServiceCallback(servicePoint = "alloc-vlanspnt",
        callType = ServiceCBType.CREATE)
    public Properties create(ServiceContext context,
                             NavuNode service,
                             NavuNode root,
                             Properties opaque) throws DpCallbackException {

        try {
            // check if it is reasonable to assume that devices
            // initially has been sync-from:ed
            NavuList managedDevices = root.
                container("devices").list("device");
            for (NavuContainer device : managedDevices) {
                if (device.list("capability").isEmpty()) {
                    String mess = "Device %1$s has no known capabilities, " +
                                   "has sync-from been performed?";
                    String key = device.getKey().elementAt(0).toString();
                    throw new DpCallbackException(String.format(mess, key));
                }
            }
        } catch (DpCallbackException e) {
            throw e;
        } catch (Exception e) {
            throw new DpCallbackException("Not able to check devices", e);
        }

        NavuContext operCtx = null;
        try {
            System.out.println("CREATE()");

            // We need a operational data tree, the NavuNode that gets passed
            // to us in the service call is Maapi based.
            // The data we want to read is not in the Maapi transaction
            // because of the FastMap algorithm that has deleted that data
            // before we get invoked.
            // We pick the Maapi instance and start an operational transaction,
            // this transaction has to be finished at the end of the call.

            operCtx = new NavuContext(root.context().getMaapi());
            int th = operCtx.startOperationalTrans(Conf.MODE_READ);
            NavuContainer operBase = new NavuContainer(operCtx);
            NavuContainer operRoot = operBase.container(allocVlanService.hash);

            // Get the container at keypath
            // /alloc-vlan{s1}

            NavuNode vlan = service;
            ConfValue serviceKey = vlan.leaf("name").value();

            // always create help data
            NavuList dataList = vlan.getParent().getParent().
                list("alloc-vlan-data");
            NavuContainer data = dataList.sharedCreate(serviceKey);


            // Always create this first reactive FM request
            NavuContainer getUnit = data.container("request-allocate-unit").
                sharedCreate();

            // Do we have a unit allocated for us ??
            // it'll be null if no unit has been allocated, e.g on the
            // first round

            //ConfValue unitVal =  getUnit.leaf("unit").value();
            ConfValue unitVal = null;
            try {
                unitVal = operRoot.list("alloc-vlan-data").
                    elem(serviceKey.toString()).
                    container("request-allocate-unit").
                    leaf("unit").value();
            }
            catch (Exception e) {}
            System.out.println("unitVal = " + unitVal);

            ConfValue vlanVal = null;

            // If we have received a unit, we must also request a vlan-id
            // i.e a second turn in the reactive FM loop
            if (unitVal != null) {
                NavuContainer getVid = data.container("request-allocate-vid").
                    sharedCreate();

                // Do we have a vlan allocated for us ???
                try {
                    vlanVal = operRoot.list("alloc-vlan-data").
                        elem(serviceKey.toString()).
                        container("request-allocate-vid").
                        leaf("vlan-id").value();
                }
                catch (Exception e) {}
            }

            System.out.println("vlanVal = " + vlanVal);

            ConfPath kp = new ConfPath(vlan.getKeyPath());

            //Now we will need to iterate over all of our managed
            //devices and do a sharedCreate of the interface and the unit
            // This is the normal Fastmap code

            //Get the list of all managed devices.
            NavuList managedDevices = root.container("devices").list("device");

            for(NavuContainer deviceContainer : managedDevices.elements()){

                NavuContainer ifs = deviceContainer.container("config").
                    container("r", "sys").container("interfaces");

                // execute as shared create of the path
                //   /interfaces/interface[name='x']/unit[name='i']

                NavuContainer unit = null;
                if (unitVal != null) {
                    NavuContainer iface =
                        ifs.list("interface").sharedCreate(
                            vlan.leaf("iface").value());
                    iface.leaf("enabled").sharedCreate();
                    unit = iface.list("unit").sharedCreate(unitVal);
                    unit.leaf("description").sharedSet(
                        vlan.leaf("description").value());

                }
                if (unitVal != null && vlanVal != null) {
                    unit.leaf("vlan-id").sharedSet(vlanVal);
                    unit.leaf("enabled").sharedSet(new ConfBool(true));
                    for (ConfValue arpValue : vlan.leafList("arp")) {
                        unit.leafList("arp").sharedCreate(arpValue);
                    }
                }

            }
        } catch (Exception e) {
            throw new DpCallbackException("Could not instantiate service", e);
        } finally {
            try {
                operCtx.finishClearTrans();
            } catch (Exception ignore) {}
        }
        return opaque;
    }

}
