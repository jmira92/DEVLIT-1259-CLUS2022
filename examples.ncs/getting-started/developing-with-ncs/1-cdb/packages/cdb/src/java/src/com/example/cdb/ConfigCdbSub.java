package com.example.cdb;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.tailf.cdb.Cdb;
import com.tailf.cdb.CdbDBType;
import com.tailf.cdb.CdbDiffIterate;
import com.tailf.cdb.CdbException;
import com.tailf.cdb.CdbSession;
import com.tailf.cdb.CdbSubscription;
import com.tailf.cdb.CdbSubscriptionSyncType;
import com.tailf.conf.ConfBinary;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfEnumeration;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfInt16;
import com.tailf.conf.ConfKey;
import com.tailf.conf.ConfObject;
import com.tailf.conf.ConfPath;
import com.tailf.conf.ConfTag;
import com.tailf.conf.ConfUInt16;
import com.tailf.conf.ConfValue;
import com.tailf.conf.DiffIterateFlags;
import com.tailf.conf.DiffIterateOperFlag;
import com.tailf.conf.DiffIterateResultFlag;
import com.tailf.conf.ErrorCode;
import com.tailf.examples.router.namespaces.router;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.ResourceManager;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;

// This low level cdb subscriber subscribes to changes under the path
// /devices/device/config/sys/interfaces/interface.
//
// Whenever a change occurs in the subscription path, the code iterates
// through the change and populate the elements in instance variable
// devices which is a java.util.List.
//
// The devices instance variable is a structure that correspond
// to the yang model in the device model.
//
// In the cdb-example package there is 5 components that all are
// subscribers and prints various information to the ncs-java-log.
// In this example we are focus on what this subscriber prints in
// the ncs-java-log so we need to filter out all the other information
// that other subscriber are printing thus we issue this in CLI:
//
// %> set java-vm java-logger com.example.cdb level level-off
// %> set java-vm java-logger com.example.cdb.ConfigCdbSub level level-info
// %> set java-vm java-logger com.example.cdb.ConfigCdbSub$DiffIterateImpl
// %> commit
//
// Now we sync NCS with our devices
//
// %> request devices direction from device
//
//
// Thus to trigger this subscription code
// go into the ncs_cli and commit any change under the subscription
//     path. For example:
//
// # ncs_cli -u admin
// admin connected from 127.0.0.1 using console on iron.local
// admin@iron> configure
// admin@iron% set devices device ex0 config sys interfaces interface en0
// mac 3c:07:54:71:13:09 mtu 1500 duplex half unit 0 family inet
// address 192.168.1.115 broadcast 192.168.1.255 prefix-length 32
// [ok][2012-07-05 12:57:59]

// [edit]
// admin@iron% commit
// Commit complete.
//
// will trigger the subscription code, the code logs and the data will end up
// in ./logs/ncs-java-vm.log (relative to where the ncs daemon executes)
//
// The code runs in an 'application' component, it implements
// the ApplicationComponent interface, this includes the run() method
// so the code will run in a separate thread started by NCS JVM.
public class ConfigCdbSub implements ApplicationComponent
{
    private static final Logger log = LogManager.getLogger(ConfigCdbSub.class);

    // let our ResourceManager inject Cdb sockets to us
    // no explicit creation of creating and opening sockets needed
    @Resource(type=ResourceType.CDB, scope=Scope.INSTANCE, qualifier="sub-sock")
    private Cdb cdbSub;

    @Resource(type=ResourceType.CDB,scope=Scope.INSTANCE, qualifier="data-sock")
    private Cdb cdbData;

    private boolean requestStop;
    private int point;

    // actually we have 3 devices (ex0,ex1,ex2) but for simplicity we
    // make it a List of n Device instances
    private CdbSubscription cdbSubscription;
    private List<Device> devices = new ArrayList<Device>();

    // some data structures which corresponds to the yang model
    // under "/ncs:devices/device/config/sys/interfaces/interface"
    // that the subscriber will use to fill values.
    protected static class Device {
        //one device could have many router interfaces
        private List<RouterInterface> routerInterfaces =
            new ArrayList<RouterInterface>();
        private ConfKey key;
        private int ordinal;

        public Device(ConfKey key, int ordinal) {
            this.key = key;
            this.ordinal = ordinal;
        }

        public int getOrdinal() {
            return ordinal;
        }

        public ConfKey getKey() {
            return this.key;
        }

        public String toString() {
            return key + "[" + routerInterfaces + "]";
        }

        public List<RouterInterface> getInterfaces() {
            return routerInterfaces;
        }
    }

    protected static class RouterInterface {
        protected ConfKey         key;
        protected ConfBuf         description;
        protected ConfEnumeration speed;
        protected ConfEnumeration duplex;
        protected ConfInt16       mtu;
        protected ConfBinary      mac;
        protected List<Unit>      units = new ArrayList<Unit>();

        public String toString() {
            return "[" + key + " \"" + description  + "\" " +
                   " " + speed + " " + duplex + " " + mtu + "]";
        }
    }

    protected static class Unit {
        protected ConfKey key;
        protected ConfBuf description;
        protected ConfUInt16 vlanId;
        protected List<AddressFamily> family = new ArrayList<AddressFamily>();
    }

    protected static class AddressFamily {
        protected ConfKey key;
        protected ConfValue prefixLength;
        protected ConfValue broadCast;
    }


    public void init() {
        log.info(" init cdb subscriber ");

        try {
            cdbSubscription = cdbSub.newSubscription();
            String path="/ncs:devices/device/config/r:sys/interfaces/interface";
            point = cdbSubscription.subscribe(1, router.hash, path);
            cdbSubscription.subscribeDone();
            log.info("subscribeDone");
            requestStop = false;
        } catch (Exception e) {
            log.error("Fail in init",e);
        }
    }

    public void finish() {
        requestStop = true;
        try {
            ResourceManager.unregisterResources(this);
        } catch (Exception e) {
            log.error("Fail in finish",e);
        }
    }

    public void run() {
        readDb(cdbData);
        dumpDb();

        try {
            while (!requestStop) {
                try {
                    int[] point = cdbSubscription.read();
                    CdbSession cdbSession =
                        cdbData.startSession(CdbDBType.CDB_RUNNING);
                    EnumSet<DiffIterateFlags> diffFlags =
                        EnumSet.of(DiffIterateFlags.ITER_WANT_PREV);
                    cdbSubscription.diffIterate(point[0], new DiffIterateImpl(),
                                                diffFlags, cdbSession);
                    cdbSession.endSession();
                    dumpDb();
                } finally {
                    cdbSubscription.sync(CdbSubscriptionSyncType.DONE_PRIORITY);
                }
            }
        } catch (Exception e) {
            log.error("Fail in run", e);
        }
        requestStop = false;
    }

    void dumpDb() {
        log.info(" ******************** DUMP DB ******************** ");
        try {
            for (Device device : devices) {
                log.info(" Device " + device.key);
                ConfPath ifPath =
                    new ConfPath (cdbData,
                                  "/ncs:devices/device[%d]/config/" +
                                  "r:sys/interfaces/interface",
                                  device.getOrdinal());

                for (RouterInterface routerInterface: device.getInterfaces()) {
                    log.info("    INTERFACE");
                    log.info("      name: " + routerInterface.key);
                    log.info("      description:" +routerInterface.description);
                    log.info("      speed:" + ConfEnumeration.
                             getLabelByEnum(ifPath.copyAppend("speed"),
                                            routerInterface.speed ));
                    log.info("      duplex:" + ConfEnumeration.
                             getLabelByEnum(ifPath.copyAppend("duplex"),
                                            routerInterface.duplex ));
                    log.info("      mtu:" + routerInterface.mtu);
                    log.info("      mac:" + routerInterface.mac);

                    for ( Unit unit: routerInterface.units ) {
                        log.info ("      UNIT");
                        log.info ("       name: " + unit.key) ;
                        log.info ("       descripton: " + unit.description);
                        log.info ("       vlan-id:" + unit.vlanId);

                        for ( AddressFamily addressFamily : unit.family) {
                            log.info ("        ADDRESS-FAMILY");
                            log.info ("          key: " + addressFamily.key);
                            log.info ("          prefixLength: " +
                                addressFamily.prefixLength);
                            log.info ("          broadCast:" +
                                addressFamily.broadCast);
                        }
                    }
                }
            }
        } catch (ConfException e) {
            log.error("Fail in dumpdb",e);
        }
    }

    private Device device(ConfKey devKey) {
        Device foundDevice = null;
        for (Device device : devices) {
            if (device.key.equals(devKey)) {
                foundDevice = device;
                break;
            }
        }
        return foundDevice;
    }


    private RouterInterface routerInterface(ConfKey devKey, ConfKey routerKey) {
        ConfigCdbSub.RouterInterface foundInterface = null;

        for (Device device: devices) {
            if (device.key.equals(devKey)) {
                for (RouterInterface routerInterface: device.getInterfaces()) {
                    if (routerInterface.key.equals(routerKey)) {
                        foundInterface = routerInterface;
                        break;
                    }
                }
            }
            if (foundInterface != null) break;
        }
        return foundInterface;
    }

    private Unit routerInterfaceUnit(ConfKey devKey,
                                     ConfKey routerIfKey,
                                     ConfKey unitKey) {
        RouterInterface routerInterface = routerInterface(devKey, routerIfKey);
        Unit foundUnit = null;
        for (Unit unit : routerInterface.units) {
            if (unit.key.equals(unitKey)) {
                foundUnit = unit;
                break;
            }
        }
        return foundUnit;
    }

    private AddressFamily addressFamily(ConfKey devKey,
                                        ConfKey routerIfKey,
                                        ConfKey unitKey,
                                        ConfKey addressFamilyKey) {
        Unit unit = routerInterfaceUnit(devKey, routerIfKey, unitKey);
        AddressFamily foundAddressFamily = null;
        for (AddressFamily addressFamily : unit.family) {
            if (addressFamily.key.equals(addressFamilyKey)) {
                foundAddressFamily = addressFamily;
                break;
            }
        }
        return foundAddressFamily;
    }

    private void readDb(Cdb cdbDataSocket){
        CdbSession cdbSession = null;
        try {
            cdbSession = cdbDataSocket.startSession(CdbDBType.CDB_RUNNING);
            int d = cdbSession.getNumberOfInstances( "/ncs:devices/device");
            for (int i = 0; i < d; i++) {
                String devicePath = "/ncs:devices/device[%d]";
                ConfValue devName = cdbSession.getElem(devicePath + "/name", i);
                boolean containsDevice = false;

                for (Device device : devices) {
                    if (device.key.equals(new ConfKey(devName ))) {
                        containsDevice = true;
                        break;
                    }
                }

                if(!containsDevice) {
                    devices.add(new Device(new ConfKey(devName), i));
                }
                String interfacePath = devicePath + "/config/" +
                    "r:sys/interfaces/interface";

                int nInterfaces = cdbSession.
                    getNumberOfInstances(new ConfPath(cdbDataSocket,
                                                      interfacePath, i));

                for (int j = 0; j < nInterfaces; j++) {
                    String ifNamePath = interfacePath + "[%d]/name";
                    ConfValue ifKey =
                        cdbSession.getElem(new ConfPath(ifNamePath, i, j));
                    readIf(cdbSession, new ConfKey(devName),new ConfKey(ifKey));
                    String unitPath =interfacePath + "[%d]/unit";
                    int nUnits = cdbSession.
                        getNumberOfInstances(new ConfPath(unitPath, i, j));
                    log.info(" rUnits:" + nUnits);
                    for (int k = 0; k < nUnits; k++) {
                        ConfValue unitKey = cdbSession.
                            getElem(new ConfPath(unitPath + "[%d]/name",i,j,k));
                        readUnit(cdbSession,new ConfKey(devName),
                                 new ConfKey(ifKey), new ConfKey(unitKey));

                        try {
                            ConfTag tag = (ConfTag) cdbSession.
                                getCase("family", "/ncs:devices/device[%d]/" +
                                        "config/r:sys/interfaces/" +
                                        "interface[%d]/unit[%d]/family", i,j,k);
                            String addrFamilyPath = unitPath + "[%d]/family";

                            switch ( tag.getTagHash() ) {
                                case router._c1: {
                                    addrFamilyPath += "/inet";
                                    break;
                                }
                                case router._c2: {
                                    addrFamilyPath += "/inet6";
                                    break;
                                }
                            }

                            addrFamilyPath += "/address";
                            int nAddress = cdbSession.getNumberOfInstances(
                                new ConfPath (addrFamilyPath, i, j, k));

                            for (int l = 0; l < nAddress; l++) {
                                ConfValue addrKey = cdbSession.getElem(
                                    new ConfPath(addrFamilyPath +
                                                 "[%d]/name",i,j,k,l));
                                readFamily(cdbSession,
                                           new ConfKey(devName ),
                                           new ConfKey(ifKey),
                                           new ConfKey(unitKey),
                                           new ConfKey(addrKey));
                            }
                        } catch (CdbException e) {
                            if (e.getErrorCode() == ErrorCode.ERR_NOEXISTS) {
                                log.warn(e.getMessage());
                            } else {
                                log.error("Fail in readDB", e) ;
                            }
                        } catch ( ConfException e ) {
                            log.error("Fail in readDB",e);
                        }
                    }
                }

            }
            cdbSession.endSession();
        } catch (Exception e) {
            log.error("Could not start new session ", e);
            return;
        }
    }

    private void readIf(CdbSession cdbSession, ConfKey deviceKey, ConfKey key) {
        RouterInterface currentRouterIf = null;
        Device currentDevice = device(deviceKey);
        log.info(" devices :" + devices );

        try {
            if (currentRouterIf == null) {
                currentRouterIf = new RouterInterface();

                for (Device device : devices) {
                    if (device.getKey().equals(deviceKey)) {
                        device.getInterfaces().add(currentRouterIf);
                        break;
                    }
                }
            }

            ConfPath ifPath =
                new ConfPath ("/ncs:devices/device{%x}/config/r:sys/" +
                              "interfaces/interface{%x}", deviceKey, key );
            cdbSession.cd(ifPath);
            currentRouterIf.key = new ConfKey(cdbSession.getElem("name"));
            currentRouterIf.description =
                (ConfBuf) cdbSession.getElem("description");
            currentRouterIf.speed =(ConfEnumeration)cdbSession.getElem("speed");
            currentRouterIf.duplex =
                (ConfEnumeration)cdbSession.getElem("duplex");
            currentRouterIf.mtu =(ConfInt16) cdbSession.getElem("mtu");
            currentRouterIf.mac = (ConfBinary) cdbSession.getElem("mac");
        } catch (Exception e) {
            log.error("Could not read values", e);
        }
    }

    private void readFamily(CdbSession cdbSession, ConfKey devKey,
                            ConfKey routerIfKey,
                            ConfKey unitKey, ConfKey addressFamilyKey ) {
        try {
            AddressFamily currentFamily =
                addressFamily(devKey, routerIfKey, unitKey, addressFamilyKey);
            if (currentFamily == null) {
                currentFamily = new AddressFamily();
                routerInterfaceUnit(devKey, routerIfKey,unitKey).family.
                    add(currentFamily);
            }
            ConfTag tag = null;
            tag = (ConfTag) cdbSession.getCase("family",
                               "/ncs:devices/device{%x}/config/r:sys/" +
                               "interfaces/interface{%x}/unit{%x}/family",
                               devKey, routerIfKey, unitKey);
            switch (tag.getTagHash()) {
                case router._c1: {
                    ConfPath inetPath = null;
                    try {
                        inetPath = new ConfPath(
                            "/ncs:devices/device{%x}/config/r:sys/" +
                            "interfaces/interface{%x}/unit{%x}/family" +
                            "/inet/address{%x}", devKey, routerIfKey,
                            unitKey, addressFamilyKey);
                    } catch (ConfException e) {
                        log.error("Fail in readFamily", e);
                        return;
                    }

                    cdbSession.cd(inetPath);
                    currentFamily.key = new ConfKey(cdbSession.getElem("name"));
                    currentFamily.prefixLength =
                        cdbSession.getElem("prefix-length");
                    currentFamily.broadCast = cdbSession.getElem("broadcast");
                    break;
                }
                case router._c2: {
                    ConfPath inet6Path = null;
                    try {
                        inet6Path = new ConfPath(
                            "/ncs:devices/device{%x}/config/r:sys/" +
                            "interfaces/interface{%x}/unit{%x}/family" +
                            "/inet6/address{%x}", devKey, routerIfKey,
                            unitKey, addressFamilyKey);
                    } catch (ConfException e) {
                        log.error("Fail in readFamily", e);
                        return;
                    }

                    currentFamily.key = new ConfKey(cdbSession.getElem("name"));
                    currentFamily.prefixLength =
                        cdbSession.getElem("prefix-length");
                    break;
                }
            }
        } catch ( Exception e ) {
            log.error("File in readFamily",e);
            return;
        }
    }

    private void readUnit(CdbSession cdbSession, ConfKey devKey, ConfKey ifKey,
                          ConfKey unitKey) {
        log.info(" Reading units ** ");
        try {
            Unit currentUnit = routerInterfaceUnit(devKey, ifKey, unitKey);
            if (currentUnit == null) {
                currentUnit = new Unit();
                routerInterface(devKey, ifKey).units.add(currentUnit);
            }

            ConfPath ifPath = new ConfPath(
                "/ncs:devices/device{%x}/config/r:sys/" +
                    "interfaces/interface{%x}/unit{%x}", devKey, ifKey,unitKey);
            cdbSession.cd(ifPath);
            currentUnit.key = new ConfKey(cdbSession.getElem("name"));
            currentUnit.description =(ConfBuf)cdbSession.getElem("description");
            currentUnit.vlanId = (ConfUInt16)cdbSession.getElem("vlan-id");
        } catch (Exception e) {
            log.error("Fail in readUnit", e);
            return;
        }
    }


    // This class implements the com.tailf.cdb.CdbDiffIterate
    private class DiffIterateImpl implements CdbDiffIterate {
        // we create out logger for this inner class
        // so to see what this class prints out
        // we need to create a logger in the CLI
        // %> set java-vm java-logger
        //     com.example.cdb.ConfigCdbSub$DiffIterateImpl level-info
        // %> commit
        private Logger log = LogManager.getLogger(DiffIterateImpl.class);

        public DiffIterateResultFlag  iterate(ConfObject[] kp,
                                              DiffIterateOperFlag op,
                                              ConfObject oldValue,
                                              ConfObject newValue,
                                              Object initstate) {
            CdbSession cdbSession = (CdbSession)initstate;
            ConfPath path = new ConfPath(kp) ;

            log.info(path + " --> " + op);
            log.info("kp=" + java.util.Arrays.toString(kp));

            switch (op) {
                case MOP_CREATED: {
                    // the case in which some entry has been created
                    // in the list or a presence container has been created
                    // however we do not have a presence container in this
                    // example.
                    //Retrieve the list tag, kp[0] should always be
                    //the ConfKey
                    ConfTag tag = (ConfTag)kp[1];

                    //What was created ?
                    switch(tag.getTagHash()) {
                    /* an router interface was created */
                        case router._interface: {
                            /* keypath is /devices/device{$key}/config/r:sys
                               /interfaces/interface{$key2} */
                            readIf(cdbSession, (ConfKey)kp[5], (ConfKey)kp[0]);
                            break;
                        }
                        //new unit has been added to existing interface
                        case router._unit: {
                            /* keypath is: /devices/device{$key}/config/r:sys
                               /interfaces/interface{$key2}/unit{$key3} */
                            readUnit(cdbSession, (ConfKey)kp[7],(ConfKey)kp[2],
                                     (ConfKey)kp[0]);
                            break;
                        }
                        // new address has been added to existing unit
                        case router._address: {
                            /* keypath is: /devices/device{$key}/config/r:sys
                               /interfaces/interface{$key2}/unit{$key3}
                               /family/[inet|inet6]/address{$key4} */
                            readFamily(cdbSession, (ConfKey)kp[11],
                                       (ConfKey)kp[6], (ConfKey)kp[4],
                                       (ConfKey)kp[0]);
                            break;
                        }
                    }
                    return DiffIterateResultFlag.ITER_RECURSE;
                } // case MOP_CREATED
                case MOP_DELETED: {
                    log.info("Delete: " + path.toString());
                    ConfTag tag = (ConfTag) kp[1];

                    switch (tag.getTagHash()) {
                        case router._interface: {
                            /* an router interface was deleted */
                            /* keypath is /devices/device{$key}/config/r:sys
                             /interfaces/interface{$key2} */
                            ConfKey devKey = (ConfKey)kp[5];
                            ConfKey ifKey = (ConfKey)kp[0];
                            device(devKey).getInterfaces().remove(
                                            routerInterface(devKey, ifKey));
                            return DiffIterateResultFlag.ITER_CONTINUE;
                        }
                        case router._unit: {
                            /* an router interface unit was deleted */
                            /* /devices/device{$key}/config/r:sys/interfaces/
                               interface{$key2}/unit{$key3} */
                            ConfKey devKey = (ConfKey)kp[7];
                            ConfKey ifKey = (ConfKey)kp[2];
                            ConfKey unitKey = (ConfKey)kp[0];

                            routerInterface(devKey, ifKey).units.remove(
                                routerInterfaceUnit(devKey, ifKey,unitKey));

                            return DiffIterateResultFlag.ITER_CONTINUE;
                        }
                        case router._address: {
                            /* an router interface unit was deleted */
                            /* /devices/device{$key}/config/r:sys/interfaces
                               /interface{$key2}/unit{$key3}
                               /family/[inet|inet6]/address{$key4} */
                            ConfKey devKey = (ConfKey) kp[11];
                            ConfKey ifKey = (ConfKey) kp[6];
                            ConfKey unitKey = (ConfKey) kp[4];
                            ConfKey addrKey = (ConfKey) kp[0];

                            routerInterfaceUnit( devKey, ifKey,unitKey).family.
                                remove(addressFamily(devKey, ifKey, unitKey,
                                                     addrKey));
                            return DiffIterateResultFlag.ITER_CONTINUE;
                        }
                    }
                    break;
                }
                case MOP_MODIFIED: {
                    log.info ( " Modified " + path.toString() );
                    return DiffIterateResultFlag.ITER_RECURSE;
                }
                case MOP_VALUE_SET: {
                    /* /devices/device{$key}/config/r:router/interfaces
                       /interface{$key2}/unit{$key3}
                       /family/[inet|inet6]/address{$key4}/name */
                    ConfTag modTag = (ConfTag)kp[2];

                    switch (modTag.getTagHash()) {
                        case router._interface: {
                            /* keypath is: /devices/device{$key}/config/r:router
                               /interfaces/interface{$key2}/
                               [description|speed|duplex|mtu|mac] */
                            ConfTag tag = (ConfTag) kp[0];
                            ConfKey routerKey = (ConfKey)kp[1];
                            ConfKey deviceKey = (ConfKey)kp[6];

                            RouterInterface routerInterface =
                                routerInterface(deviceKey, routerKey );
                            switch (tag.getTagHash()) {
                                case router._description: {
                                    /* keypath is:  /devices/device{$k1}/config
                                     /r:router/interfaces/interface{$k2}
                                     /description */
                                    routerInterface.description =
                                        (ConfBuf)newValue;
                                    return DiffIterateResultFlag.ITER_RECURSE;
                                }
                                case router._speed: {
                                    routerInterface.speed =
                                        (ConfEnumeration) newValue;
                                    return DiffIterateResultFlag.ITER_RECURSE;
                                }
                                case router._duplex: {
                                    routerInterface.duplex =
                                        (ConfEnumeration) newValue;
                                    return DiffIterateResultFlag.ITER_RECURSE;
                                }
                                case router._mtu: {
                                    routerInterface.mtu = (ConfInt16) newValue;
                                    return DiffIterateResultFlag.ITER_RECURSE;
                                }
                                case router._mac: {
                                    routerInterface.mac = (ConfBinary) newValue;
                                    return DiffIterateResultFlag.ITER_RECURSE;
                                }
                            }
                            break;
                        }
                        case router._unit: {
                            /* /devices/device{$key}/config/r:router
                               /interfaces/interface{$key2}
                               /unit{$key3}/[name|description|vlan-id]
                            */
                            ConfTag unitProp =  (ConfTag)kp[0];
                            ConfKey unitKey = (ConfKey)kp[1];
                            ConfKey routerIfKey  = (ConfKey)kp[3];
                            ConfKey deviceKey = (ConfKey)kp[8];

                            Unit unit = routerInterfaceUnit(deviceKey,
                                                            routerIfKey,
                                                            unitKey);
                            switch (unitProp.getTagHash()) {
                                case router._name: {
                                    unit.key = new ConfKey(newValue);
                                    break;
                                }
                                case router._description: {
                                    unit.description = (ConfBuf) newValue;
                                    break;
                                }
                                case router._vlan_id: {
                                    unit.vlanId = (ConfUInt16) newValue;
                                    break;
                                }
                            }
                            return DiffIterateResultFlag.ITER_RECURSE;
                        }
                        case router._address: {
                            /* /devices/device{$k1}/config/r:router/interfaces
                               /interface{$k2}/unit{$k3}
                               /family/[inet|inet6]/address{$key4}/name */
                            ConfTag inetMod = (ConfTag)kp[0];
                            ConfKey addressKey = (ConfKey) kp[1];
                            ConfKey deviceKey = (ConfKey) kp[12];
                            ConfKey unitKey = (ConfKey) kp[5];
                            ConfKey routerIfKey = (ConfKey) kp[7];

                            switch (inetMod.getTagHash()) {
                                case router._name: {
                                    addressFamily(deviceKey,routerIfKey,unitKey,
                                                  addressKey).key =
                                                  new ConfKey(newValue);
                                    return DiffIterateResultFlag.ITER_RECURSE;
                                }
                                case router._prefix_length: {
                                    addressFamily(deviceKey,routerIfKey,unitKey,
                                                  addressKey).prefixLength =
                                                  (ConfValue) newValue;
                                    return DiffIterateResultFlag.ITER_RECURSE;
                                }
                                case router._broadcast: {
                                    addressFamily(deviceKey,routerIfKey,unitKey,
                                                  addressKey).broadCast =
                                                  (ConfValue) newValue;
                                    return DiffIterateResultFlag.ITER_RECURSE;
                                }
                            }
                            break;
                        }
                    }
                }
            }
            return DiffIterateResultFlag.ITER_CONTINUE;
        }
    }
}
