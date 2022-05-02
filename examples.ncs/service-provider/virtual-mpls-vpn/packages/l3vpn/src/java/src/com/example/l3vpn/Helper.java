package com.example.l3vpn;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;

import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuException;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.tailf.cdb.Cdb;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBool;
import com.tailf.conf.ConfEnumeration;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfXMLParam;
import com.tailf.conf.ConfXMLParamValue;
import com.tailf.maapi.Maapi;
import com.tailf.ncs.ns.Ncs;

final class Helper {
  private static Logger LOGGER = LogManager.getLogger(Helper.class);

  public static void safeclose(Cdb s) {
    try {
      s.close();
    } catch (Exception ignore) {
    }
  }

  public static void safeclose(Maapi maapi) {
    try {
      maapi.getSocket().close();
    } catch (Exception ignore) {
    }
  }

  public static String toESCPEDeviceName(String tenant, String deploymentName) {
    return tenant + "_" + deploymentName + "_CSR0";
  }

  public static String makeDevName(String tenant, String depname,
                            String vmgroup, String num) {
    return
            tenant + "_" +
                    depname + "_" +
                    vmgroup + "_" +
                    num;
  }

  public static boolean deviceExists(Maapi maapi, String deviceName)
      throws IOException, ConfException {
    int tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);

    boolean exists = maapi.exists(tid,
        "/ncs:devices/device{%s}/apply-template", deviceName);

    maapi.finishTrans(tid);
    return exists;
  }

  public static boolean isDeviceSynced(Maapi maapi, String deviceName) {
    try {
      // Cannot be synced if it doesn't exist
      if (!deviceExists(maapi, deviceName)) {
        return false;
      }

      ConfXMLParam[] isSyncParams = maapi.requestAction(new ConfXMLParam[0],
          "/ncs:devices/device{%s}/check-sync", deviceName);

      ConfEnumeration resultValue =
          (ConfEnumeration) ((ConfXMLParamValue) isSyncParams[0]).getValue();

      return Ncs._in_sync_result == resultValue.getOrdinalValue();
    } catch (Exception e) {
      LOGGER.error("isDeviceSynced", e);
      return false;
    }

  }

  public static void syncFromDevice(Maapi maapi, String deviceName)
      throws ConfException, IOException {

    LOGGER.info("check-sync");
    if (!isDeviceSynced(maapi, deviceName)) {

      LOGGER.info("sync-from");
      ConfXMLParam[] syncParamsOut = maapi.requestAction(new ConfXMLParam[0],
          "/ncs:devices/device{%s}/sync-from", deviceName);
      LOGGER.info("DONE sync-from");

      ConfXMLParamValue syncResult = (ConfXMLParamValue) syncParamsOut[0];

      ConfBool status = (ConfBool) syncResult.getValue();

      if (!status.booleanValue()) {
        throw new ConfException(((ConfXMLParamValue) syncParamsOut[1])
            .getValue().toString());
      }
    }
  }

  public static InetAddress increment(InetAddress address)
      throws UnknownHostException {
    byte[] addr = address.getAddress();
    int i = addr.length - 1;
    while (i >= 0 && addr[i] == (byte) 0xff) {
      addr[i] = 0;
      i--;
    }

    addr[i]++;
    return InetAddress.getByAddress(addr);
  }
}
