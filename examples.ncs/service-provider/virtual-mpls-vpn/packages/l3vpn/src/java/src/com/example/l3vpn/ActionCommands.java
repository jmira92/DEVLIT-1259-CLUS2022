package com.example.l3vpn;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.net.Socket;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.example.l3vpn.namespaces.l3vpn;
import com.example.l3vpn.namespaces.l3vpnTypes;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfEnumeration;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfKey;
import com.tailf.conf.ConfObject;
import com.tailf.conf.ConfTag;
import com.tailf.conf.ConfXMLParam;
import com.tailf.dp.DpActionTrans;
import com.tailf.dp.DpCallbackException;
import com.tailf.dp.annotations.ActionCallback;
import com.tailf.dp.proto.ActionCBType;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.navu.NavuContainer;
import com.tailf.ncs.NcsMain;
import com.tailf.ncs.ns.Ncs;

public class ActionCommands {
  private static Logger LOGGER = LogManager.getLogger(ActionCommands.class);

  private Maapi maapi;

  private void init() throws DpCallbackException {
    try {
      Socket socket = new Socket(NcsMain.getInstance().getNcsHost(),
                                 NcsMain.getInstance().getNcsPort());
      maapi = new Maapi(socket);
      maapi.startUserSession("", maapi.getSocket().getInetAddress(), "system",
          new String[] { "" }, MaapiUserSessionFlag.PROTO_TCP);
    } catch (Exception e) {
      throw new DpCallbackException(e);
    }
  }

  @ActionCallback(callPoint = "recovery-action", callType = ActionCBType.INIT)
  public void initRecoveryComplete(DpActionTrans trans)
      throws DpCallbackException {
    init();
  }

  @ActionCallback(callPoint = "recovery-action", callType = ActionCBType.ACTION)
  public ConfXMLParam[] recoveryComplete(DpActionTrans trans, ConfTag name,
      ConfObject[] kp, ConfXMLParam[] params) throws DpCallbackException {
    try {

      ConfKey key = (ConfKey) kp[0];
      ESCParameters escParam = getParams(key);
      LOGGER.info("ESC param: " + escParam);

      ConfEnumeration action = (ConfEnumeration) params[0].getValue();

      LOGGER.info("action " + action);

      ConfBuf tenant = ((ConfBuf) key.elementAt(0));

      String fileName;
      switch (action.getOrdinalValue()) {
      case l3vpnTypes._complete_success:
        fileName = "VM_RECOVERY_COMPLETE";
        break;
      case l3vpnTypes._complete_failure:
        fileName = "VM_RECOVERY_COMPLETE_FAILURE";
        break;
      case l3vpnTypes._undeployed:
        fileName = "VM_RECOVERY_UNDEPLOYED";
        break;
      default:
        throw new DpCallbackException("Unknown action " + action);
      }

      createFile(fileName, tenant.toString(), escParam);
    } catch (Exception e) {
      throw new DpCallbackException(e);
    }

    return new ConfXMLParam[0];
  }

  private ESCParameters getParams(ConfKey key) throws IOException,
      ConfException {
    int tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
    NavuContainer ncsRoot = new NavuContainer(maapi, tid, Ncs.hash);
    NavuContainer l3vpnRoot = ncsRoot.getParent().container(l3vpn.uri);

    ESCParameters escParam = l3vpnRFS.getEscVM(l3vpnRoot.container(l3vpn._vpn)
        .list(l3vpn._l3vpn).elem(key.elementAt(0).toString()));

    // maapi.applyTrans(tid, false);
    maapi.finishTrans(tid);

    return escParam;
  }

  private void createFile(String filename, String tenant, ESCParameters params)
      throws IOException {
    String line = String.format("%s %s %s %s", params.vm_group,
        params.service_name, "vpn", tenant);
    String path = "/tmp/" + filename;
    LOGGER.info(String.format("Writing '%s' to %s", line, path));
    BufferedWriter out = new BufferedWriter(new FileWriter(path));
    try {
      out.write(line);
      out.write("\n");
    } finally {
      out.close();
    }
  }
}
