package com.example.weblog;

import com.example.weblog.namespaces.weblog;
import com.tailf.cdb.Cdb;
import com.tailf.cdb.CdbDBType;
import com.tailf.cdb.CdbLockType;
import com.tailf.cdb.CdbSession;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfPath;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.text.SimpleDateFormat;
import java.util.EnumSet;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

public class WebUILogger {
  private WebUILogger() {
  }

  private static Logger LOGGER = LogManager.getLogger(WebUILogger.class);

  private static class TheLogger implements Runnable {
    private SimpleDateFormat df = new SimpleDateFormat(
        "yyyy/MM/dd HH:mm:ss.SSS");

    TheLogger() throws UnknownHostException, IOException, ConfException {
      Socket socket = new Socket("127.0.0.1", Conf.NCS_PORT);
      cdb = new Cdb("WebUILogger", socket);
    }

    private BlockingQueue<Log> bq = new ArrayBlockingQueue<WebUILogger.Log>(5);
    private Cdb cdb;

    @Override
    public void run() {
      long lastTimestamp = 0;
      while (!Thread.currentThread().isInterrupted()) {
        try {
          Log log = bq.take();
          // Have to provide LOCK_REQUEST to generate subscription events
          CdbSession sess = cdb.startSession(
              CdbDBType.CDB_OPERATIONAL,
              EnumSet.of(CdbLockType.LOCK_REQUEST,
                         CdbLockType.LOCK_WAIT));
          try {
            // If the logs are very close to each other, we may get
            // collisions.
            long ts = log.timestamp.getTime();
            if (ts == lastTimestamp) {
              ++ts;
            }

            lastTimestamp = ts;

            String path = String.format("/" + weblog.prefix + ":" +
                           weblog._logs_ + "/" + weblog._web_ + "{%d}", ts);
            ConfPath p = new ConfPath(path);

            sess.create(p);
            sess.setElem(new ConfBuf(df.format(log.timestamp)),
                p.copyAppend(weblog._timestamp_text_));
            sess.setElem(new ConfBuf(log.msg), p.copyAppend(weblog._message_));
          } finally {
            sess.endSession();
          }
        } catch (Exception e) {
          LOGGER.error(e.getMessage(), e);
        }
      }
    }
  }

  private static class Log {
    java.util.Date timestamp = new java.util.Date();
    String msg;

    public Log(String msg) {
      this.msg = msg;
    }
  }

  private static TheLogger theLogger;
  private static Thread loggerTh;

  static {
    try {
      theLogger = new TheLogger();
      loggerTh = new Thread(theLogger);
      loggerTh.start();
    } catch (Exception e) {
      LOGGER.error(e.getMessage(), e);
    }
  }

  public static void log(Logger extLogger, String log) {
    extLogger.info(log);
    try {
      theLogger.bq.put(new Log(log));
    } catch (InterruptedException e) {
      extLogger.error(e.getMessage(), e);
    }
  }

  public static void logRedeploy(Logger extLogger, String msg) {
    String log = String.format("Redeploy: %s", msg);
    extLogger.info(log);
    try {
      theLogger.bq.put(new Log(log));
    } catch (InterruptedException e) {
      extLogger.error(e.getMessage(), e);
    }
  }
}
