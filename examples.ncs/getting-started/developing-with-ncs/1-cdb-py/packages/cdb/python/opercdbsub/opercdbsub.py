# -*- mode: python; python-indent: 4 -*-

import _ncs
import _ncs.cdb as cdb
import _ncs.maapi as maapi
from ncs_pyvm import NcsPyVM

import _namespaces.test_ns as ns

import socket
import threading

_schemas_loaded = False

# This Navu cdb subscriber subscribes to changes under the path
# /t:test/stats-item
# Whenever a change occurs there, the code iterates through the
# change and prints the values. The other examples dealing with
# configuration changes are triggered by the user adding/changing
# some configuration data. This cannot be done here, there is no
# way to manipulate operational data from the CLI. We must add
# that data using some other process.
# For CDB operational data, we can write a Java program which writes
# into the CDB operational store. The code in CreateOperData.java
# does that. Thus to invoke the main() in CreateOperData, we do
# # ant stats -Dop=CREATE -Dkey=k4
# in the directory above where the build.xml file resides.

# This will trigger the operational data subscriber below

# The code runs in an 'application' component, it implements
# the ApplicationComponent interface, this includes the run() method
# so the code will run in a Thread.


class OperSubscriber(object):
    def __init__(self, prio, path, debug):
        self.sock = socket.socket()
        self.path = path
        self.debug = debug
        self.prio = prio
        global _schemas_loaded

        if _schemas_loaded is False:
            ms = socket.socket()
            maapi.connect(
                sock=ms,
                ip='127.0.0.1',
                port=_ncs.NCS_PORT)
            maapi.load_schemas(ms)
            self.debug("Loaded schemas!\n")
            ms.close()
            _schemas_loaded = True

        cdb.connect(
            self.sock,
            type=cdb.DATA_SOCKET,
            ip='127.0.0.1',
            port=_ncs.NCS_PORT,
            path=self.path)
        self.subid = cdb.oper_subscribe(
            self.sock,
            path=self.path,
            nspace=ns.ns.hash)
        cdb.subscribe_done(self.sock)
        self.debug("Subscription {0}, subscribed to {1}".format(self.subid,
                                                                self.path))

    def wait(self):
        cdb.read_subscription_socket(self.sock)

    def ack(self):
        cdb.sync_subscription_socket(self.sock, cdb.DONE_OPERATIONAL)

    def diff_iter(self):
        def iterator(kp, op, oldv, newv, state):
            self.debug("kp : {0}, op : {1}, oldv : {2}, newv : {3}, state : {4}"
                       .format(str(kp),
                               op,
                               str(oldv),
                               str(newv),
                               str(state)))
            return _ncs.ITER_RECURSE
        cdb.diff_iterate(self.sock, self.subid, iterator, 0, None)

    def diff_iter_loop(self):
        while True:
            self.debug("waiting in iter loop...")
            self.wait()
            self.diff_iter()
            self.ack()

    def close(self):
        cdb.end_session(self.sock)
        cdb.close(self.sock)


# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------

class Opercdbsub(object):
    def __init__(self, *args, **kwds):
        # Setup the NCS object, containing mechanisms
        # for communicating between NCS and this User code.
        self._ncs = NcsPyVM(*args, **kwds)
        self.debug('--- Service INIT OBJECT')
        # Register our 'finish' callback
        self._finish_cb = lambda: self.finish()

        self.sub = OperSubscriber(prio=100, path="/test",
                                  debug=self.debug)

        self._ncs.reg_finish(self._finish_cb)

        self._stopevent = threading.Event()

    # This method is supposed to start the User application
    def run(self):
        self.debug("Runinng user application")
        self.sub.diff_iter_loop()

    # Just a convenient logging function
    def debug(self, line):
        self._ncs.debug(line)

    # Callback that will be invoked by NCS when the system is shutdown.
    # Make sure to shutdown the User code, including any User created threads.
    def finish(self):
        self.debug('OperSub in finish () =>\n')
        self.sub.close()
        self._stopevent.set()
        self.debug('OperSub in finish () => ok\n')
