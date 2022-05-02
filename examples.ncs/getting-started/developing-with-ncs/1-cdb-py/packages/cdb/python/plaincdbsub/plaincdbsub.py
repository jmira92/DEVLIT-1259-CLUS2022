# -*- mode: python; python-indent: 4 -*-

import _ncs
import _ncs.cdb as cdb
import _ncs.maapi as maapi
from ncs_pyvm import NcsPyVM

import _namespaces.test_ns as ns

import socket
import threading

_schemas_loaded = False

# This low level cdb subscriber subscribes to changes under the path
# /devices/device{ex0}/config
# Whenever a change occurs there, the code iterates through the
# change and prints the values. Thus to trigger this subscription code
# go into the ncs_cli and commit any change under the subscription
# path. For example:

# ncs_cli -u admin
# admin connected from 127.0.0.1 using console on iron.local
# admin@iron> configure
# dmin@iron% set devices device ex0 config sys syslog server 4.5.6.7 enabled
# [ok][2012-07-05 12:57:59]

# [edit]
# admin@iron% commit
# Commit complete.

# will trigger the subscription code, the code logs and the data will end up
# in ./logs/ncs-python-vm-cdb.log (relative to where the ncs daemon executes)

# The code runs in an 'application' component, it implements
# the ApplicationComponent interface, this includes the run() method
# so the code will run in a Thread.


class Subscriber(object):
    def __init__(self, prio, path, debug):
        self.sock = socket.socket()
        self.path = path
        self.debug = debug
        self.prio = prio
        global _schemas_loaded

        if _schemas_loaded is False:
            ms = socket.socket()
            maapi.connect(sock=ms, ip='127.0.0.1',
                          port=_ncs.NCS_PORT)
            maapi.load_schemas(ms)
            ms.close()
            _schemas_loaded = True

        cdb.connect(self.sock, type=cdb.DATA_SOCKET, ip='127.0.0.1',
                    port=_ncs.NCS_PORT, path=self.path)
        self.subid = cdb.subscribe(self.sock, prio=self.prio, path=self.path,
                                   nspace=ns.ns.hash)
        cdb.subscribe_done(self.sock)
        self.debug("Subscription {0}, subscribed to {1}".format(self.subid,
                                                                self.path))

    def wait(self):
        cdb.read_subscription_socket(self.sock)

    def ack(self):
        cdb.sync_subscription_socket(self.sock, cdb.DONE_PRIORITY)

    def diff_iter(self):
        def iterator(kp, op, oldv, newv, state):
            self.debug(
                "diffIterate: kp= {0}, OP= {1}, old_value={2}, new_value={3}"
                .format(str(kp), op, str(oldv), str(newv)))
            return _ncs.ITER_RECURSE
        cdb.diff_iterate(self.sock, self.subid, iterator, 0, None)
    
    def diff_iter_loop(self):
        while True:
            self.wait()
            self.diff_iter()
            self.ack()

    def close(self):
        cdb.end_session(self.sock)
        cdb.close(self.sock)


# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------

class Plaincdbsub(object):
    def __init__(self, *args, **kwds):
        # Setup the NCS object, containing mechanisms
        # for communicating between NCS and this User code.
        self._ncs = NcsPyVM(*args, **kwds)
        self.debug('--- Service INIT OBJECT')
        # Register our 'finish' callback
        self._finish_cb = lambda: self.finish()

        self.sub = Subscriber(prio=100, path="/devices/device{ex0}/config",
                              debug=self.debug)

        self._ncs.reg_finish(self._finish_cb)

        self._stopevent = threading.Event()

    # This method is supposed to start the User application
    def run(self):
        self.debug('Running diff iter loop')
        self.sub.diff_iter_loop()

    # Just a convenient logging function
    def debug(self, line):
        self._ncs.debug(line)

    # Callback that will be invoked by NCS when the system is shutdown.
    # Make sure to shutdown the User code, including any User created threads.
    def finish(self):
        self.debug(' PlainSub in finish () =>\n')
        self.sub.close()
        self._stopevent.set()
        self.debug(' PlainSub in finish () => ok\n')
