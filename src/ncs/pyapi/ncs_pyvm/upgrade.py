#
# Copyright 2019 Cisco Systems
#
"""Module for running upgrade components.

This module is executed in a separate Python VM started by NSO
at the end of phase 0 by sending the flag '-u' to bin/ncs-start-python-vm
(or whatever is specified in ncs.conf).
"""
import sys
import socket
import inspect
import ncs
import _ncs


# ------------------------------------------------------------------------------
def log(msg):
    """Simple log formatter."""
    print('{}'.format(msg))


# ------------------------------------------------------------------------------
def call_upgrade_fun(fun):
    """Calls the upgrade method from class specified in package-meta-data.xml.

    The method is called with two arguments:

    cdbsock - a connected CDB data socket
    trans = a ncs.maapi.Transaction instance attached to the init trans

    The method should return True or None (or no return) to indicate
    success. Any other return value indicates error.

    Example method:

    def upgrade(self, cdbsock, trans):
        # read old data
        v = _ncs.cdb.get(cdbsock, '/some/path/to/leaf')
        new_v = convert_value(v)
        trans.set_elem(new_v, /some/path/to/leaf')
    """
    cdbsock = socket.socket()
    _ncs.cdb.connect(cdbsock, ncs.cdb.DATA_SOCKET, '127.0.0.1', ncs.PORT)
    try:
        with ncs.maapi.Maapi() as m:
            with m.attach_init() as t:
                return fun(cdbsock, t)
    finally:
        _ncs.cdb.close(cdbsock)


# ------------------------------------------------------------------------------
def do_upgrade(pkgname, cls):
    """Performs the upgrade.

    First it tries to get hold of the class object by importing
    the module 'pkgname' and doing getattr() for find the right thing.

    If the object is a class it is instantiated using the empty constructor
    and the method 'upgrade' is called if it exists.
    """
    if '.' not in cls:
        log("Error: class name '{}' must be fully qualified".format(cls))
        return 1

    modname, clsname = cls.rsplit('.', 1)
    mod = __import__(modname)
    for m in modname.split('.')[1:]:
        mod = getattr(mod, m)
    clso = getattr(mod, clsname)

    if inspect.isclass(clso):
        _tmp = clso()
        fun = getattr(_tmp, 'upgrade', None)
        if fun is None or not inspect.ismethod(fun):
            log("Error: class '{}' has no 'upgrade' method".format(clsname))
            return 1
    else:
        log("Error: '{}' is not a class".format(cls))
        return 1

    ret = call_upgrade_fun(fun)

    if ret is None or ret is True:
        return 0
    return 1


# ------------------------------------------------------------------------------
# main entry point (started by bin/ncs-start-python-vm -u)
# ------------------------------------------------------------------------------
if __name__ == '__main__':
    if len(sys.argv) != 4:
        log('usage: {} -u <package> <class>'.format(sys.argv[0]))
        sys.exit(1)

    # sys.argv[1] is '-u'

    # package name
    pkgname = sys.argv[2]
    # fully qualified class name
    cls = sys.argv[3]

    sys.exit(do_upgrade(pkgname, cls))
