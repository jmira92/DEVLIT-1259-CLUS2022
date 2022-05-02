"""NCS Python low level module.

This module and its submodules provide Python bindings for the C APIs,
described by the confd_lib(3) man page.

The companion high level module, ncs, provides an abstraction layer on top of
this module and may be easier to use.
"""
import sys
import os

from ._ncs_py3 import cdb
from ._ncs_py3 import maapi
from ._ncs_py3 import dp
from ._ncs_py3 import ha
from ._ncs_py3 import lib
from ._ncs_py3 import error
from ._ncs_py3 import events

# Add these to sys.modules so python3 can find them
# when you do import ncs._dp and family
sys.modules['_ncs.cdb'] = cdb
sys.modules['_ncs.maapi'] = maapi
sys.modules['_ncs.dp'] = dp
sys.modules['_ncs.ha'] = ha
sys.modules['_ncs.error'] = error
sys.modules['_ncs.events'] = events

# Python 3 version of "from lib import *"
for symbol in [x for x in dir(lib) if x[0] != '_']:
    setattr(sys.modules['_ncs'], symbol, getattr(lib, symbol))

# If run inside a package the configured NCS port can be found
# in the environment variable NCS_IPC_PORT. We get the value from
# there and redefine _ncs.PORT to that value.
try:
    PORT = int(os.environ.get('NCS_IPC_PORT'))
except Exception:
    pass

del symbol
del lib
del _ncs_py3
del os
