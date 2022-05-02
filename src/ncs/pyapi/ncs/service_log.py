"""This module provides service logging"""

import ncs


class ServiceLog(object):
    """This class contains methods to write service log entries."""

    def __init__(self, node_or_maapi):
        """Initialize a service log object."""
        self._maapi = ncs.maagic.get_maapi(node_or_maapi)

    def trace(self, path, msg, type):
        """Log a trace message."""
        return self._do_log(path, msg, type, ncs.Value(2, ncs.C_ENUM_VALUE))

    def debug(self, path, msg, type):
        """Log a debug message."""
        return self._do_log(path, msg, type, ncs.Value(3, ncs.C_ENUM_VALUE))

    def info(self, path, msg, type):
        """Log an information message."""
        return self._do_log(path, msg, type, ncs.Value(4, ncs.C_ENUM_VALUE))

    def warn(self, path, msg, type):
        """Log an warning message."""
        return self._do_log(path, msg, type, ncs.Value(5, ncs.C_ENUM_VALUE))

    def error(self, path, msg, type):
        """Log an error message."""
        return self._do_log(path, msg, type, ncs.Value(6, ncs.C_ENUM_VALUE))

    def _do_log(self, path, msg, type, level):
        self._maapi.write_service_log_entry(path, msg, type, level)
