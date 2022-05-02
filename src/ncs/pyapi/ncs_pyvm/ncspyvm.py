# -*- mode: python; python-indent: 4 -*-
#
# Copyright 2015 Cisco Inc.
#
import logging


class NcsPyVM(object):

    def __init__(self, *args, **kwds):
        self._logger = logging.getLogger(kwds['vmid'])
        self.debug('--- Init NcsPyVM object')
        self._ncs_i = kwds['i']
        self._ncs_id = kwds['vmid']
        self._ncs_pname = kwds['pname']
        self._ncs_cname = kwds['cname']
        self._ncs_user_threads = kwds['user_threads']
        self._ncs_callback_channel = kwds['callback_channel']

    def add_running_thread(self, class_name):
        t = {'i': self._ncs_i,
             'pname': self._ncs_pname,
             'cname': self._ncs_cname,
             'main': class_name,
             'status': 'running'}
        self._ncs_user_threads.add_thread(t)

    def del_running_thread(self, class_name):
        t = {'i': self._ncs_i,
             'pname': self._ncs_pname,
             'cname': self._ncs_cname,
             'main': class_name,
             'status': 'running'}
        self._ncs_user_threads.del_thread(t)

    def reg_finish(self, cbfun):
        self._ncs_callback_channel.reg_finish(cbfun)

    def unreg_finish(self, cbfun):
        self._ncs_callback_channel.unreg_finish(cbfun)

    def debug(self, line):
        self._logger.debug(line)

    def info(self, line):
        self._logger.info(line)

    def warning(self, line):
        self._logger.warning(line)

    def error(self, line):
        self._logger.error(line)

    def critical(self, line):
        self._logger.critical(line)
