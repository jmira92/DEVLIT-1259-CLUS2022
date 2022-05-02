# -*- mode: python; python-indent: 4 -*-
#
# Copyright 2015 Cisco Inc.
#
import mailbox

import copy
import logging
import threading
import traceback


class ThreadInfo(mailbox.MBoxThread):

    """
    Communicate between Threads.
    """

    def __init__(self, name):
        super(ThreadInfo, self).__init__(name)

        self._running_threads = []
        self.register_op('add_thread', self._add_thread)
        self.register_op('del_thread', self._del_thread)
        self.register_op('del_all_thread', self._del_all_thread)
        self.register_op('thread_finished', self._thread_finished)
        self.register_op('thread_failed', self._thread_failed)
        self.register_op('get_running_threads', self._get_running_threads)

        self._finish_callbacks = []
        self.register_op('finish', self._finish)
        self.register_op('reg_finish', self._reg_finish)
        self.register_op('unreg_finish', self._unreg_finish)

    #
    # I N T E R N A L   M E T H O D S
    #
    def _add_thread(self, data):
        self._running_threads.append(data)

    def _del_thread(self, data):
        self._running_threads[:] = [
            x for x in self._running_threads if not (x['i'] == data['i'] and
                                                     x['main'] == data['main'])]

    def _del_all_thread(self, i):
        self._running_threads[:] = [
            x for x in self._running_threads if not x['i'] == i]

    def _thread_finished(self, i):
        for x in self._running_threads:
            if 'i' in x and x['i'] == i:
                x['status'] = 'finished'

    def _thread_failed(self, args):
        i, error = args
        for x in self._running_threads:
            if 'i' in x and x['i'] == i:
                x['status'] = 'failed'
                x['error'] = error

    def _get_running_threads(self, args):
        return copy.deepcopy(self._running_threads)

    def _finish(self, _data):
        for x in self._finish_callbacks:
            try:
                x()
            except Exception:
                logging.error(traceback.format_exc())

    def _reg_finish(self, cbfun):
        if cbfun in self._finish_callbacks:
            pass
        else:
            self._finish_callbacks.append(cbfun)

    def _unreg_finish(self, cbfun):
        self._finish_callbacks[:] = [x for x in self._finish_callbacks
                                     if x != cbfun]

    #
    # P U B L I C   M E T H O D S
    #

    def add_thread(self, tname):
        """Inform NCS that a new User thread has been created."""
        self.cast('add_thread', tname)

    def del_thread(self, tname):
        """Inform NCS that a User thread has been terminated."""
        self.cast('del_thread', tname)

    def del_all_thread(self, i):
        """Delete all information about a particular thread."""
        self.cast('del_all_thread', i)

    def thread_finished(self, i):
        """Mark thread as finsihed."""
        self.cast('thread_finished', i)

    def thread_failed(self, i, error):
        """Mark thread as failed."""
        self.call0('thread_failed', (i, error))

    def get_running_threads(self, frm):
        """Get all thread information."""
        (_msg, result, _sender) = self.call('get_running_threads', None, frm)
        return result

    def finish(self, data=None):
        """Invoke all registered 'finish' callback functions."""
        self.cast('finish', data)

    def reg_finish(self, cbfun):
        """Register a 'finish' callback function."""
        self.cast('reg_finish', cbfun)

    def unreg_finish(self, cbfun):
        """Unregister a 'finish' callback function."""
        self.cast('unreg_finish', cbfun)

#
# C O M P O N E N T   T H R E A D
#


# Each NCS package component class will be started as a ComponentThread


class ComponentThread(threading.Thread):

    def __init__(self, n, component, main_class, *args, **kwds):
        super(ComponentThread, self).__init__(
            name='ComponentThread:{0}'.format(component))

        self.n = n
        self.main_class = main_class
        self._user_threads = kwds['user_threads']
        self._component_threads = kwds['component_threads']
        self._logger = logging.getLogger(kwds['vmid'])
        self._main_q = kwds['main_q']
        self._i = kwds['i']

        # Import the component module
        if "." not in main_class:
            self._logger.warning("Deprecation warning: " +
                                 "class name should be fully qualified.")
            modname = component
            classname = main_class
        else:
            modname, classname = main_class.rsplit(".", 1)
        mod = __import__(modname)
        for m in modname.split('.')[1:]:
            mod = getattr(mod, m)

        self.dbg('Imported: ' + modname)
        self.dbg('*** main_class: ' + str(classname))

        # Get the 'main class' from module
        self.dbg('Getattr ' + classname + ' from: ' + modname)
        main_cls = getattr(mod, classname)

        # Create the 'main class' object
        self.dbg('Creating class: ' + str(classname))
        self.main = main_cls(*args, **kwds)
        self.dbg('Created class: ' + str(classname))

    def run(self):
        self.dbg('Starting Main Class: ' + self.main_class)
        try:
            if hasattr(self.main, '_run'):
                self.main._run()
            else:
                self.main.run()
            self._component_threads.thread_finished(self.n)
        except Exception as e:
            self._logger.error(str(e))
            self._logger.error(traceback.format_exc())
            component_ready_event = getattr(self.main, '_ready', None)
            self._main_q.cast('component_error', (self._i, str(e),
                                                  component_ready_event))

        self.dbg('Finished: ' + self.main_class)

        # Clean up any 'running_threads' info
        self._user_threads.del_all_thread(self.n)

    def dbg(self, line):
        self._logger.debug(line)

    def set_log_level(self, log_level):
        if hasattr(self.main, 'set_log_level'):
            self.main.set_log_level(log_level)
