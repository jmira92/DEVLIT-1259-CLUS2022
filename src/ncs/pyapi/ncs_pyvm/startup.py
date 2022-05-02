# -*- mode: python; python-indent: 4 -*-
#
# Copyright 2015 Cisco Inc.
#
import mailbox
import ncsctlsock
from ncsthreads import ThreadInfo, ComponentThread
import ncs.application
import ncs.log
import ncs.util
import _ncs

import os
import sys
import getopt
import logging
import multiprocessing
import traceback


#
# C O M M A N D   L I N E   P R O C E S S I N G
#

def usage():
    print("Usage help here...")


def parse_command_line(args):
    vmid = "default"
    log_level = logging.INFO
    log_file = None
    # Parse the command line arguments
    try:
        opts, args = getopt.getopt(args[1:],
                                   "l:f:hi:",
                                   ["log-level=", "log-file=", "help", "id="])

        for opt, arg in opts:
            if opt in ("-h", "--help"):
                usage()
                sys.exit()
            elif opt in ("-l", "--log-level"):
                log_level = to_log_level(arg)
            elif opt in ("-f", "--log-file"):
                log_file = arg
            elif opt in ("-i", "--id"):
                vmid = arg

        return (vmid, log_file, log_level)

    except getopt.GetoptError:
        usage()
        sys.exit(2)


#
# L O G G I N G - Setup logging, or not...
#
def setup_logging(vmid, log_file, log_level):
    global logger

    if log_file:
        ncs.log.init_logging(vmid, log_file, log_level)
    else:
        logging.basicConfig(
            level=log_level, format=ncs.log.log_format(),
            datefmt=ncs.log.log_datefmt())

    logger = logging.getLogger(vmid)
    logger.setLevel(log_level)


def dbg(line):
    logger.debug(line)


def info(line):
    logger.info(line)


def warning(line):
    logger.warning(line)


def error(line):
    logger.error(line)


def to_log_level(lvl):
    if lvl == 'debug':
        return logging.DEBUG
    elif lvl == 'info':
        return logging.INFO
    elif lvl == 'warning':
        return logging.WARNING
    elif lvl == 'error':
        return logging.ERROR
    elif lvl == 'critical':
        return logging.CRITICAL
    elif lvl == 'off':
        return logging.CRITICAL + 1
    else:
        return logging.WARNING


#
# M A I N
#
# This is the main routine that communicates with NCS.
# It will start all the ComponentWorkers.
# The communication to NCS is done by sending Json messages
# as encoded dictionaries. Each message, at least, contain
# a name ('msg') and the Id ('id') that was allocated by NCS
# and is used for identifying this particular Python VM.


class Main(object):

    def __init__(self, vmid):
        self.vmid = vmid
        self.exit = False
        self._components_started = False
        self._components = []

        # Return channel for Mailbox communication
        self._q = mailbox.queue.Queue()

    def _start_threadinfo(self, name):
        tinfo = ThreadInfo(name)
        tinfo.start()
        return tinfo

    def _stop_threadinfos(self, tinfos):
        for tinfo in tinfos:
            tinfo.finish()
            tinfo.stop()
        for tinfo in tinfos:
            tinfo.join()

    class _CompMsgSink(mailbox.MBoxThread):
        def __init__(self, main):
            super(Main._CompMsgSink, self).__init__('_CompMsgSink')
            self.main = main
            self.register_op('component_error', self._component_error)

        def _component_error(self, args):
            i, error, component_ready_event = args
            # check components_started before mutating to avoid race
            components_started = self.main._components_started
            self.main.component_threads.thread_failed(i, error)
            if component_ready_event is not None:
                component_ready_event.set()
            if components_started:
                # if not started yet, then the main thread will do it for us
                self.main.send_started_classes_msg()

    def run(self):
        info('Starting...')
        self.callback_channel = self._start_threadinfo('callback_channel')
        self.user_threads = self._start_threadinfo('user_threads')
        self.component_threads = self._start_threadinfo('component_threads')
        self.msg_sink = Main._CompMsgSink(self)
        self.msg_sink.start()
        info('Started')

        with ncsctlsock.SafeControlSocket(self.vmid) as s:
            self.s = s
            # The main loop
            while not self.exit:
                dbg('Waiting for Json msgs')
                msg = s.recv_json()
                dbg('Got Json msg: ' + str(msg))
                self.dispatch(msg)

        info('Exiting...')
        self.msg_sink.stop()
        self._stop_threadinfos([self.callback_channel, self.user_threads,
                                self.component_threads])

        for ct in self._components:
            info('joining ComponentThread ' + ct.main_class + '...')
            ct.join()
            info('joined ComponentThread ' + ct.main_class)

        info('Exited')

    def dispatch(self, msg):
        try:
            if msg['msg'] == 'echo':
                dbg('GOT ECHO string: ' + msg['string'])
                self.send_echo_msg(msg)

            elif msg['msg'] == 'start_components':
                dbg('GOT START_COMPONENTS ...')
                self.start_components(msg)

            elif msg['msg'] == 'set_log_file_prefix':
                dbg('GOT SET_LOG_FILE_PREFIX ...')
                self.set_log_file_prefix(msg)

            elif msg['msg'] == 'set_log_level':
                dbg('GOT SET_LOG_LEVEL ...')
                self.set_log_level(msg)

            elif msg['msg'] == 'reopen_logs':
                dbg('GOT REOPEN_LOGS ...')
                self.reopen_logs(msg)

            elif msg['msg'] == 'get_status':
                dbg('GOT GET_STATUS')
                self.return_status(msg)

            elif msg['msg'] == 'stop':
                dbg('GOT STOP')
                self.exit = True

            else:
                dbg('GOT UNKNOWN ...' + msg['msg'])

        except Exception:
            warning('dispatch exception: ' + str(sys.exc_info()[0]))
            warning(traceback.format_exc())
            raise

    def start_components(self, msg):
        i = 0
        components = []
        for c in msg['components']:
            pname = c['pname']
            cname = c['cname']
            classes = c['classes']
            dbg('cname=' + cname)
            dbg('classes=' + str(classes))
            for main in classes:
                i += 1
                try:
                    ct = ComponentThread(
                        i, cname, main, i=i, vmid=self.vmid,
                        component_threads=self.component_threads,
                        user_threads=self.user_threads,
                        callback_channel=self.callback_channel,
                        pname=pname, cname=cname, main_q=self.msg_sink)
                    dbg('Created Thread: ' + main)
                    ct.start()
                    components.append(ct)
                    self.component_threads.add_thread({'pname': pname,
                                                       'cname': cname,
                                                       'main': main,
                                                       'status': 'running',
                                                       'i': i})
                except Exception:
                    traceback.print_exc(file=sys.stdout)
                    e = traceback.format_exc()
                    e1 = e.splitlines()[-1]
                    error(e)
                    dbg('Failed to create thread: ' + main + ': ' + e1)
                    self.component_threads.add_thread({'pname': pname,
                                                       'cname': cname,
                                                       'main': main,
                                                       'status': 'failed',
                                                       'error': e1})

        for ct in components:
            if hasattr(ct.main, '_ready'):
                ct.main._ready.wait()
        self._components_started = True
        self.send_started_classes_msg()
        self._components = components

    def set_log_file_prefix(self, msg):
        global logger
        old_handler = ncs.log.log_handler()
        log_file = msg['log_file_prefix']
        log_level = logger.level
        setup_logging(self.vmid, log_file, log_level)
        if old_handler is not None:
            logging.root.removeHandler(old_handler)
            old_handler.close()

    def set_log_level(self, msg):
        global logger
        level_str = msg['log_level']
        level = to_log_level(level_str)

        def do_set_log_level(ll, name):
            dbg('log_level {0} -> {1} on {2}'.format(ll.level, level, name))
            if level != ll.level:
                info('Setting log level {0} on {1}'.format(level_str, name))
                ll.setLevel(level)
            else:
                info('Keeping log level {0} on {1}'.format(level_str, name))

        do_set_log_level(logger, self.vmid)
        do_set_log_level(logging.root, '{} (root)'.format(self.vmid))
        for component in self._components:
            component.set_log_level(level)

    def reopen_logs(self, msg):
        if ncs.log.log_handler() is not None:
            info('Reopening log files')
            ncs.log.reopen_logs()

    def return_status(self, msg):
        req_id = msg['req_id']
        result = self.component_threads.get_running_threads(self._q)
        xx = self.get_extra_threads()
        dbg('Extra threads = ' + str(xx))
        result.extend(xx)
        self.send_status_msg(req_id, result)

    def get_extra_threads(self):
        dbg('Extra threads: calling mailbox...')
        res = self.user_threads.get_running_threads(self._q)
        dbg('Extra threads: returned from mailbox, now getting result...')
        dbg('Extra threads: result = ' + str(res))
        return res

    # Control messages
    def send_echo_msg(self, msg):
        self.send_msg(msg)

    def send_status_msg(self, req_id, result):
        self.send_msg({'msg': 'status',
                       'req_id': req_id,
                       'result': result})

    def send_started_classes_msg(self):
        rt = self.component_threads.get_running_threads(self._q)
        running = [z for z in rt if z['status'] == 'running']
        failed = [z for z in rt if z['status'] == 'failed']
        self.send_msg({'msg': 'started_classes',
                       'running': running,
                       'failed': failed})

    def send_msg(self, msg):
        msg['id'] = self.vmid
        self.s.send_json(msg)


def main():
    (vmid, log_file, log_level) = parse_command_line(sys.argv)

    setup_logging(vmid, log_file, log_level)

    info('Python {}'.format(sys.version.replace('\n', '')))

    if sys.version_info < (3, 7):
        error('Python 3.7 or greater required')
        sys.exit(1)
    else:
        # The fork start method should be considered unsafe as it can
        # lead to crashes of the subprocess. See bpo-33725. This was
        # changed to be default in python 3.8 because of this.
        if sys.platform == 'darwin' and sys.version_info < (3, 8):
            multiprocessing.set_start_method('spawn')

        if not ncs.util.set_kill_child_on_parent_exit():
            callpoint_model = os.getenv('NCS_PYTHON_CALLPOINT_MODEL', None)
            if callpoint_model == 'multiprocessing':
                info('failed to init process cleanup, falling back to '
                     'threading callpoint-model')
                os.putenv('NCS_PYTHON_CALLPOINT_MODEL', 'threading')
        info('Using callpoint-model: {}'.format(
            os.getenv('NCS_PYTHON_CALLPOINT_MODEL', 'threading')))

        # Start the main loop in a separate thread
        main = Main(vmid)
        main.run()
        info('Goodbye cruel world...')
        logging.shutdown()
        sys.exit(1)


if __name__ == '__main__':
    main()
