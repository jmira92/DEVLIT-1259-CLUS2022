"""Module for building NCS applications."""
import threading
import traceback
import functools
import multiprocessing
import os
import signal
import logging

from . import dp
from . import log
from . import maapi
from . import maagic
from . import pool
from . import util

import _ncs
import ncs_pyvm


def _run_app_start_funs(app_funs, stop_funs, state):
    for (start_fun, stop_fun) in app_funs:
        fun_data = start_fun(state)
        stop_funs.append((stop_fun, fun_data))


def _run_app_stop_funs(stop_funs):
    for (stop_fun, fun_data) in stop_funs:
        stop_fun(fun_data)


class _StateManager(dp.StateManager):
    def __init__(self, app):
        super(_StateManager, self).__init__(app.log)
        self._app = app
        self._app_actions_cls = []
        self._app_services_cls = []
        self._app_nano_services_cls = []
        self._app_funs = []

    def setup(self, state, previous_state):
        actions = state['_app_actions'] = []
        services = state['_app_services'] = []
        nano_services = state['_app_nano_services'] = []
        app_funs = state['_app_funs'] = []

        # pool of MAAPI connections used for service creation. a pool
        # is used as the connection is required to exist _after_ the
        # create callback is executed to generate reverse diffs
        maapi_pool_cfg = pool.PoolConfig(
            min_size=0, max_size=0, idle_timeout_s=60)
        maapi_pool_cb = _MaapiCb(
            self.log, state['ip'], state['port'], state['path'])
        state['maapi_pool'] = pool.Pool(
            state['log'], 'Maapi', maapi_pool_cfg, maapi_pool_cb)

        for (action_pt, action_cls, init_args) in self._app_actions_cls:
            a = action_cls(state, action_pt, None, init_args)
            a.start()
            if previous_state is None:
                self._app.add_running_thread(action_pt + ' (action)')
            actions.append(a)

        for (service_pt, service_cls, init_args) in self._app_services_cls:
            s = service_cls(state, service_pt, None, init_args)
            s.start()
            if previous_state is None:
                self._app.add_running_thread(service_pt + ' (service)')
            services.append(s)

        for (service_pt, service_cls, component_type,
             mystate, init_args) in self._app_nano_services_cls:
            n = service_cls(state, service_pt, component_type, mystate, None,
                            init_args)
            n.start()
            if previous_state is None:
                self._app.add_running_thread(service_pt + ' (nano service)')
            nano_services.append(n)

        _run_app_start_funs(self._app_funs, app_funs, state)

    def teardown(self, state, finished):
        _run_app_stop_funs(state['_app_funs'])

        for a in state['_app_actions']:
            a.stop()
            if finished is True:
                self._app.del_running_thread(a.actionpoint + ' (action)')

        for s in state['_app_services']:
            s.stop()
            if finished is True:
                self._app.del_running_thread(s.servicepoint + ' (service)')

        for n in state['_app_nano_services']:
            n.stop()
            if finished is True:
                self._app.del_running_thread(n.servicepoint + ' (nano service)')

        state['maapi_pool'].stop()

    def register_action(self, action_pt, action_cls, init_args=None):
        self._app_actions_cls.append((action_pt, action_cls, init_args))

    def register_service(self, service_pt, service_cls, init_args=None):
        self._app_services_cls.append((service_pt, service_cls, init_args))

    def register_nano_service(self, service_pt, service_cls, component_type,
                              state, init_args=None):
        self._app_nano_services_cls.append((service_pt, service_cls,
                                           component_type, state, init_args))

    def register_fun(self, start_fun, stop_fun):
        self._app_funs.append((start_fun, stop_fun))


class _AppInfo(object):
    def __init__(self, ncs_id, ncs_pname, ncs_cname, parent_q):
        self.ncs_id = ncs_id
        self.ncs_pname = ncs_pname
        self.ncs_cname = ncs_cname
        self.parent_q = parent_q

        self.app_funs = []
        self.trans_cb_cls = None


class _AppWorker(object):
    def __init__(self, model, cls, app_info, worker_id,
                 app_actions_cls, app_services_cls, app_nano_services_cls):
        self._model = model
        self._cls = cls

        # delay deamon and worker construction until the process is
        # spawned, will not survive a pickle
        self._worker = None
        self.dd = None

        self._app_info = app_info
        self._worker_id = worker_id
        self._app_actions_cls = app_actions_cls
        self._app_services_cls = app_services_cls
        self._app_nano_services_cls = app_nano_services_cls
        self._callpoints = [cp[0] for cp in app_actions_cls]
        self._callpoints.extend(cp[0] for cp in app_services_cls)
        self._callpoints.extend(cp[0] for cp in app_nano_services_cls)

        # event used in wait() by the caller to ensure worker is up
        # before continuing.
        event_cls = getattr(globals()[model], 'Event')
        self._ready = event_cls()
        self._stop = event_cls()

    def start(self):
        name = '_AppWorker {}'.format(', '.join(self._callpoints))
        worker_cls = getattr(globals()[self._model], self._cls)
        worker = worker_cls(name=name, target=self.run)
        worker.start()
        self._worker = worker

    def add_running_thread(self, thread):
        raise NotImplementedError()

    def del_running_thread(self, thread):
        raise NotImplementedError()

    def wait(self):
        self._ready.wait()

    def stop(self):
        raise NotImplementedError()

    def join(self, timeout_s=None):
        if self._worker is None:
            # worker failed to start or already joined, nothing to
            # join
            return

        self._worker.join(timeout_s)
        if getattr(self._worker, 'exitcode', 0) is None:
            # process not exited yet, force kill
            os.kill(self._worker.pid, signal.SIGKILL)
            self._worker.join()
        self._worker = None

    def run(self):
        try:
            self._run()
        except Exception:
            self._ready.set()
            raise

    def set_log_level(self, level):
        # no need to set the level, same process so shared with the
        # parent.
        pass

    def _run(self):
        # setup name inside of run to get the PID right
        self._daemon_name = self._mk_daemon_name()

        self._state_mgr = _StateManager(self)
        self._state_mgr._app_actions_cls = self._app_actions_cls
        self._state_mgr._app_services_cls = self._app_services_cls
        self._state_mgr._app_nano_services_cls = self._app_nano_services_cls
        self._state_mgr._app_funs = list(self._app_info.app_funs)

        self._main()

    def _main(self):
        self.log.debug('{} for callpoints {} starting...'.format(
            self.__class__.__name__, ', '.join(self._callpoints)))
        self.dd = dp.Daemon(self._daemon_name, self.log,
                            state_mgr=self._state_mgr)
        if self._app_info.trans_cb_cls is not None:
            self.dd.register_trans_cb(self._app_info.trans_cb_cls)
        self.dd.load_schemas()
        self.dd.start()
        self._ready.set()

        while True:
            msg = self._get_msg()
            if msg[0] == 'stop':
                break

            if msg[0] == 'set_log_level':
                log.set_log_level(self._app_info.ncs_id, msg[1])
                self._put_msg(('set_log_level', True))

        self.dd.finish()
        self.dd.join()

    def _finish(self):
        self._stop.set()

    def _get_msg(self):
        raise NotImplementedError()

    def _put_msg(self):
        raise NotImplementedError()

    def _mk_daemon_name(self):
        return 'ncs-dp-{0}-{1}:{2}:{3}'.format(
            os.getpid(), self._app_info.ncs_pname, self._app_info.ncs_cname,
            self._worker_id)


class _ThreadAppWorker(_AppWorker):
    def __init__(self, app, app_info, worker_id,
                 app_actions_cls, app_services_cls, app_nano_services_cls):
        _AppWorker.__init__(
            self, 'threading', 'Thread', app_info, worker_id,
            app_actions_cls, app_services_cls, app_nano_services_cls)

        self.log = app.log
        self._app = app

    def add_running_thread(self, thread):
        self._app.add_running_thread(thread)

    def del_running_thread(self, thread):
        self._app.del_running_thread(thread)

    def stop(self):
        self._stop.set()

    def _get_msg(self):
        self._stop.wait()
        return ('stop', )

    def _put_msg(self):
        # do nothing, no need to use messages to communicate
        pass


class _ProcessAppWorker(_AppWorker):
    def __init__(self, app_info, worker_id,
                 app_actions_cls, app_services_cls, app_nano_services_cls):
        super(_ProcessAppWorker, self).__init__(
            'multiprocessing', 'Process', app_info, worker_id,
            app_actions_cls, app_services_cls, app_nano_services_cls)
        # keep the log level from the root logger before spawning a
        # process, reset on OS X
        self._initial_log_level = logging.root.level

        # using a pipe for sync calls and the parent queue to send
        # data from all children to the parent
        self._ppipe, self._cpipe = multiprocessing.Pipe()

    def add_running_thread(self, thread):
        self._parent_put(('add_running_thread', thread))

    def del_running_thread(self, thread):
        self._parent_put(('del_running_thread', thread))

    def stop(self):
        self._ppipe.send(('stop', ))

    def set_log_level(self, level):
        self._ppipe.send(('set_log_level', level))
        # wait for response from worker, to ensure log level has taken
        # effect when this call release
        self._ppipe.recv()

    def _run(self):
        # setup child cleanup as early as possible
        util.set_kill_child_on_parent_exit()

        # patch up print in the child forwarding to the parent process
        # as stdout seems to be closed.
        def _parent_print(*args, **kwargs):
            self._parent_put(('print', args, kwargs))
        import builtins
        builtins.print = _parent_print

        # not timestamping in the child to avoid double timestamps,
        # parent will timestamp when processing the logs.
        old_log_handler = log.log_handler()
        if old_log_handler is not None:
            logging.root.removeHandler(old_log_handler)
        log_handler = log.ParentProcessLogHandler(self._app_info.parent_q)
        log_handler.setFormatter(log.mk_log_formatter())
        logging.root.addHandler(log_handler)
        log.set_log_level(self._app_info.ncs_id, self._initial_log_level)
        self.log = log.Log(logging.getLogger(self._app_info.ncs_id),
                           add_timestamp=False)
        super(_ProcessAppWorker, self)._run()

    def _get_msg(self):
        return self._cpipe.recv()

    def _put_msg(self, msg):
        self._cpipe.send(msg)

    def _parent_put(self, msg):
        self._app_info.parent_q.put((os.getpid(), msg))


class Application(ncs_pyvm.NcsPyVM):
    """Class for easy implementation of an NCS application.

    This class is intended to be sub-classed and used as a 'component class'
    inside an NCS package. It will be instantiated by NCS when the package
    is loaded. The setup() method should to be implemented to register
    service- and action callbacks. When NCS stops or an error occurs,
    teardown() will be called. A 'log' attribute is available for logging.

    Example application:

        from ncs.application import Application, Service, NanoService
        from ncs.dp import Action

        class FooService(Service):
            @Service.create
            def cb_create(self, tctx, root, service, proplist):
                # service code here

        class FooNanoService(NanoService):
            @NanoService.create
            def cb_nano_create(self, tctx, root, service, plan, component,
                               state, proplist, compproplist):
                # service code here

        class FooAction(Action):
            @Action.action
            def cb_action(self, uinfo, name, kp, input, output):
                # action code here

        class MyApp(Application):
            def setup(self):
                self.log.debug('MyApp start')
                self.register_service('myservice-1', FooService)
                self.register_service('myservice-2', FooService, 'init_arg')
                self.register_nano_service('nano-1', 'myserv:router',
                                           'myserv:ntp-initialized',
                                           FooNanoService)
                self.register_action('action-1', FooAction)

            def teardown(self):
                self.log.debug('MyApp finish')
    """

    # Time Application will wait for _ProcessAppWorker to join during
    # _finish before sending a SIGKILL
    APP_WORKER_STOP_TIMEOUT_S = 1

    def __init__(self, *args, **kwds):
        """Initialize an Application object.

        Don't try to initialize this object. I will be done by NCS.
        """
        super(Application, self).__init__(*args, **kwds)
        self.log = log.Log(self)

        # ready event used by ComponentThread to detect component
        # application startup being complete.
        self._ready = threading.Event()

        self.reg_finish(self._finish)
        self._app_actions_cls = []
        self._app_services_cls = []
        self._app_nano_services_cls = []

        # queue used to process messages from the _AppWorker child
        # proccess
        self._child_q = multiprocessing.Queue()
        self._app_info = _AppInfo(
            self._ncs_id, self._ncs_pname, self._ncs_cname, self._child_q)

        self._workers = []
        self._app_stop_funs = []

    def setup(self):
        """Application setup method.

        Override this method to register actions and services. Any other
        initialization could also be done here. If the call to this method
        throws an exception the teardown method will be immediately called
        and the application shutdown.
        """
        pass

    def teardown(self):
        """Application teardown method.

        Override this method to clean up custom resources allocated in
        setup().
        """
        pass

    def create_daemon(self, name=None):
        """Name the underlying dp.Daemon object (deprecated)"""
        pass

    def register_trans_cb(self, trans_cb_cls):
        """Register a transaction callback class.

        If a custom transaction callback implementation is needed, call this
        method with the transaction callback class as the 'trans_cb_cls'
        argument.
        """
        self._app_info.trans_cb_cls = trans_cb_cls

    def register_service(self, servicepoint, service_cls, init_args=None):
        """Register a service callback class.

        Call this method to register 'service_cls' as the service callback
        class for service point 'servicepoint'. 'service_cls' should be a
        subclass of Service. If the optional argument 'init_args' is supplied
        it will be passed in to the init() method of the subclass.
        """
        self._app_services_cls.append((servicepoint, service_cls, init_args))

    def register_nano_service(self, servicepoint, componenttype,
                              state, nano_service_cls, init_args=None):
        """Register a nano service callback class.

        Call this method to register 'nano_service_cls' as the nano service
        callback class for service point 'servicepoint'.
        'nano service_cls' should be a subclass of NanoService.
        If the optional argument 'init_args' is supplied
        it will be passed in to the init() method of the subclass.
        """
        self._app_nano_services_cls.append((servicepoint, nano_service_cls,
                                            componenttype, state, init_args))

    def register_action(self, actionpoint, action_cls, init_args=None):
        """Register an action callback class.

        Call this method to register 'action_cls' as the action callback
        class for action point 'actionpoint'. 'action_cls' should be a
        subclass of dp.Action. If the optional argument 'init_args' is
        supplied it will be passed in to the init() method of the subclass.
        """
        self._app_actions_cls.append((actionpoint, action_cls, init_args))

    def register_fun(self, start_fun, stop_fun):
        """Register custom start and stop functions.

        Call this method to register a start and stop function that
        will be called with a dp.Daemon.State during application
        setup.

        Example start and stop functions:

            def my_start_fun(state):
                state.log.info('my_start_fun START')
                return (state, time.time())

            def my_stop_fun(fun_data):
                (state, start_time) = fun_data
                state.log.info('my_start_fun started {}'.format(start_time))
                state.log.info('my_start_fun STOP')
        """
        self._app_info.app_funs.append((start_fun, stop_fun))

    def set_log_level(self, log_level):
        """Set log level for all workers (only relevant for
        _ProcessAppWorker)"""
        for worker in self._workers:
            worker.set_log_level(log_level)

    def _run(self):
        # in case of multiprocessing worker, ensure schema gets loaded
        # in the parent process
        with maapi.Maapi(load_schemas=True):
            pass

        try:
            self.setup()
        except Exception:
            self.log.error("Error in setup method")
            raise
        else:
            groups = self._group_callpoints()
            self._start(groups)
        finally:
            self.teardown()

    def _group_callpoints(self):
        model = os.getenv('NCS_PYTHON_CALLPOINT_MODEL', 'threading')
        if model == 'threading':
            return [(self._app_actions_cls, self._app_services_cls,
                     self._app_nano_services_cls)]

        groups = []
        for app_action in self._app_actions_cls:
            groups.append(([app_action], [], []))

        for app_service in self._app_services_cls:
            groups.append(([], [app_service], []))

        for app_nano_service in self._app_nano_services_cls:
            groups.append(([], [], [app_nano_service]))
        return groups

    def _start(self, groups):
        if len(groups) == 1:
            # single group, no need to create a sub process running
            # everything, use a single thread worker
            actions, services, nano_services = groups[0]
            worker = _ThreadAppWorker(self, self._app_info, 0,
                                      actions, services, nano_services)
            self._workers.append(worker)
        else:
            for group in groups:
                actions, services, nano_services = group
                worker = _ProcessAppWorker(self._app_info, len(self._workers),
                                           actions, services, nano_services)
                self._workers.append(worker)

        if len(self._workers) == 0:
            # no workers specified, run the start functions with an
            # empty state in the current process.
            _run_app_start_funs(
                self._app_info.app_funs, self._app_stop_funs, self)
        else:
            # workers started in sync to get a predictable order
            # (initial implementation did a start() loop and then a
            # wait() loop but order might be of significance)
            for worker in self._workers:
                worker.start()
                worker.wait()

        self._ready.set()
        try:
            self._main()
        except Exception:
            self._finish()

    def _main(self):
        (msg_pid, msg) = self._child_q.get()
        while msg is not None:
            self._process_child_msg(msg_pid, msg)
            (msg_pid, msg) = self._child_q.get()

    def _process_child_msg(self, msg_pid, msg):
        if msg[0] == 'raw_log':
            self._raw_log(msg[1])
        elif msg[0] == 'print':
            print(*msg[1], **msg[2])
        elif msg[0] == 'add_running_thread':
            self.add_running_thread(msg[1])
        elif msg[0] == 'del_running_thread':
            self.del_running_thread(msg[1])
        else:
            self.log.error('unsupported child message {}'.format(msg))

    def _finish(self):
        _run_app_stop_funs(self._app_stop_funs)
        for worker in self._workers:
            worker.stop()
        for worker in self._workers:
            worker.join(Application.APP_WORKER_STOP_TIMEOUT_S)

        self._child_q.put((0, None))

    def _raw_log(self, msg):
        log_file = log.log_file()
        log_handler = log.log_handler()
        if log_file is None or log_handler is None:
            return

        log_handler.acquire()
        try:
            log_file.write(msg)
            log_file.flush()
        finally:
            log_handler.release()


class _MaapiCb(pool.PoolItemCb):
    def __init__(self, log, ip, port, path):
        super(_MaapiCb, self).__init__(log)

        self._ip = ip
        self._port = port
        self._path = path

    def create_item(self):
        self.log.debug('_MaapiCb create connection to {0}:{1} / {2}'.format(
            self._ip, self._port, self._path))
        return maapi.Maapi(self._ip, self._port, self._path)

    def delete_item(self, m):
        self.log.debug('_MaapiCb close connection')
        m.close()


class Service(object):
    """Service callback.

    This class makes it easy to create and register service callbacks by
    subclassing it and implementing some of the service callbacks.
    """

    def __init__(self, daemon, servicepoint, log=None, init_args=None):
        """Initialize this object.

        The 'daemon' argument should be a Daemon instance. 'servicepoint'
        is the name of the tailf:servicepoint to manage. Argument 'log' can
        be any log object, and if not set the Daemon log will be used.
        'init_args' may be any object that will be passed into init() when
        this object is constructed. Lastly, the low-level function
        dp.register_service_cb() will be called.

        When creating a service callback using Application.register_service
        there is no need to manually initialize this object as it is then
        done automatically.
        """
        self.servicepoint = servicepoint

        daemon_d = dp._daemon_as_dict(daemon)
        ctx = daemon_d['ctx']
        self.log = log or daemon_d['log']
        self._maapi_pool = daemon['maapi_pool']

        if init_args:
            self.init(init_args)

        _ncs.dp.register_service_cb(ctx, servicepoint, self)

    def init(self, init_args):
        """Custom initialization.

        When registering a service using Application this method will be
        called with the 'init_args' passed into the register_service()
        function.
        """
        pass

    @property
    def maapi(self):
        raise Exception('maapi no longer supported, see maagic.get_maapi')

    @staticmethod
    def create(fn):
        """Decorator for the cb_create callback.

        Using this decorator alters the signature of the cb_create callback
        and passes in maagic.Node objects for root and service.
        The maagic.Node objects received in 'root' and 'service' are backed
        by a MAAPI connection with the FASTMAP handle attached. To update
        'proplist' simply return it from this function.

        Example of a decorated cb_create:

            @Service.create
            def cb_create(self, tctx, root, service, proplist):
                pass

        Callback arguments:

        * tctx - transaction context (TransCtxRef)
        * root -- root node (maagic.Node)
        * service -- service node (maagic.Node)
        * proplist - properties (list(tuple(str, str)))
        """
        @functools.wraps(fn)
        def wrapper(self, tctx, kp, proplist, th):
            m = self._maapi_pool.take_item()
            try:
                trans = m.attach(th)
                root = maagic.get_root(trans, shared=True)
                service = maagic.get_node(trans, kp, shared=True)
                pl = fn(self, tctx, root, service, proplist)
                if pl is not None:
                    _ncs.dp.service_reply_proplist(tctx, pl)
            except Exception as e:
                self.log.error(e)
                self.log.error(traceback.format_exc())
                raise
            finally:
                m.detach(th)
                self._maapi_pool.return_item(m)
        return wrapper

    @staticmethod
    def pre_lock_create(fn):
        """Decorator for the cb_pre_lock_create callback.

        For details see Service.create decorator.

        Please note that the cb_pre_lock_create callback is deprecated.
        Use the cb_create callback instead.
        """
        return Service.create(fn)

    @staticmethod
    def pre_modification(fn):
        """Decorator for the cb_pre_modification callback.

        Using this decorator alters the signature of the cb_pre_modification.
        callback and passes in a maagic.Node object for root.
        This method is invoked outside FASTMAP. To update 'proplist' simply
        return it from this function.

        Example of a decorated cb_pre_modification:

            @Service.pre_modification
            def cb_pre_modification(self, tctx, op, kp, root, proplist):
                pass

        Callback arguments:

        * tctx - transaction context (TransCtxRef)
        * op -- operation (int)
        * kp -- keypath (HKeypathRef)
        * root -- root node (maagic.Node)
        * proplist - properties (list(tuple(str, str)))
        """
        @functools.wraps(fn)
        def wrapper(self, tctx, op, kp, proplist):
            m = self._maapi_pool.take_item()
            try:
                trans = m.attach(tctx)
                root = maagic.get_root(trans, shared=False)
                pl = fn(self, tctx, op, kp, root, proplist)
                if pl is not None:
                    _ncs.dp.service_reply_proplist(tctx, pl)
            except Exception as e:
                self.log.error(e)
                self.log.error(traceback.format_exc())
                raise
            finally:
                m.detach(tctx)
                self._maapi_pool.return_item(m)
        return wrapper

    @staticmethod
    def post_modification(fn):
        """Decorator for the cb_post_modification callback.

        For details see Service.pre_modification decorator.
        """
        return Service.pre_modification(fn)

    def start(self):
        """Start Service"""
        self.log.debug('Service(', self.servicepoint, ').run()')

    def stop(self):
        """Stop Service"""
        self.log.debug('Service(', self.servicepoint, ').run() finished')


class NanoService(object):
    """NanoService callback.

    This class makes it easy to create and register nano service callbacks by
    subclassing it and implementing some of the nano service callbacks.
    """

    def __init__(self, daemon, servicepoint, componenttype, state, log=None,
                 init_args=None):
        """Initialize this object.

        The 'daemon' argument should be a Daemon instance. 'servicepoint'
        is the name of the tailf:servicepoint to manage. Argument 'log' can
        be any log object, and if not set the Daemon log will be used.
        'init_args' may be any object that will be passed into init() when
        this object is constructed. Lastly, the low-level function
        dp.register_nano_service_cb() will be called.

        When creating a service callback using Application.register_nano_service
        there is no need to manually initialize this object as it is then
        done automatically.
        """
        self.servicepoint = servicepoint
        if init_args:
            self.init(init_args)

        daemon_d = dp._daemon_as_dict(daemon)
        ctx = daemon_d['ctx']
        self.log = log or daemon_d['log']
        self._maapi_pool = daemon['maapi_pool']

        _ncs.dp.register_nano_service_cb(ctx, servicepoint, componenttype,
                                         state, self)

    def init(self, init_args):
        """Custom initialization.

        When registering a service using Application this method will be
        called with the 'init_args' passed into the register_service()
        function.
        """
        pass

    @property
    def maapi(self):
        raise Exception('maapi no longer supported, see maagic.get_maapi')

    @staticmethod
    def create(fn):
        """Decorator for the cb_nano_create callback.

        Using this decorator alters the signature of the cb_create callback
        and passes in maagic.Node objects for root and service.
        The maagic.Node objects received in 'root' and 'service' are backed
        by a MAAPI connection with the FASTMAP handle attached. To update
        'proplist' simply return it from this function.

        Example of a decorated cb_create:

            @NanoService.create
            def cb_nano_create(self, tctx, root,
                               service, plan, component, state,
                               proplist, compproplist):
                pass

        Callback arguments:

        * tctx - transaction context (TransCtxRef)
        * root -- root node (maagic.Node)
        * service -- service node (maagic.Node)
        * plan -- current plan node (maagic.Node)
        * component -- plan component active for this invokation
        * state -- plan component state active for this invokation
        * proplist - properties (list(tuple(str, str)))
        * compproplist - component properties (list(tuple(str, str)))
        """
        @functools.wraps(fn)
        def wrapper(self, tctx, kp, component, state, proplist,
                    component_proplist, skp, th):
            m = self._maapi_pool.take_item()
            try:
                trans = m.attach(th)
                root = maagic.get_root(trans, shared=True)
                planpath = maagic.get_node(trans, kp, shared=True)
                planpath = maagic.cd(planpath, "../..")
                service = maagic.get_node(trans, skp, shared=True)

                # Work around the problem that identity refs are not
                # qualified with a prefix.
                (component_type, component_name) = component
                path = planpath.component._path
                cs_node = _ncs.cs_node_cd(None, "{}/type".format(path))
                component_type_str = component_type.val2str(cs_node)
                component = (component_type_str, component_name)

                path = planpath.component[component].state._path
                cs_node = _ncs.cs_node_cd(None, "{}/name".format(path))
                state = state.val2str(cs_node)

                pl = fn(self, tctx, root, service, planpath, component, state,
                        proplist, component_proplist)
                if pl is not None:
                    _ncs.dp.nano_service_reply_proplist(tctx, pl)
            except Exception as e:
                self.log.error(e)
                self.log.error(traceback.format_exc())
                raise
            finally:
                m.detach(th)
                self._maapi_pool.return_item(m)
        return wrapper

    @staticmethod
    def delete(fn):
        """Decorator for the cb_nano_delete callback.

        Using this decorator alters the signature of the cb_delete callback
        and passes in maagic.Node objects for root and service.
        The maagic.Node objects received in 'root' and 'service' are backed
        by a MAAPI connection with the FASTMAP handle attached. To update
        'proplist' simply return it from this function.

        Example of a decorated cb_create:

            @NanoService.delete
            def cb_nano_delete(self, tctx, root,
                               service, plan, component, state,
                               proplist, compproplist):
                pass

        Callback arguments:

        * tctx - transaction context (TransCtxRef)
        * root -- root node (maagic.Node)
        * service -- service node (maagic.Node)
        * plan -- current plan node (maagic.Node)
        * component -- plan component active for this invokation
        * state -- plan component state active for this invokation
        * proplist - properties (list(tuple(str, str)))
        * compproplist - component properties (list(tuple(str, str)))
        """
        @functools.wraps(fn)
        def wrapper(self, tctx, kp, component, state, proplist,
                    component_proplist, skp, th):
            m = self._maapi_pool.take_item()
            try:
                trans = m.attach(th)
                root = maagic.get_root(trans, shared=True)
                planpath = maagic.get_node(trans, kp, shared=True)
                planpath = maagic.cd(planpath, "../..")
                service = maagic.get_node(trans, skp, shared=True)

                # Work around the problem that identity refs are not
                # qualified with a prefix.
                (component_type, component_name) = component
                path = planpath.component._path
                cs_node = _ncs.cs_node_cd(None, "{}/type".format(path))
                component_type_str = component_type.val2str(cs_node)
                component = (component_type_str, component_name)

                path = planpath.component[component].state._path
                cs_node = _ncs.cs_node_cd(None, "{}/name".format(path))
                state = state.val2str(cs_node)

                pl = fn(self, tctx, root, service, planpath, component, state,
                        proplist, component_proplist)
                if pl is not None:
                    _ncs.dp.nano_service_reply_proplist(tctx, pl)
            except Exception as e:
                self.log.error(e)
                self.log.error(traceback.format_exc())
                raise
            finally:
                m.detach(th)
                self._maapi_pool.return_item(m)
        return wrapper

    def start(self):
        """Start NanoService"""
        self.log.debug('NanoService(', self.servicepoint, ').run()')

    def stop(self):
        """Stop NanoService"""
        self.log.debug('NanoService(', self.servicepoint, ').run() finished')


def get_device(node, name):
    """Get a device node by name.

    Returns a maagic node representing a device.

    Arguments:

    * node -- any maagic node with a Transaction backend or a Transaction object
    * name -- the device name (string)
    """
    root = maagic.get_root(node)
    return root.ncs__devices.ncs__device[name]


def get_ned_id(device):
    """Get the ned-id of a device.

    Returns the ned-id as a string or None if not found.

    Arguments:

    * device -- a maagic node representing the device
    """
    ned_id = _lookup_ned_id(device)
    if ned_id is None:
        with maagic.get_maapi(device).start_read_trans() as t:
            dev_t = maagic.get_node(t, device)
            ned_id = _lookup_ned_id(dev_t)
    return str(ned_id) if ned_id else None


def _lookup_ned_id(device):
    try:
        ne_type = device.device_type.ne_type
        ned_id_path = '%s/%s/ned-id' % (device.device_type._path, ne_type)
        ned_id = maagic.cd(device, ned_id_path)
        return ned_id
    except Exception:
        return None


class PlanComponent(object):
    """Service plan component.

    The usage of this class is in conjunction with a service that
    uses a reactive FASTMAP pattern.
    With a plan the service states can be tracked and controlled.

    A service plan can consist of many PlanComponent's.
    This is operational data that is stored together with the service
    configuration.
    """

    def __init__(self, planpath, name, component_type):
        """Initialize a PlanComponent."""
        self.component = planpath.plan.component.create(name)
        self.component.type = component_type

    def append_state(self, state_name):
        """Append a new state to this plan component.

        The state status will be initialized to 'ncs:not-reached'.
        """
        if state_name not in self.component.state:
            state = self.component.state.create(state_name)
            state.status = 'not-reached'

    def set_reached(self, state_name):
        """Set state status to 'ncs:reached'."""
        self.set_status(state_name, 'reached')

    def set_failed(self, state_name):
        """Set state status to 'ncs:failed'."""
        self.set_status(state_name, 'failed')

    def set_status(self, state_name, status):
        """Set state status."""
        if state_name not in self.component.state:
            raise Exception("no state '%s' in plan" % (state_name))
        state = self.component.state[state_name]
        state.status = status
