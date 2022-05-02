import threading
import logging
import queue


log = logging.getLogger(__name__)


class MBoxThread(threading.Thread):
    def __init__(self, name):
        super(MBoxThread, self).__init__(name=name)
        self.q = queue.Queue()
        self.fns = dict(_stop_=self._on_stop)
        self.running = True

    def _on_stop(self, args):
        self.running = False

    def register_op(self, op, fn):
        if self.fns.get(op) is not None:
            raise Exception('operation %s already registered' % (str(op)))
        self.fns[op] = fn

    def call0(self, op, args, timeout=None):
        '''This is slow. Don't use it for anything real'''
        rq = queue.Queue()
        self.q.put((op, args, rq))
        return rq.get(timeout=timeout)

    def call(self, op, args, frm, timeout=None):
        self.q.put((op, args, frm))
        return frm.get(timeout=timeout)

    def call_async(self, op, args, frm):
        self.q.put((op, args, frm))

    def cast(self, op, args):
        self.q.put((op, args, None))

    def stop(self):
        self.q.put(('_stop_', None, None))

    def run(self):
        while self.running:
            (op, args, frm) = self.q.get()
            fn = self.fns.get(op, None)
            if not fn:
                log.debug('discarding %s' % (str((op, args, frm))))
            res = fn(args)
            if frm:
                frm.put((op, res, None))
