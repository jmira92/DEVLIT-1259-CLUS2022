"""Utility module, low level abstrations"""

import os
import select
import signal
import threading

import _ncs

class _KQueueParentObserver(threading.Thread):
    """Thread waiting for the parent process to exit and then runs
    on_exit_fun"""

    def __init__(self, on_exit_fun):
        super(_KQueueParentObserver, self).__init__(daemon=True)
        self._ppid = os.getppid()
        self._on_exit_fun = on_exit_fun

    def run(self):
        # wait for parent process to exit
        ev = select.kevent(self._ppid,
                           filter=select.KQ_FILTER_PROC,
                           fflags=select.KQ_NOTE_EXIT)
        kq = select.kqueue()
        try:
            kq.control([ev], 1)
        finally:
            kq.close()

        self._on_exit_fun()


def set_kill_child_on_parent_exit():
    """Multi OS variant of _ncs.set_kill_child_on_parent_exit falling back
    to kqueue if the OS supports it."""

    if _ncs.set_kill_child_on_parent_exit():
        return True

    if hasattr(select, 'kqueue'):
        pid = os.getpid()
        obs = _KQueueParentObserver(lambda: os.kill(pid, signal.SIGKILL))
        obs.start()
        return True

    return False


if __name__ == '__main__':
    import struct
    import sys
    import time
    import unittest

    class SetKillChildOnParentExitTest(unittest.TestCase):
        def test_exit_child(self):
            """Test verifying standard child exit (and not doing so
            prematurely)"""
            read_fd, write_fd = os.pipe()
            pid = os.fork()
            if pid == 0:
                self.assertTrue(set_kill_child_on_parent_exit())
                os.write(write_fd, b'i')
                os.close(write_fd)

                # child, wait for input on pipe for a controlled
                # shutdown
                os.read(read_fd, 1)
                return 0

            # sync startup and ensure child is running
            os.read(read_fd, 1)
            os.close(read_fd)
            os.kill(pid, 0)

            # notify child to exit
            os.write(write_fd, b'x')
            os.close(write_fd)

            (_, res) = os.waitpid(pid, 0)
            self.assertEqual(0, os.WEXITSTATUS(res))

        def test_kill_parent(self):
            """Test verifying child exit after killing the parent"""
            pread_fd, pwrite_fd = os.pipe()
            ppid = os.fork()
            if ppid == 0:
                # double fork to not kill the unittest
                read_fd, write_fd = os.pipe()
                pid = os.fork()
                if pid == 0:
                    self.assertTrue(set_kill_child_on_parent_exit())
                    os.write(write_fd, b'i')
                    os.close(write_fd)
                    time.sleep(3600)
                    sys.exit(0)

                # sync startup, set_kill_child_on_parent_exit must be
                # called before to get ProcessLookupError triggering
                os.read(read_fd, 1)

                # write grand-child pid, will be used to verify that
                # the grand-child indeed got killed.
                os.write(pwrite_fd, struct.pack('i', pid))
                os.close(pwrite_fd)
                time.sleep(3600)
                sys.exit(0)

            gc_pid_packed = os.read(pread_fd, len(struct.pack('i', 0)))
            (gc_pid, ) = struct.unpack('i', gc_pid_packed)
            os.close(pwrite_fd)
            os.close(pread_fd)

            # kill child process, the grand childprocess should go
            # down in the process
            os.kill(ppid, signal.SIGKILL)
            (_, res) = os.waitpid(ppid, 0)
            self.assertEqual(0, os.WEXITSTATUS(res))

            # wait for the os to clean up the process (using time, better way?)
            time.sleep(1)

            # send signal 0, should return error if the process is
            # missing.
            with self.assertRaises(ProcessLookupError):
                os.kill(gc_pid, 0)

    unittest.main()
