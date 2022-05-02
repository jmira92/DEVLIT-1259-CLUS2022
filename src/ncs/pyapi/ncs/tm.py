"""Internally used classes and functions."""

import functools

TM = '_ncs'


def ncs_only(fn):
    """Decorator for NCS specific functions."""
    @functools.wraps(fn)
    def wrapper(*args, **kwargs):
        return fn(*args, **kwargs)
    return wrapper
