"""This module implements classes to simplify template processing."""
import ncs


class Template(object):
    """Class to simplify applying of templates in a NCS service callback."""

    def __init__(self, service, path=None):
        """Initialize a Template object.

        The 'service' argument is the 'service' variable received in
        decorated cb_create and cb_pre_lock_create methods in a service class.
        ('service' can in fact be any maagic.Node (except a Root node)
        instance with an underlying Transaction). It is also possible to
        provide a maapi.Transaction instance for the 'service' argument in
        which case 'path' must also be provided.
        """
        if isinstance(service, ncs.maagic.Node):
            if isinstance(service, ncs.maagic.Root):
                raise Exception('A maagic.Root object cannot be used as a'
                                ' context node in Template')
            else:
                self._trans = service._backend
                self._path = service._path
        elif isinstance(service, ncs.maapi.Transaction):
            if path is None:
                raise Exception('Missing argument "path" for Template')
            self._trans = service
            self._path = path
        else:
            raise Exception('Invalid service type {} - expected maagic.Node or'
                            ' maapi.Transaction'.format(type(service)))

    def apply(self, name, vars=None, flags=0):
        """Apply the template 'name'.

        The optional argument 'vars' may be provided in form of a
        Variables instance.
        """
        self._trans.apply_template(name, self._path, vars, flags)

    def _varibles(self, name):
        raise Exception('Not implemented yet!')


class Variables(list):
    """Class to simplify passing of variables when applying a template."""

    def __init__(self, init=None):
        """Initialize a Variables object.

        The optional argument 'init' can be any iterable yielding a
        2-tuple in the form (name, value).
        """
        super(Variables, self).__init__()
        if init:
            for name, value in init:
                self.add(name, value)

    def add(self, name, value):
        """Add a value for the variable 'name'.

        The value will be quoted before adding it to the internal list.

        Quoting works like this:
            If value contains ' all occurrences of " will be replaced by ' and
            the final value will be quoted with ". Otherwise, the final value
            will be quoted with '.
        """
        value = self._normalize_value(value)
        value = self._quote_value(value)
        self.add_plain(name, value)

    def add_plain(self, name, value):
        """Add a value for the variable 'name'.

        It's up to the caller to do proper quoting of value.
        """
        # convert to str _before_ looking for duplicates to ensure non
        # str keys match
        name = str(name)
        value = str(value)

        for n, v in self:
            if n == name:
                self.remove((n, v))
                break
        self.append((name, value))

    def _normalize_value(self, value):
        # Special handling of bool's
        if isinstance(value, bool):
            return str(value).lower()
        return str(value)

    def _quote_value(self, value):
        if "'" in value:
            return '"{}"'.format(value.replace('"', "'"))
        return "'{}'".format(value)


if __name__ == '__main__':
    import unittest

    class VariablesTest(unittest.TestCase):
        def test_add(self):
            v = Variables()
            v.add('key', 'value')
            v.add(42, 'fourty two')
            v.add(True, 'is true')
            self.assertEqual([('key', "'value'"),
                              ('42', "'fourty two'"),
                              ('True', "'is true'")], v)

        def test_add_duplicate_reorder(self):
            v = Variables()
            v.add('one', 1)
            v.add('two', 2)
            v.add('one', 3)
            self.assertEqual([('two', "'2'"),
                              ('one', "'3'")], v)

        def test_add_duplicate_non_str(self):
            v = Variables()
            v.add(['a', 'python', 'list'], 3)
            v.add(['a', 'python', 'list'], 4)
            self.assertEqual([("['a', 'python', 'list']", "'4'")], v)

        def test__normalize_value(self):
            v = Variables()
            self.assertEqual('true', v._normalize_value(True))
            self.assertEqual('false', v._normalize_value(False))
            self.assertEqual('string', v._normalize_value('string'))
            self.assertEqual('42', v._normalize_value(42))

        def test__quote_value(self):
            v = Variables()
            self.assertEqual(
                "'no quote'", v._quote_value('no quote'))
            self.assertEqual(
                "\"single ' quote\"", v._quote_value("single ' quote"))
            self.assertEqual(
                "'double \" quote'", v._quote_value('double " quote'))

    unittest.main()
