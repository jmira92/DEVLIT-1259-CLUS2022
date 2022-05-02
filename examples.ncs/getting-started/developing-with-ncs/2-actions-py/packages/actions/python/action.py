"""NCS Action Package example.

Implements an package with actions
(C) 2015 Tail-f Systems
Permission to use this code as a starting point hereby granted

See the README file for more information
"""
from __future__ import print_function
import time

import _ncs
import _ncs.dp
from ncs.dp import Action
from ncs.application import Application
from _namespaces.action_test_ns import ns


class ActionHandler(Action):
    """This class implements the dp.Action class."""

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        """Called when the actionpoint is invoked.

        The function is called with:
            uinfo -- a UserInfo object
            name -- the tailf:action name (string)
            kp -- the keypath of the action (HKeypathRef)
            input -- input node (maagic.Node)
            output -- output node (maagic.Node)
        """
        self.log.debug("action(uinfo={0}, name={1})".format(uinfo.usid, name))
        if name == ns.a_reboot_:
            self.log.debug("action: reboot")
            return _ncs.CONFD_OK
        elif name == ns.a_restart_:
            mode = input.mode
            self.log.debug("action: restart mode={0}".format(mode))
            output.time = time.strftime("%H:%M:%S")
        else:
            self.log.debug("got bad operation: {0}".format(name))
            return _ncs.CONFD_ERR


# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Action(Application):
    """This class is referred to from the package-meta-data.xml."""

    def setup(self):
        """Setting up the action callback."""
        self.log.debug('action app start')
        self.register_action('reboot-point', ActionHandler, [])
