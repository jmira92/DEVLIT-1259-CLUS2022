# -*- mode: python; python-indent: 4 -*-
import ncs
#BEGIN-SERVICE-IMPORT
from ncs.application import Service
#END-SERVICE-IMPORT
#BEGIN-ACTION-IMPORT
from ncs.dp import Action
#END-ACTION-IMPORT
#BEGIN-ACTION-EXAMPLE


# ---------------
# ACTIONS EXAMPLE
# ---------------
class DoubleAction(Action):
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        self.log.info('action name: ', name)
        self.log.info('action input.number: ', input.number)

        # Updating the output data structure will result in a response
        # being returned to the caller.
        output.result = input.number * 2
#END-ACTION-EXAMPLE
#BEGIN-SERVICE-EXAMPLE


# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------
class ServiceCallbacks(Service):

    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @Service.create
    def cb_create(self, tctx, root, service, proplist):
        self.log.info('Service create(service=', service._path, ')')

#BEGIN-SERVICE-TEMPLATE
        vars = ncs.template.Variables()
        vars.add('DUMMY', '127.0.0.1')
        template = ncs.template.Template(service)
        template.apply('%NAME%-template', vars)
#END-SERVICE-TEMPLATE

    # The pre_modification() and post_modification() callbacks are optional,
    # and are invoked outside FASTMAP. pre_modification() is invoked before
    # create, update, or delete of the service, as indicated by the enum
    # ncs_service_operation op parameter. Conversely
    # post_modification() is invoked after create, update, or delete
    # of the service. These functions can be useful e.g. for
    # allocations that should be stored and existing also when the
    # service instance is removed.

    # @Service.pre_lock_create
    # def cb_pre_lock_create(self, tctx, root, service, proplist):
    #     self.log.info('Service plcreate(service=', service._path, ')')

    # @Service.pre_modification
    # def cb_pre_modification(self, tctx, op, kp, root, proplist):
    #     self.log.info('Service premod(service=', kp, ')')

    # @Service.post_modification
    # def cb_post_modification(self, tctx, op, kp, root, proplist):
    #     self.log.info('Service postmod(service=', kp, ')')
#END-SERVICE-EXAMPLE
#BEGIN-SUBSCRIBER-EXAMPLE


# ------------------------------------------------
# SUBSCRIBER ITERATOR OBJECT
# ------------------------------------------------
class DemoSubscriber(ncs.cdb.Subscriber):
    """This subscriber subscribes to changes in the..."""

    # custom initializer which gets called from the
    # constructor (__int__)
    def init(self):
        self.register('/blaha:something/somethingelse', priority=100)

    # Initate your local state
    def pre_iterate(self):
        self.log.info('DemoSubscriber: pre_iterate')
        return []

    # Iterate over the change set
    def iterate(self, keypath, op, oldval, newval, state):
        # Do stuff, e.g test for kind of operation...
        if op == ncs.MOP_VALUE_SET:
            pass
        # ...perhaps save stuff into your local state
        state.append(42)
        return ncs.ITER_RECURSE

    # This will run in a separate thread to avoid a transaction deadlock
    def post_iterate(self, state):
        self.log.info('DemoSubscriber: post_iterate, state=', state)

        # DO YOUR STUFF HERE...
        pass

    # determine if post_iterate() should run
    def should_post_iterate(self, state):
        return state != []
#END-SUBSCRIBER-EXAMPLE


# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------
class %CLASS%(ncs.application.Application):
    def setup(self):
        # The application class sets up logging for us. It is accessible
        # through 'self.log' and is a ncs.log.Log instance.
        self.log.info('%CLASS% RUNNING')
#BEGIN-SERVICE-REGISTER

        # Service callbacks require a registration for a 'service point',
        # as specified in the corresponding data model.
        #
        self.register_service('%NAME%-servicepoint', ServiceCallbacks)
#END-SERVICE-REGISTER
#BEGIN-ACTION-REGISTER

        # When using actions, this is how we register them:
        #
        self.register_action('%NAME%-action', DoubleAction)
#END-ACTION-REGISTER
#BEGIN-SUBSCRIBER-REGISTER

        # Create your subscriber
        self.sub = DemoSubscriber(app=self)
        self.sub.start()
#END-SUBSCRIBER-REGISTER

        # If we registered any callback(s) above, the Application class
        # took care of creating a daemon (related to the service/action point).

        # When this setup method is finished, all registrations are
        # considered done and the application is 'started'.

    def teardown(self):
        # When the application is finished (which would happen if NCS went
        # down, packages were reloaded or some error occurred) this teardown
        # method will be called.
#BEGIN-SUBSCRIBER-DEREGISTER

        self.sub.stop()
#END-SUBSCRIBER-DEREGISTER

        self.log.info('%CLASS% FINISHED')
