#! /bin/sh
#
# SuSE Startup script for NCS
#
### BEGIN INIT INFO
# Provides:          ncs
# Required-Start:    $local_fs $network $syslog
# Required-Stop:
# X-Start-Before:
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: NCS
# Description: NCS - Network Control System
### END INIT INFO

ncsdir=%ncsdir%
confdir=%confdir%
rundir=%rundir%
logdir=%logdir%

ncs=${ncsdir}/bin/ncs
prog=ncs
conf="-c ${confdir}/ncs.conf"
heart="--heart"


test -x $ncs || exit 5


# Shell functions sourced from /etc/rc.status:
#      rc_check         check and set local and overall rc status
#      rc_status        check and set local and overall rc status
#      rc_status -v     ditto but be verbose in local rc status
#      rc_status -v -r  ditto and clear the local rc status
#      rc_failed        set local and overall rc status to failed
#      rc_failed <num>  set local and overall rc status to <num><num>
#      rc_reset         clear local rc status (overall remains)
#      rc_exit          exit appropriate to overall rc status
#      rc_active        checks whether a service is activated by symlinks
. /etc/rc.status

# First reset status of this service
rc_reset

# Return values acc. to LSB for all commands but status:
# 0 - success
# 1 - generic or unspecified error
# 2 - invalid or excess argument(s)
# 3 - unimplemented feature (e.g. "reload")
# 4 - insufficient privilege
# 5 - program is not installed
# 6 - program is not configured
# 7 - program is not running
#
# Note that starting an already running service, stopping
# or restarting a not-running service as well as the restart
# with force-reload (in case signalling is not supported) are
# considered a success.

case "$1" in
    start)
        echo -n "Starting ${prog}"
        . $ncsdir/ncsrc
        NCS_CONFIG_DIR=${confdir}
        NCS_RUN_DIR=${rundir}
        NCS_LOG_DIR=${logdir}
        export NCS_CONFIG_DIR NCS_RUN_DIR NCS_LOG_DIR
        %sustart%${ncs} --cd ${rundir} ${heart} ${conf}%suend%

        # Remember status and be verbose
        rc_status -v
        ;;
    stop)
        echo -n "Shutting down ${prog}"
        ${ncs} --stop

        # Remember status and be verbose
        rc_status -v
        ;;
    try-restart)
        ## Stop the service and if this succeeds (i.e. the
        ## service was running before), start it again.
        ## Note: try-restart is not (yet) part of LSB (as of 0.7.5)
        $0 status >/dev/null &&  $0 restart

        # Remember status and be quiet
        rc_status
        ;;
    restart)
        ## Stop the service and regardless of whether it was
        ## running or not, start it again.
        $0 stop
        $0 start

        # Remember status and be quiet
        rc_status
        ;;
    force-reload)
        echo -n "Force Reload service ${prog}"
        ## if it supports it:
        $ncs --reload
        rc_status -v
        ;;
    reload)
        ## Like force-reload, but if daemon does not support
        ## signalling, do nothing (!)
        # If it supports signalling:
        $ncs --reload
        rc_status -v
        ;;
    status)
        echo -n "Checking for service ${prog}: "
        $ncs --status
        rc_status -v
        ;;
    probe)
        ## Optional: Probe for the necessity of a reload,
        ## print out the argument which is required for a reload.
        test /etc/FOO/FOO.conf -nt /var/run/FOO.pid && echo reload
        ;;
    start-with-package-reload)
        ## Start NCS and attempt an NCS package code reload
        NCS_RELOAD_PACKAGES=true $0 start
        rc_status
        ;;
    restart-with-package-reload)
        ## Restart NCS and attempt an NCS package code reload
        NCS_RELOAD_PACKAGES=true $0 restart
        rc_status
        ;;
    *)
        echo "Usage: $0 {start|stop|status|try-restart|restart|force-reload|reload|probe}"
        exit 1
        ;;
esac
rc_exit
