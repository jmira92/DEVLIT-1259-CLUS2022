#!/bin/bash
#
# Debian/Ubuntu Startup script for NCS
#
# config: %confdir%/ncs.conf
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

test -x $ncs || exit 1

setup_ncs_environment () {
        . ${ncsdir}/ncsrc
        NCS_CONFIG_DIR=${confdir}
        NCS_RUN_DIR=${rundir}
        NCS_LOG_DIR=${logdir}
        export NCS_CONFIG_DIR NCS_RUN_DIR NCS_LOG_DIR
}

case "$1" in
    start)
        echo -n "Starting $prog: "
        setup_ncs_environment
        %sustart%$ncs --cd ${rundir} ${heart} ${conf}%suend%
        ret=$?
        echo "."
        ;;
    stop)
        echo -n "Stopping $prog: "
        $ncs   --stop
        echo "."
        ;;
    restart)
        echo -n "Stopping $prog: "
        setup_ncs_environment
        $ncs   --stop
        echo -n "Starting $prog: "
        %sustart%$ncs --cd ${rundir}  ${heart} ${conf}%suend%
        ret=$?
        echo "."
        ;;
    reload)
        echo -n "Reloading $prog: "
        $ncs   --reload
        echo "."
        ;;
    status)
        $ncs  --status
        ret=$?
        echo "."
        ;;
    start-with-package-reload)
        ## Start NCS and attempt an NCS package code reload
        echo -n "Starting $prog: "
        setup_ncs_environment
        NCS_RELOAD_PACKAGES=true \
            %sustart%$ncs --cd ${rundir}  ${heart} ${conf}%suend%
        ret=$?
        echo "."
        ;;
    restart-with-package-reload)
        ## Restart NCS and attempt an NCS package code reload
        echo -n "Stopping $prog: "
        setup_ncs_environment
        $ncs   --stop
        echo -n "Starting $prog: "
        NCS_RELOAD_PACKAGES=true \
            %sustart%$ncs --cd ${rundir}  ${heart} ${conf}%suend%
        ret=$?
        echo "."
        ;;
    *)
        echo $"Usage: $prog {start|stop|restart|reload|status}"
        exit 1
esac

exit ${ret}

