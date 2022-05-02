#!/bin/bash
#
# Redhat Startup script for NCS
#
# chkconfig: 2345 95 05
# description: NCS - Network Control System
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

# When systemd is used, the sourcing of /etc/rc.d/init.d/functions
# below will actually use systemctl to tell systemd to do the
# start/stop/etc when the script is invoked by a user, and only
# when systemd in turn invokes the script will it actually do the
# requested action. Thus we must pass info about package reload
# from the first to the second invocation, via a "flag file".

package_reload_file="$rundir/state/do_package_reload"
package_reload_file_force="$rundir/state/do_package_reload_force"

case "$1" in
    *start*)
        if [ "x$NCS_RELOAD_PACKAGES" = "xtrue" ]; then
            touch $package_reload_file
        elif [ "x$NCS_RELOAD_PACKAGES" = "xforce" ]; then
            touch $package_reload_file_force
        fi
        ;;
esac

. /etc/rc.d/init.d/functions

case "$1" in
    *start*)
        if [ -f $package_reload_file ]; then
            rm -f $package_reload_file
            export NCS_RELOAD_PACKAGES=true
        elif [ -f $package_reload_file_force ]; then
            rm -f $package_reload_file_force
            export NCS_RELOAD_PACKAGES=force
        fi
        ;;
esac

start() {
        echo -n $"Starting $prog: "
        . $ncsdir/ncsrc
        NCS_CONFIG_DIR=${confdir}
        NCS_RUN_DIR=${rundir}
        NCS_LOG_DIR=${logdir}
        export NCS_CONFIG_DIR NCS_RUN_DIR NCS_LOG_DIR
        %sustart%$ncs --cd ${rundir}  ${heart} ${conf}%suend%
        RETVAL=$?
        echo
        # [ $RETVAL = 0 ] && touch /var/lock/subsys/ncs
        return $RETVAL
}

stop() {
        echo -n $"Stopping $prog: "
        $ncs --stop
        if [ "$?" = 0 ]; then
            echo_success
            RETVAL=0
        else
            echo_failure
            RETVAL=1
        fi
        echo
        rm -f /var/lock/subsys/ncs
        return $RETVAL
}


reload() {
        echo -n $"Reloading $prog: "
        r=`$ncs --reload`
        RETVAL=$?
        echo $r
}

usage() {
    cat <<EOF
"Usage: $prog {start | stop | restart | reload | status |
               start-with-package-reload |
               restart-with-package-reload |
               start-with-package-reload-force |
               restart-with-package-reload-force |
               help}"

 start
     Start the NCS daemon via systemctl.

 stop
     Stop the NCS daemon via systemctl.

 restart
     Restart the running NCS service via systemctl.

 reload
     Reload the NCS configuration via systemctl.

 status
     Print status about the NCS daemon.

 start-with-package-reload
     Start the NCS daemon via systemctl and reload all packages.

 restart-with-package-reload
     Restart the running NCS daemon via systemctl and reload all packages.

 start-with-package-reload-force
     Start the NCS daemon via systemctl and reload all packages
     in an unsage manner and override warnings if any.

 restart-with-package-reload-force
     Restart the running NCS daemon via systemctl and reload all packages
     in an unsage manner and override warnings if any.

EOF
}

# See how we were called.
case "$1" in
  start)
        start
        ;;
  stop)
        stop
        ;;
  restart)
        stop
        start
        ;;
  reload)
        reload
        ;;
  status)
        $ncs --status
        RETVAL=$?
        ;;
  start-with-package-reload)
        # Start NCS and attempt an NCS package code reload
        # This will not be redirected to systemd by default, need
        # to make it happen or systemd will be confused
        NCS_RELOAD_PACKAGES=true exec $0 start
        ;;
  restart-with-package-reload)
        # Restart NCS and attempt an NCS package code reload
        # This will not be redirected to systemd by default, need
        # to make it happen or systemd will be confused
        NCS_RELOAD_PACKAGES=true exec $0 restart
        ;;
  start-with-package-reload-force)
        # Start NCS and reload all packages in an unsafe manner
        # and override warnings if any.
        # This will not be redirected to systemd by default, need
        # to make it happen or systemd will be confused
        NCS_RELOAD_PACKAGES=force exec $0 start
        ;;
  restart-with-package-reload-force)
        # Start NCS and reload all packages in an unsafe manner
        # and override warnings if any.
        # This will not be redirected to systemd by default, need
        # to make it happen or systemd will be confused
        NCS_RELOAD_PACKAGES=force exec $0 restart
        ;;

  help)
        $ncs --help
        ;;
  *)
        usage
        exit 1
esac

exit $RETVAL
