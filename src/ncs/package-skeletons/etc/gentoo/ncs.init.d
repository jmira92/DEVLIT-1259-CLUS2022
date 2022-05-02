#!/sbin/runscript
#
# Startup script for NCS (for Gentoo Linux)
#
# config: %confdir%/ncs.conf
#
# description: ncs - Network Control System
#
# use: rc-update add ncs default
#

ncsdir=%ncsdir%
confdir=%confdir%
rundir=%rundir%
logdir=%logdir%

ncs=${ncsdir}/bin/ncs
prog=ncs
conf="--conf ${confdir}/ncs.conf"
heart="--heart"

opts="start stop reload query"

depend() {
       need net
}


start() {
       ebegin "Starting ${prog} "
       . $ncsdir/ncsrc
       NCS_CONFIG_DIR=${confdir}
       NCS_RUN_DIR=${rundir}
       NCS_LOG_DIR=${logdir}
       export NCS_CONFIG_DIR NCS_RUN_DIR NCS_LOG_DIR
       %sustart%${ncs} --cd ${rundir} ${heart}  ${conf}%suend%
       eend $?
}


stop() {
       ebegin "Stopping ${prog} "
       ${ncs} --stop
       eend $?
}


reload() {
       ebegin "Reloading ${prog} "
       ${ncs}  --reload
       eend $?
}


query() {
       ebegin "Querying ${prog} "
       ${ncs} --status
       eend $?
}

