
start:
	NCS_IPC_PORT=5758 sname=n2 NCS_HA_NODE=n2 ncs -c ncs.conf

start-i:
	NCS_IPC_PORT=5758 sname=n2 NCS_HA_NODE=n2 ncs -c ncs.conf  -i \
	--with-package-reload
stop:
	NCS_IPC_PORT=5758 sname=n2 NCS_HA_NODE=n2 ncs --stop
cli:
	NCS_IPC_PORT=5758 ncs_cli -u admin
