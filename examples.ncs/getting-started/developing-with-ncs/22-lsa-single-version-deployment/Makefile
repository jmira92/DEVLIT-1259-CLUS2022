NODES =  upper-nso lower-nso-1 lower-nso-2

all:    upper-nso-all    lower-nso-1 lower-nso-2 netsim

manual: upper-nso-manual lower-nso-1 lower-nso-2 netsim

upper-nso-all: upper-nso
.PHONY: upper-nso-all

upper-nso-manual: package-store/cfs-vlan
	rm -rf upper-nso
	ncs-setup --no-netsim --dest upper-nso
	cp nso-etc/upper-nso/ncs.conf upper-nso
.PHONY: upper-nso-manual

upper-nso: package-store/cfs-vlan
	ncs-setup --no-netsim --dest $@
	cp nso-etc/$@/ncs.conf $@
	ln -sf ../../package-store/cfs-vlan     $@/packages
	$(MAKE) $@/packages/rfs-vlan-ned

package-store/cfs-vlan:
	$(MAKE) -C $@/src
.PHONY: package-store/cfs-vlan

upper-nso/packages/rfs-vlan-ned:
	ncs-make-package --no-netsim --no-java --no-python           \
	    --lsa-netconf-ned package-store/rfs-vlan/src/yang        \
	    --dest $@ --build $(@F)

lower-nso-%: package-store/rfs-vlan ../packages/router
	ncs-setup --no-netsim --dest $@
	cp nso-etc/$@/ncs-cdb/devs.xml      $@/ncs-cdb
	cp nso-etc/$@/ncs.conf              $@
	ln -sf ../../package-store/rfs-vlan $@/packages
	ln -sf ../../../packages/router        $@/packages

package-store/rfs-vlan:
	$(MAKE) -C $@/src
.PHONY: package-store/rfs-vlan

../packages/router:
	$(MAKE) -C $@/src
.PHONY: ../packages/router

netsim:
	ncs-netsim create-network ../packages/router 6 ex --dir ./netsim

start:
	$(MAKE) start-all

start-%:
	ncs-netsim start
	cd upper-nso;   NCS_IPC_PORT=4569 sname=upper-nso   ncs -c ncs.conf
	cd lower-nso-1; NCS_IPC_PORT=4570 sname=lower-nso-1 ncs -c ncs.conf
	cd lower-nso-2; NCS_IPC_PORT=4571 sname=lower-nso-2 ncs -c ncs.conf
	./init-$*.sh

stop:
	ncs-netsim stop
	NCS_IPC_PORT=4569 ncs --stop
	NCS_IPC_PORT=4570 ncs --stop
	NCS_IPC_PORT=4571 ncs --stop

start-upper-nso-i:
	cd upper-nso;   NCS_IPC_PORT=4569 sname=upper-nso ncs -i -c ncs.conf

stop-upper-nso:
	NCS_IPC_PORT=4569 ncs --stop

status:
	@ncs-netsim is-alive
	@NCS_IPC_PORT=4569 ncs --status > /dev/null 2>&1; \
	if [ $$? = 0 ]; then                              \
	    echo "upper-nso: UP";                         \
	    else echo "upper-nso: ERR";                   \
	fi
	@NCS_IPC_PORT=4570 ncs --status > /dev/null 2>&1; \
	if [ $$? = 0 ]; then                              \
	    echo "lower-nso-1: UP";                       \
	else                                              \
	    echo "lower-nso-1: ERR";                      \
	fi
	@NCS_IPC_PORT=4571 ncs --status > /dev/null 2>&1; \
	if [ $$? = 0 ]; then                              \
	    echo "lower-nso-2: UP";                       \
	else                                              \
	    echo "lower-nso-2: ERR";                      \
	fi

cli-upper-nso:
	NCS_IPC_PORT=4569 ncs_cli -u admin
cli-lower-nso-1:
	NCS_IPC_PORT=4570 ncs_cli -u admin
cli-lower-nso-2:
	NCS_IPC_PORT=4571 ncs_cli -u admin

clean:
	make -C package-store/cfs-vlan/src clean
	make -C package-store/rfs-vlan/src clean
	rm -rf upper-nso lower-nso-1 lower-nso-2
	rm -rf netsim
