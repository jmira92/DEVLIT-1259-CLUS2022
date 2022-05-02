# The order of packages is significant as there are dependencies between
# the packages. Typically generated namespaces are used by other packages.
PACKAGES = \
	resource-manager     \
	ipaddress-allocator  \
	id-allocator         \
	esc                  \
	weblog               \
	vm-manager           \
	vm-manager-esc       \
	l3vpn                \
	l3vpnui

# Directory of example packages
PACKAGE_STORE=$(NCS_DIR)/packages/neds

# The create-network argument to ncs-netsim
NETWORK = create-network $(PACKAGE_STORE)/cisco-ios-cli-3.8      9 ce   \
	  create-network $(PACKAGE_STORE)/cisco-iosxr-cli-3.5    2 pe   \
	  create-network $(PACKAGE_STORE)/juniper-junos-nc-3.0   1 pe   \
	  create-network $(PACKAGE_STORE)/cisco-iosxr-cli-3.5    1 pe   \
	  create-network $(PACKAGE_STORE)/cisco-iosxr-cli-3.5    4 p

all: packages netsim ncs
.PHONY: all

packages:
	for i in $(PACKAGES); do \
	    $(MAKE) -C packages/$${i}/src all || exit 1; \
	done
.PHONY: packages

netsim:
ifdef OPENSTACK
	ncs-netsim --dir netsim $(NETWORK)
else
	ncs-netsim --dir netsim $(NETWORK) create-network packages/esc 1 esc
endif
	cp initial_data/ios.xml netsim/ce/ce0/cdb
	cp initial_data/ios.xml netsim/ce/ce1/cdb
	cp initial_data/ios.xml netsim/ce/ce2/cdb
	cp initial_data/ios.xml netsim/ce/ce3/cdb
	cp initial_data/ios.xml netsim/ce/ce4/cdb
	cp initial_data/ios.xml netsim/ce/ce5/cdb
	cp initial_data/ios.xml netsim/ce/ce6/cdb
	cp initial_data/ios.xml netsim/ce/ce7/cdb
	cp initial_data/ios.xml netsim/ce/ce8/cdb
	cp initial_data/iosxr.xml netsim/pe/pe0/cdb
	cp initial_data/iosxr.xml netsim/pe/pe1/cdb
	cp initial_data/iosxr.xml netsim/pe/pe3/cdb
	cp initial_data/iosxr.xml netsim/p/p0/cdb
	cp initial_data/iosxr.xml netsim/p/p1/cdb
	cp initial_data/iosxr.xml netsim/p/p2/cdb
	cp initial_data/iosxr.xml netsim/p/p3/cdb
ifndef OPENSTACK
	cp initial_data/esc.xml netsim/esc/esc0/cdb
endif

ncs:
	ncs-setup --netsim-dir netsim --dest .
	cp ncs.conf.in ncs.conf
	cp initial_data/topology.xml ncs-cdb
	cp initial_data/compliance.xml ncs-cdb
	cp initial_data/static-map-pos.xml ncs-cdb
	cp initial_data/template.xml ncs-cdb
	cp initial_data/qos.xml ncs-cdb
	cp initial_data/device-groups.xml ncs-cdb
	cp initial_data/device-icons.xml ncs-cdb
	cp initial_data/virtual_pe.xml ncs-cdb
.PHONY: ncs

clean:
	for i in $(PACKAGES); do                           \
	    $(MAKE) -C packages/$${i}/src clean || exit 1; \
	done
	rm -rf netsim
	rm -rf logs ncs-cdb state
	rm -f README.ncs README.netsim ncs.conf
	rm -f packages/cisco-ios-cli-3.8 packages/cisco-iosxr-cli-3.5
	rm -f packages/juniper-junos-nc-3.0
stop:
	-ncs --stop
	-ncs-netsim stop

# We need ignore initial validation since we are loading ip address pool and
# id pool data at the beginning, and the java validators are not loaded at
# that time.
# This is no problem in this case since we know data in initial_data is good.
start:
	ncs-netsim start &
	ncs --with-package-reload --ignore-initial-validation


reset:
	ncs-setup --reset

cli:
	ncs_cli -u admin

kill_ncs:
	pkill -lf ncs.conf

sync-from:
	echo request devices device \* sync-from | ncs_cli -u admin

rebuild: kill_ncs stop clean all start sync-from cli

qstop:
	pkill -lf ncs.conf || true
	pkill -lf confd.conf || true

# This rule is to cleanup the vPE devices, no support in ncs-netsim to
# delete individual devices yet
del_netsim:
	for ns in `ncs-netsim list| cut -d' ' -f 1| cut -d= -f 2| grep _vpn_CSR0`; \
	do ncs-netsim stop $$ns || true; \
	done
	sed -e "/^## device .*_vpn_CSR0/,/^#####/d" netsim/.netsiminfo > tmp.txt
	mv tmp.txt netsim/.netsiminfo
	rm -rf netsim/*_vpn_CSR

clean_cdb:
	rm -f ncs-cdb/*.cdb

restart_test: del_netsim stop start
