# The order of packages is significant as there are dependencies between
# the packages. Typically generated namespaces are used by other packages.
PACKAGES = lb webserver webserver2 dns-service ntp-service \
	website-service nc-notif-receiver

# The create-network argument to ncs-netsim
NETWORK = create-network packages/lb 1 lb \
	create-network packages/webserver 2 www \
	create-network packages/webserver2 1 www

.PHONY: all
all:
	for i in $(PACKAGES); do \
		$(MAKE) -C packages/$${i}/src all || exit 1; \
	done
	$(MAKE) netsim

netsim:
	ncs-netsim --dir netsim $(NETWORK)
	ncs-netsim --dir netsim ncs-xml-init > ncs-cdb/netsim_devices_init.xml

.PHONY: clean
clean:
	for i in $(PACKAGES); do \
		$(MAKE) -C packages/$${i}/src clean || exit 1; \
	done
	rm -rf netsim running.DB state/*
	rm -f logs/* ncs-cdb/*.cdb ncs-cdb/*.db *.trace traplog.txt
	rm -f ncs-cdb/netsim_devices_init.xml

.PHONY: start
start:
	ncs-netsim start
	ncs -c ncs.conf

.PHONY: starti
starti:
	ncs-netsim start
	ncs -i -c ncs.conf

.PHONY: wait-until-started
wait-until-started:
	ncs --wait-started

.PHONY: stop
stop:
	ncs-netsim stop
	ncs --stop

.PHONY: cli
cli:
	ncs_cli -u admin
