# The order of packages is significant as there are dependencies bewteen
# the packafes. Typically generated namespaces are used by other packages.
PACKAGE_DIRS = router alarms

all:
	for i in $(PACKAGE_DIRS); do \
		$(MAKE) -C packages/$${i}/src all || exit 1; \
	done
	$(MAKE) netsim

netsim:
	ncs-netsim  --dir netsim \
		create-network packages/router 3 ex


clean:
	for i in $(PACKAGE_DIRS); do \
		$(MAKE) -C packages/$${i}/src clean || exit 1; \
	done
	rm -rf netsim running.DB logs/* state/* ncs-cdb/*.cdb







