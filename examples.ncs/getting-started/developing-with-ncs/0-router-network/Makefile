all:
	$(MAKE) -C packages/router/src all
	$(MAKE) netsim

netsim:
	ncs-netsim create-network ./packages/router 3 ex --dir ./netsim

showcase-clean-start:
	rm -rf ./netsim
	$(MAKE) all
	ncs-netsim stop >/dev/null
	ncs-netsim start
	@if [ -n '$(NSO_RUNDIR)' ] ; then \
		cp ncs-cdb/ncs_init.xml '$(NSO_RUNDIR)/ncs-cdb/'; \
		cp -a ./packages/router '$(NSO_RUNDIR)/packages/' || true; \
	else \
		echo "NSO_RUNDIR not set, skipping onboarding..."; \
	fi

clean:
	for f in packages/*/src; do \
		$(MAKE) -C $$f clean || exit 1; \
	done
	rm -rf ./netsim  logs/* state/* ncs-cdb/*.cdb

