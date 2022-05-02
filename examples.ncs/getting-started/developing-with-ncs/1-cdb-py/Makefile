all:
	$(MAKE) -C packages/router/src all
	$(MAKE) -C packages/cdb/src all
	$(MAKE) netsim
	ncs-setup --netsim-dir ./netsim --dest .
	cp logging.xml ncs-cdb

netsim:
	ncs-netsim create-network ./packages/router 3 ex --dir ./netsim


clean:
	for f in packages/*/src; do \
		$(MAKE) -C $$f clean || exit 1; \
	done
	rm -rf ./netsim running.DB logs state/* ncs-cdb/*.cdb ncs-cdb/logging.xml logs ncs.conf
	rm -rf ./README.n*  ncs-cdb/netsim_devices_init.xml ./netsim packages/cdb/load-dir/test.fxs packages/cdb/python/_namespaces/test_ns.py ./scripts


stop:
	-ncs --stop
	ncs-netsim stop


start:
	mkdir -p logs
	ncs-netsim start
	ncs


update_py:
	cp packages/cdb/python/plaincdbsub/plaincdbsub.py state/packages-in-use/1/cdb/python/plaincdbsub/
	make -C state/packages-in-use/1/cdb/python

