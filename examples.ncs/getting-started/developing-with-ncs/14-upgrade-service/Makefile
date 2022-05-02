all:
	$(MAKE) -C packages/router/src all
	$(MAKE) -C package-store/vlan/src all
	$(MAKE) -C package-store/vlan_v2/src all
	$(MAKE) -C package-store/tunnel/src all
	$(MAKE) netsim
netsim:
	ncs-netsim create-network ./packages/router 3 ex --dir ./netsim
vlan:
	$(MAKE) -C packages/vlan/src all
tunnel:
	$(MAKE) -C packages/tunnel/src all
clean:
	for f in packages/router/src; do \
		$(MAKE) -C $$f clean || exit 1; \
	done
	for f in package-store/*/src; do \
		$(MAKE) -C $$f clean || exit 1; \
	done
	cd packages; rm -rf tunnel; rm -rf vlan;
	rm -rf ./netsim logs/* state/* ncs-cdb/*.cdb

