

include ../../../support/include.mk
NCSC = $(NATIVE_SVNTOP_DIR)/bin/ncsc
CONFDC = $(NATIVE_SVNTOP_DIR)/bin/confdc
YANGER = $(NATIVE_SVNTOP_DIR)/bin/yanger

EXTRA_LINK_FLAGS := --yangpath $(NCS_DIR)/src/ncs/snmp/yang

YANG_MODULES = tailf-ncs tailf-ncs-alarms tailf-ncs-ned \
	       tailf-ncs-snmp tailf-ncs-devices-yang-library \
	       tailf-ncs-event-forwarding tailf-ncs-scheduler \
	       tailf-ncs-progress tailf-netconf-ncs \
	       tailf-ncs-kicker-extension tailf-ncs-netconf-ned-builder \
	       tailf-ncs-acm tailf-restconf-ncs

all: $(YANG_MODULES:%=%.fxs) $(YANG_MODULES:%=%.hrl) \
     ../priv/tailf-ncs-packages.fxs tailf-ncs-packages.hrl \
	tailf-ncs-rfc5277-device.fxs tailf-ncs-rfc5277-device.hrl \
	tailf-ncs-generic-ned-notification.fxs tailf-ncs-generic-ned-notification.hrl \
	tailf-ncs-cli-ned-notification.fxs tailf-ncs-cli-ned-notification.hrl
	cd alarm-mib && $(MAKE) all

tailf-netconf-ncs.fxs: tailf-netconf-ncs.yang
	$(YANGER) -f fxs \
	-p ../../confd/src/ --fxs-export netconf \
	--fxs-internal-exs -o $@ $<

tailf-restconf-ncs.fxs: tailf-restconf-ncs.yang
	$(YANGER) -f fxs \
	-p ../../rest/src/ \
	--fxs-export rest -o $@ $<

#
# Generate a temporary yang file for the packages; used in order to have
# a fxs file in runtime for exs_validator when parsing package files
#
../priv/tailf-ncs-packages.fxs: .tailf-ncs-packages.yang
	$(YANGER) -f fxs --fxs-export none --fxs-internal-exs \
	--fxs-internal-ordered-false -o $@ $<;

tailf-ncs-packages.hrl: ../priv/tailf-ncs-packages.fxs
	$(CONFDC) --fail-on-warnings --emit-hrl $@ $<

.INTERMEDIATE: .tailf-ncs-packages.yang

.tailf-ncs-packages.yang: tailf-ncs-packages.yang
	awk -f patch-packages.awk $< > $@

%.fxs: %.yang
	$(CONFDC) --fail-on-warnings `ls $*-ann.yang  \
       > /dev/null 2>&1 && echo "-a $*-ann.yang"` $(EXTRA_LINK_FLAGS) \
       --use-description -c -o $@  $<

%.hrl: %.fxs
	$(CONFDC) --fail-on-warnings --emit-hrl $@ $<


../priv/%.fxs: %.fxs
	rm -f $@ ; cp $< $@

# Special "NED" for RFC 5277 (NETCONF notifications). This RFC defines
# a pre-YANG data model, which means that a device will not advertise this
# as a YANG module. netconf_netmod.yang is our internal YANG version of this
# data model, and we compile it as a special NED that all NETCONF devices
# automatically handle.
# We give it this name b/c calling it netconf_netmod.fxs is really confusing,
# since we have a netconf_netmod.fxs in netconf's priv dir (for northbound
# notifications).
tailf-ncs-rfc5277-device.yang: ../../netconf/src/netconf_netmod.yang
	cat $< | \
	sed 's/module netconf_netmod/module tailf-ncs-rfc5277-device/' > \
	$@
tailf-ncs-rfc5277-device.fxs: tailf-ncs-rfc5277-device.yang
	mkdir -p device_dir && \
	$(NCSC) --ncs-compile-module $<  --ncs-device-type netconf \
	--ncs-skip-config --ncs-skip-template \
	--ncs-ned-id tailf-ncs-ned:rfc5277-id --ncs-device-dir device_dir && \
	cp device_dir/modules/fxs/$@ $@

tailf-ncs-generic-ned-notification.yang: ../../netconf/src/netconf_netmod.yang
	cat $< | \
	sed -e 's/module netconf_netmod/module tailf-ncs-generic-ned-notification/' \
            -e 's/urn:ietf:params:xml:ns:netmod:notification/"http:\/\/tail-f.com\/ns\/ncs-ned\/notification\/1.0"/' \
            -e 's/prefix nm/prefix gnn/' \
            -e 's/netconf/internal/' > \
	$@
tailf-ncs-generic-ned-notification.fxs: tailf-ncs-generic-ned-notification.yang
	mkdir -p device_dir && \
	$(NCSC) --ncs-compile-module $< --ncs-device-type generic-ned \
	--ncs-skip-config --ncs-skip-template \
	--ncs-ned-id tailf-ncs-ned:generic-ned-notification-id --ncs-device-dir device_dir && \
	cp device_dir/modules/fxs/$@ $@

tailf-ncs-cli-ned-notification.yang: ../../netconf/src/netconf_netmod.yang
	cat $< | \
	sed -e 's/module netconf_netmod/module tailf-ncs-cli-ned-notification/' \
            -e 's/urn:ietf:params:xml:ns:netmod:notification/"http:\/\/tail-f.com\/ns\/ncs-ned\/notification\/1.0"/' \
            -e 's/prefix nm/prefix cnn/' \
            -e 's/netconf/internal/' > \
	$@
tailf-ncs-cli-ned-notification.fxs: tailf-ncs-cli-ned-notification.yang
	mkdir -p device_dir && \
	$(NCSC) --ncs-compile-module $< --ncs-device-type cli-ned \
	--ncs-skip-config --ncs-skip-template \
	--ncs-ned-id tailf-ncs-ned:cli-ned-notification-id --ncs-device-dir device_dir && \
	cp device_dir/modules/fxs/$@ $@

clean:
	-rm -f *.hrl *.fxs ../priv/*.fxs *.bin .tailf-ncs-packages.yang .*.yang.d
	-rm -f tailf-ncs-rfc5277-device.yang
	-rm -f tailf-ncs-generic-ned-notification.yang
	-rm -f tailf-ncs-cli-ned-notification.yang
	-rm -rf .*.ok device_dir
	cd alarm-mib && $(MAKE) clean || true


#
# Dependencies for YANG modules
#
YANG_DEPS = $(YANG_MODULES:%=.%.yang.d)

.tailf-restconf-ncs.yang.d: tailf-restconf-ncs.yang
	$(YANGER) -f depend --depend-include-path --depend-target=$*.fxs --depend-extension=.yang -p ../../rest/src/ $< > $@

.%.yang.d: %.yang
	$(YANGER) -f depend --depend-include-path --depend-target=$*.fxs --depend-extension=.yang $< > $@


ifneq ($(MAKECMDGOALS),clean)
-include $(YANG_DEPS)
endif
