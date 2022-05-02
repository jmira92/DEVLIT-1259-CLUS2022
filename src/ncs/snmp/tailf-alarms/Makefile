

NCSC = $(NCS_DIR)/bin/ncsc
EXTRA_LINK_FLAGS := --yangpath $(NCS_DIR)/src/ncs/snmp/yang \
                    --yangpath $(NCS_DIR)/src/ncs/yang

EXP=--export snmp


%.fxs: %.yang
	$(NCSC) --fail-on-warnings \
                   `ls $*-ann.yang  > /dev/null 2>&1 && echo "-a $*-ann.yang"`\
                    $(EXTRA_LINK_FLAGS) $(EXP) -c -o $@  $<

%.yang:	%.mib
	$(NCSC) --mib2yang $< -o $@

%.bin:	%.fxs
	$(NCSC) -c $*.mib $*.fxs  \
        -o $@ \
	-f $(NCS_DIR)/etc/ncs \
	-f $(NCS_DIR)/etc/ncs/snmp -f .

TAILF-ALARM-TC-MIB.bin: TAILF-TOP-MIB.bin
TAILF-ALARM-MIB.bin: TAILF-TOP-MIB.bin TAILF-ALARM-TC-MIB.bin

SNMP-MIBS = \
	TAILF-TOP-MIB.mib \
	TAILF-ALARM-TC-MIB.mib \
	IANA-ITU-ALARM-TC-MIB.mib  \
	ITU-ALARM-TC-MIB.mib  \
	TAILF-ALARM-MIB.mib

SNMP-YANG := $(SNMP-MIBS:%.mib=%.yang)
SNMP-FXS := $(SNMP-MIBS:%.mib=%.fxs)
SNMP-BIN := $(SNMP-MIBS:%.mib=%.bin)

all:	$(SNMP-YANG) $(SNMP-FXS) $(SNMP-BIN)


clean:
	rm  -f $(SNMP-YANG) $(SNMP-FXS) $(SNMP-BIN) 2>/dev/null || true


