include ../../../../support/include.mk

CONFDC = $(NCS_DIR)/bin/confdc
TD = $(NCS_DIR)/etc/ncs/snmp
EXTRA_LINK_FLAGS := --yangpath $(NCS_DIR)/src/ncs/snmp/yang --yangpath $(NCS_DIR)/src/ncs/yang

ifeq ($(BUILDNCS),true)
EXP=--export snmp
else
EXP=
endif

$(TD)/%.fxs: %.yang
	$(CONFDC) --fail-on-warnings \
                  `ls $*-ann.yang  > /dev/null 2>&1 && echo "-a $*-ann.yang"`\
                   $(EXTRA_LINK_FLAGS) $(EXP) -c -o $@  $<


%.hrl: $(TD)/%.fxs
	$(CONFDC) --emit-hrl $@ $<

%.yang:	%.mib
	$(CONFDC) --mib2yang $< -o $@

%.bin:	$(TD)/%.fxs
	$(CONFDC) -c $*.mib $(TD)/$*.fxs  \
        -o $@ \
	-f $(NCS_DIR)/etc/ncs/ \
	-f $(NCS_DIR)/etc/ncs/snmp && cp $@ $(TD)/$@

TAILF-ALARM-TC-MIB.bin: TAILF-TOP-MIB.bin
TAILF-ALARM-MIB.bin: TAILF-TOP-MIB.bin TAILF-ALARM-TC-MIB.bin

SNMP-MIBS = \
	TAILF-TOP-MIB.mib \
	TAILF-ALARM-TC-MIB.mib \
	IANA-ITU-ALARM-TC-MIB.mib  \
	ITU-ALARM-TC-MIB.mib  \
	TAILF-ALARM-MIB.mib

SNMP-YANG   := $(SNMP-MIBS:%.mib=%.yang)
SNMP-HRL    := $(SNMP-MIBS:%.mib=%.hrl)
SNMP-FXS    := $(SNMP-MIBS:%.mib=$(TD)/%.fxs)
SNMP-BIN    := $(SNMP-MIBS:%.mib=%.bin)
TD-SNMP-BIN := $(SNMP-MIBS:%.mib=$(TD)/%.bin)

all:	$(SNMP-YANG) $(SNMP-FXS) $(SNMP-BIN) $(SNMP-HRL)

clean:
	rm  -f $(SNMP-YANG) $(SNMP-FXS) $(SNMP-BIN) \
               $(SNMP-HRL) $(TD-SNMP-BIN)


