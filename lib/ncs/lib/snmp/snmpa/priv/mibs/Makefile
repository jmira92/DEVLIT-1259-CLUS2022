######################################################################
# Makefile
# (C) 2006 Tail-f Systems
#
# 
######################################################################

include ../../../support/include.mk

MIBC      = $(NATIVE_SVNTOP_DIR)/otp/installed/bin/erlc
CONFDC    = $(NATIVE_SVNTOP_DIR)/bin/confdc

######################################################################

all:	SNMPv2-MIB.bin \
	SNMP-FRAMEWORK-MIB.bin \
	SNMP-TARGET-MIB.bin \
	SNMP-MPD-MIB.bin \
	SNMP-NOTIFICATION-MIB.bin \
	SNMP-USER-BASED-SM-MIB.bin \
	SNMP-COMMUNITY-MIB.bin \
	SNMP-VIEW-BASED-ACM-MIB.bin \
	TRANSPORT-ADDRESS-MIB.hrl \
	TRANSPORT-ADDRESS-MIB.bin \
	IPV6-TC.bin

SNMP-FRAMEWORK-MIB.bin SNMP-MPD-MIB.bin TRANSPORT-ADDRESS-MIB.bin IPV6-TC.bin: %.bin: %.mib
	$(MIBC) $<

SNMP-COMMUNITY-MIB.bin: SNMP-COMMUNITY-MIB.mib
	$(CONFDC) -c $< ../yang/SNMP-COMMUNITY-MIB.fxs \
	  ../yang/SNMP-TARGET-MIB.fxs --fail-on-warnings \
	  -f ../yang -f $(CONFD_DIR)/etc/confd/ietf-yang-types.fxs \
	  -f $(CONFD_DIR)/etc/confd/ietf-inet-types.fxs

SNMPv2-MIB.bin:	SNMPv2-MIB.mib ../yang/SNMPv2-MIB.fxs
	$(CONFDC) -c $^ --fail-on-warnings \
	  -f ../yang -f $(CONFD_DIR)/etc/confd/ietf-yang-types.fxs \
	  -f $(CONFD_DIR)/etc/confd/ietf-inet-types.fxs

%.bin:	%.mib
	$(CONFDC) -c $< ../yang/$*.fxs --fail-on-warnings \
	  -f ../yang -f $(CONFD_DIR)/etc/confd/ietf-yang-types.fxs \
	  -f $(CONFD_DIR)/etc/confd/ietf-inet-types.fxs

%.hrl: %.bin
	$(MIBC) $<

######################################################################
clean:
	-rm -rf *.bin *.hrl

######################################################################
