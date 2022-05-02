######################################################################
# This Makefile needs the UNIX environment variable CONFD_DIR
# set to where confd got installed. The easiest way is to source
# the confdrc script found in the toplevel dir of the confd installation
#
# (C) 2009 Tail-f Systems
######################################################################

CONFDC  = $(CONFD_DIR)/bin/confdc

######################################################################
# EXPORT = --export snmp
# EXPORT = --export none
EXPORT =
######################################################################

MIB	= SNMPv2-TC \
	SNMPv2-SMI \
	SNMPv2-MIB \
	SNMP-FRAMEWORK-MIB \
	SNMP-TARGET-MIB \
	SNMP-MPD-MIB \
	SNMP-NOTIFICATION-MIB \
	SNMP-USER-BASED-SM-MIB \
	SNMP-COMMUNITY-MIB \
	SNMP-VIEW-BASED-ACM-MIB \
        IPV6-TC \
        TRANSPORT-ADDRESS-MIB

FXS     = $(MIB:%=%.fxs)
H       = $(MIB:%=%.h)


all:	$(FXS) $(H)

%.fxs:	%.yang
	a=$*-ann.yang; \
	$(CONFDC) -c -o $@ `[ -e $$a ] && echo "-a $$a"` $(EXPORT) -f . \
		-f $(CONFD_DIR)/etc/confd -- $<

%.h:	%.fxs
	$(CONFDC) --emit-h $@ $<

######################################################################
clean:
	-rm -rf *.fxs *.h > /dev/null

######################################################################
