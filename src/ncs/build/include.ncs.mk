# This include makefile is used by all the NCS examples

# Define common NCS build tools and flags

OSNAME		 := $(shell uname -s)

CONFDC           = @echo "WARNING: CONFDC has been renamed to NCSC" ; false
NCS		 = $(NCS_DIR)/bin/ncs
NETSIM		 = $(NCS_DIR)/bin/ncs-netsim
NCSC		 = $(NCS_DIR)/bin/ncsc
NCS_MAKE_PACKAGE = $(NCS_DIR)/bin/ncs-make-package
PYANG		 = $(NCS_DIR)/bin/pyang
CDB_DIR		 = ./ncs-cdb
FXS_WERR         ?= --fail-on-warnings

KILLALL          = killall

TAR              = tar
ifeq ($(OSNAME),Darwin)
    TAR          = $(shell which gnutar || which tar)
endif
ifeq ($(OSNAME),FreeBSD)
    TAR          = gtar
endif

# export for shell script use
export MAKE

# Define default ncs build rules

%.fxs: %.yang
	$(NCSC) $(FXS_WERR) $(EXTRA_LINK_FLAGS) -c -o $@  $<

%.ccl: %.cli
	$(NCSC) -c $<

default:
	@echo "** Specify target, e.g 'make all'"

env-check:
	@if [ ! -f $(NCS_DIR)/src/ncs/build/include.ncs.mk ]; then \
		echo "Need proper NCS installation with NCS_DIR env set"; \
		exit 1;\
	fi

iclean:	env-check
	rm -rf *.log *.trace logs/* ncs-cdb/*.db ncs-cdb/*.cdb \
	state/*

### developer only targets

ifneq ($(shell which gnome-terminal 2>/dev/null),)
TERMINAL=gnome-terminal -e
else
TERMINAL=xterm -e
endif

ifeq ($(shell ls $(NCS_DIR)/.tailf_developer 2>/dev/null),)
X-DEBUG_FLAGS=--foreground --verbose
else
X-DEBUG_FLAGS=-i
endif

ncs-example-debug: stop clean all confd_start ncs_debug_start

# that we start, ncs, java and the cli in three different
# terminals

ncs_debug_start:
	$(TERMINAL) "$(NCS) -c ncs.conf $(X-DEBUG_FLAGS)" &
	$(NCS) --wait-started
	sleep 1
	$(TERMINAL)  "ncs_cli -u admin"  &
