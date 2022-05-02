# Define common ConfD build tools and flags

CONFD_DIR = $(NCS_DIR)/netsim/confd

OSNAME		 := $(shell uname -s)
CONFD		 = $(CONFD_DIR)/bin/confd
CONFD_CLI	 = $(CONFD_DIR)/bin/confd_cli
CONFDC		 = $(CONFD_DIR)/bin/confdc
PYANG		 = $(CONFD_DIR)/bin/pyang
SMIDUMP		 = $(CONFD_DIR)/bin/smidump
INCLUDE		 = -I$(CONFD_DIR)/include
CONFD_LIB	?= $(CONFD_DIR)/lib/libconfd.a
CONFD_VSN        = $(shell cat $(CONFD_DIR)/src/confd/build/vsn.mk | \
                     grep CONFDVSN | sed 's/.*=//')
FXS_WERR        ?= --fail-on-warnings

LIBS		 = $(CONFD_LIB) -lpthread -lm
LIBDL		 = -ldl

CFLAGS		 = -Wall -g $(INCLUDE) -DCONFD_C_PRODUCT_CONFD
CDB_DIR		 = ./cdb

KILLALL		 = killall

ifeq ($(OSNAME),FreeBSD)
CFLAGS		+= -I/usr/local/include
LIBS		+= -L/usr/local/lib
LIBDL		=
endif
ifeq ($(OSNAME),Darwin)
CFLAGS		+= -I/opt/local/include
LIBS		+= -L/opt/local/lib
endif

ifeq ($(OSNAME),Darwin)
SHARED_FLAGS	= -dynamiclib
LD_ENV		= DYLD_LIBRARY_PATH
else
SHARED_FLAGS	= -shared
LD_ENV		= LD_LIBRARY_PATH
endif


# Define default ConfD build rules

%.h: %.fxs
	$(CONFDC) --emit-h $*.h $*.fxs

%.fxs: %.yang
	$(CONFDC) $(FXS_WERR) $(EXTRA_LINK_FLAGS) -c -o $@  $<

%.ccl: %.cli
	$(CONFDC) -c $<

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

%.bin:	%.fxs
	$(CONFDC) -c $*.mib $*.fxs -f $(CONFD_DIR)/etc/confd/snmp

