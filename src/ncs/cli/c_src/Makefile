######################################################################
# (C) 2006 Tail-f Systems
#
######################################################################

# Uncomment if your OS does not have getopt_long(3)
#NEED_GETOPT_LONG = yes

STRIP      = strip

SRC        = clistart.c confd_ipc_access.c
OBJS       = $(SRC:.c=.o)
MAAPI_SRC  = ncs-maapi.c
MAAPI_OBJS = $(MAAPI_SRC:.c=.o)

CFLAGS    += -Wall -DNCS -I$(NCS_DIR)/include
ifeq ($(NEED_GETOPT_LONG), yes)
SRC       += getopt_long.c
CFLAGS    += -I.
endif

MAAPI_LDFLAGS = -lpthread

all: ncs_cli ncs-maapi

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

ncs_cli: $(OBJS)
	$(CC) -o $@ $^
	$(STRIP) $@

ncs-maapi: $(MAAPI_OBJS)
	$(CC) -o $@ $^ $(NCS_DIR)/lib/libconfd.a $(MAAPI_LDFLAGS)
	$(STRIP) $@

clean:
	rm -f *.o ncs_cli ncs-maapi
