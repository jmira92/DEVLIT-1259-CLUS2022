#
# Copyright 2009 Tail-f Systems AB
#

LIBCONFD  = $(NCS_DIR)/lib/libconfd.a
CFLAGS   += -Wall -g -DNCS -I$(NCS_DIR)/include

LDFLAGS    = -lpthread

# ASLR - Address Space Layout Randomization
CFLAGS   += -fpie
LDFLAGS  += -pie

all: ncs_cmd ncs_load

ncs_cmd: ncs_cmd.c
	$(CC) -o $@ $< $(CFLAGS) $(LIBCONFD) $(LDFLAGS) -lm

ncs_load: ncs_load.c
	$(CC) -o $@ $< $(CFLAGS) $(LIBCONFD) $(LDFLAGS) -lm

clean:
	rm -f *.o a.out core ncs_cmd ncs_load
	rm -rf *.dSYM

