######################################################################
# Example showing a NSO version upgrade of the upper NSO instance for
# the developing-with-ncs/22-lsa-single-version-deployment example,
# aka 22-layered-service-architecture pre NSO 5.4.
#
# (C) 2021 Tail-f Systems
#
# See the README file for more information
######################################################################
RED=\033[0;31m
NC=\033[0;m # No Color

all: stop clean

check:
	@if [ -z ${NCS_DIR} ]; then \
	  printf "${RED}Where is NSO installed? Source ncsrc or set NCS_DIR to"; \
		printf " point it out!\n${NC}"; \
		exit 1; \
	fi

upgrade:
	./upper_nso_upgrade.sh -h
	./upper_nso_upgrade.sh

start: check upgrade

stop: check
	if [ -d 22-layered-service-architecture/netsim ]; then \
		ncs-netsim --dir 22-layered-service-architecture/netsim stop; \
	elif [ -d 22-lsa-single-version-deployment/netsim ]; then \
		ncs-netsim --dir 22-lsa-single-version-deployment/netsim stop; \
	fi
	-NCS_IPC_PORT=4569 ncs --stop
	-NCS_IPC_PORT=4570 ncs --stop
	-NCS_IPC_PORT=4571 ncs --stop
	@echo "##### All NSO and netsim instances stopped"

clean:
	rm -rf 22-layered-service-architecture 22-lsa-single-version-deployment
	rm -rf packages
