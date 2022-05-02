######################################################################
# Example showing a NSO version upgrade of the upper and one lower NSO
# instance for the developing-with-ncs/28-lsa-multi-version-deployment
# example.
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
	./multiver_nso_upgrade.sh -h
	./multiver_nso_upgrade.sh

start: check upgrade

stop: check
	if [ -d 28-lsa-multi-version-deployment/netsim ]; then \
		ncs-netsim --dir 28-lsa-multi-version-deployment/netsim stop; \
	fi
	-NCS_IPC_PORT=4569 ncs --stop
	-NCS_IPC_PORT=4570 ncs --stop
	-NCS_IPC_PORT=4571 ncs --stop
	@echo "##### All NSO and netsim instances stopped"

clean:
	rm -rf 28-lsa-multi-version-deployment
	rm -rf packages
