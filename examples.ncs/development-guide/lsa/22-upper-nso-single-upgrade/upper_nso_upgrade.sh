#!/bin/bash
set -eu # Abort the script if a command returns with a non-zero exit code or if
        # a variable name is dereferenced when the variable hasn't been set

RED='\033[0;31m'
GREEN='\033[0;32m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

function usage()
{
   printf "${GREEN}Demo upgrading the"
   printf " 22-layered-service-architecture/lsa-single-version-deployment\n"
   printf "example upper layer NSO instance to a newer NSO version\n\n"
   printf "  -d  Path to old NSO local install. Default: NCS_DIR environment variable\n"
   printf "  -p  Path to new NSO local install. Default: NCS_DIR environment variable\n"
   printf "  -o  Old NSO version. 4.7 or newer Default: Output of the ncs --version command\n"
   printf "  -n  New NSO version. 5.4.1 or newer. Default: Output of the ncs --version command\n\n"
   printf "I.e. if only default values are used the upgrade will be to the ${RED}same NSO version${GREEN},\n"
   printf "still performing the same upgrade steps for demo purposes.\n"
   printf "\nTo, for example, upgrade the 4.7.10 22-layered-service-architecture example upper layer NSO to 5.7:\n\n"
   printf "  \$ ./upper_nso_upgrade.sh -o 4.7.10 -d /Users/tailf/nso-4.7.10 -n 5.7 -p /Users/tailf/nso-5.7\n\n${NC}"
}

# Retrieve the calling parameters.
while getopts "o:n:d:p:h" OPTION; do
    case "${OPTION}"
    in
        o)  OLD_VERSION="${OPTARG}";;
        n)  NEW_VERSION="${OPTARG}";;
        d)  OLD_DIR="${OPTARG}";;
        p)  NEW_DIR="${OPTARG}";;
        h)  usage; exit 0;;
        \?) echo "Invalid parameter"; usage; return 1;;
    esac
done

set +u
if [ -z "$OLD_VERSION" ]; then
    OLD_VERSION=$(ncs --version)
fi
if [ -z "$NEW_VERSION" ]; then
    NEW_VERSION=$(ncs --version)
fi
if [ -z "$OLD_DIR" ]; then
    OLD_DIR=${NCS_DIR}
fi
if [ -z "$NEW_DIR" ]; then
    NEW_DIR=${NCS_DIR}
fi
set -u

if ! [ -d $NEW_DIR/packages/lsa/cisco-nso-nc-${OLD_VERSION::3} ]; then
    printf "\n${PURPLE}No $NEW_DIR/packages/lsa/cisco-nso-nc-${OLD_VERSION::3} for NSO $NEW_VERSION\n${NC}"
    printf "${RED}Upgrade not supported\n${NC}"
    exit 1
fi

function version_lt() { test "$(printf '%s\n' "$@" | sort -rV | head -n 1)" != "$1"; }
function version_ge() { test "$(printf '%s\n' "$@" | sort -rV | head -n 1)" == "$1"; }

NSO5=5
NSO53=5.3
NSO541=5.4.1
NSO55=5.5
NSO56=5.6

EXAMPLE_DIR=$(pwd)
# The LSA example changed name from "22-layered-service-architecture" to
# "22-lsa-single-version-deployment" in NSO 5.4.1
if version_lt $OLD_VERSION "5.4.1"; then
    LSA_EXAMPLE_DIR=22-layered-service-architecture
else
    LSA_EXAMPLE_DIR=22-lsa-single-version-deployment
fi
ORIGINAL_EXAMPLE_DIR=$OLD_DIR/examples.ncs/getting-started/developing-with-ncs/$LSA_EXAMPLE_DIR

printf "\n${PURPLE}##### Stop any running NSO or netsim instances\n${NC}"
set +u
source $NEW_DIR/ncsrc
set -u
set +e
if [ -d $EXAMPLE_DIR/22-layered-service-architecture ] ; then
    cd 22-layered-service-architecture
    ncs-netsim stop
    cd $EXAMPLE_DIR
elif [ -d $EXAMPLE_DIR/22-lsa-single-version-deployment ] ; then
    cd 22-lsa-single-version-deployment
    ncs-netsim stop
    cd $EXAMPLE_DIR
fi
NCS_IPC_PORT=4569 ncs --stop
NCS_IPC_PORT=4570 ncs --stop
NCS_IPC_PORT=4571 ncs --stop
set -e

printf "\n${PURPLE}##### Get a copy of the $OLD_VERSION $ORIGINAL_EXAMPLE_DIR\n${NC}"
rm -rf 22-lsa-single-version-deployment 22-layered-service-architecture packages
cp -r $ORIGINAL_EXAMPLE_DIR .
cp -r $ORIGINAL_EXAMPLE_DIR/../packages .

set +u
source $OLD_DIR/ncsrc
set -u

cd $EXAMPLE_DIR/packages/router/src
make clean
cd $EXAMPLE_DIR/$LSA_EXAMPLE_DIR
printf "\n${PURPLE}##### Build the $LSA_EXAMPLE_DIR example from version $OLD_VERSION\n${NC}"
make clean all

printf "\n${PURPLE}##### Start the NSO $OLD_VERSION simulated network and the three NSO nodes\n${NC}"
make start

printf "\n${PURPLE}##### Run the NSO $OLD_VERSION example and commit some CFS configuration\n${NC}"
if version_lt $OLD_VERSION $NSO541; then
    RFS_VLAN=rfs-vlan:vlan
else
    RFS_VLAN=rfs-vlan:services
fi
NCS_IPC_PORT=4569 ncs_cli -u admin -C << EOF
config
cfs-vlan v1 a-router ex0 z-router ex5 iface eth3 unit 3 vid 77
commit dry-run
commit
end
show packages
show running-config ncs:devices device config $RFS_VLAN | display xml | display service-meta-data
EOF

printf "\n${PURPLE}##### Always take a backup here if necessary. Stop the upper layer NSO instances and switch from $OLD_VERSION to the newer NSO $NEW_VERSION version\n${NC}"
NCS_IPC_PORT=4569 ncs --stop
cd "$EXAMPLE_DIR/$LSA_EXAMPLE_DIR/upper-nso/packages"

printf "\n${PURPLE}##### Execute the ncsrc commands for NSO $NEW_VERSION\n${NC}"
set +u
source $NEW_DIR/ncsrc
set -u
make clean all -C cfs-vlan/src

printf "\n${RED}##### Important step: ${PURPLE}Replace the old NSO $OLD_VERSION tailf/cisco-nso-nc-${OLD_VERSION::3} package with \n"
printf "                      the NSO $NEW_VERSION NETCONF NED package for ${OLD_VERSION::3}\n${NC}"
rm -rf *-nso-nc-${OLD_VERSION::3}
ln -sf ${NCS_DIR}/packages/lsa/cisco-nso-nc-${OLD_VERSION::3} .

# The LSA example rfs package changed name in NSO 5.4.1
if version_lt $OLD_VERSION $NSO541; then
    RFS_NED=rfs-ned
else
    RFS_NED=rfs-vlan-ned
fi

printf "${RED}##### Important step: ${PURPLE}Update the Makefile for the $RFS_NED package\n${NC}"
if version_lt $OLD_VERSION $NSO5; then
    # NSO 5 need the --lsa-netconf-ned flag which was not implemented for NSO 4.7
    printf "${RED}##### Important step: ${PURPLE}Add the --ncs-ned-id tailf-ncs-ned:lsa-netconf when upgrading from NSO 4.7 to 5.x \n${NC}"
    sed -i.bak "s/--ncs-device-type netconf/--ncs-device-type netconf --ncs-ned-id tailf-ncs-ned:lsa-netconf/" $RFS_NED/src/Makefile
    rm $RFS_NED/src/Makefile.bak
fi

sed -i.bak "s%NCSCPATH   =%NCSCPATH   = --yangpath \.\./\.\./cisco-nso-nc-${OLD_VERSION::3}/src/yang #%" $RFS_NED/src/Makefile
rm $RFS_NED/src/Makefile.bak

printf "\n${RED}##### Note: As we only change the upper NSO instance version, to keep this example simple, we skip the optional multi-version${NC}"
printf "\n${RED}##### $RFS_NED package name change, NED ID change, and migration by assuming we will not need to handle multiple different${NC}"
printf "\n${RED}##### versions of the lower NSO instances.\n\n${NC}"

printf "${RED}##### Important step: ${PURPLE}Rebuild the $RFS_NED package\n${NC}"
make clean all -C $RFS_NED/src

printf "${RED}##### Important step: ${PURPLE}Make the necessary changes to ncs.conf to upgrade from NSO $OLD_VERSION to $NEW_VERSION\n${NC}"
cd "$EXAMPLE_DIR/$LSA_EXAMPLE_DIR/upper-nso"
# NSO 5 removed the logs/syslog-config/udp parameter used in 4.7
if version_lt $OLD_VERSION $NSO5; then
    sed -i.bak '/<udp>/I,+3 d' ncs.conf
    rm ncs.conf.bak
fi
# NSO 5.3 added a mandatory encrypted-strings/AES256CFB128 key parameter
if version_lt $OLD_VERSION $NSO53; then # New version assumed to be > 5.4.1
    sed -i.bak 's%</encrypted-strings>%<AES256CFB128><key>0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef</key></AES256CFB128></encrypted-strings>%'\
                ncs.conf
    rm ncs.conf.bak
fi
# NSO 5.5 added a trace-id, enabled by default, which is not recognized by the
# earlier versions of NSO. The show-log-directory parameter was removed.
if version_lt $OLD_VERSION $NSO55 && version_ge $NEW_VERSION $NSO55; then
    sed -i.bak -e 's%</logs>%<trace-id>false</trace-id></logs>%'\
               -e 's%<show-log-directory>./logs</show-log-directory>%%'\
               ncs.conf
    rm ncs.conf.bak
fi
# NSO 5.6 removed the large-scale parameters
if version_lt $OLD_VERSION $NSO56 && version_ge $NEW_VERSION $NSO56; then
    sed -i.bak '/<large-scale>/I,+7 d' ncs.conf
    rm ncs.conf.bak
fi

printf "${PURPLE}##### Restart the upper NSO instance and upgrade to ${NEW_VERSION::3}\n${NC}"
ncs --cdb-compact ncs-cdb
NCS_IPC_PORT=4569 sname=upper-nso ncs -c ncs.conf --with-package-reload

if version_lt $OLD_VERSION $NSO5 && version_ge $NEW_VERSION $NSO56; then
    printf "${RED}##### Important step: ${PURPLE}When upgrading from NSO 4.7 to 5.6+ we need to add"
    printf " ssh-rsa to the list of supported algorithms on the 5.6+ upper NSO instance\n${NC}"
    NCS_IPC_PORT=4569 ncs_cli -u admin -C << EOF
config
ncs:devices global-settings ssh-algorithms public-key ssh-rsa
commit
EOF
fi

printf "\n${PURPLE}##### Check if there are changes using re-deploy\n${NC}"
NCS_IPC_PORT=4569 ncs_cli -u admin -C << EOF
config
cfs-vlan v1 re-deploy dry-run
EOF

printf "\n${PURPLE}##### No changes should be indicated.\n${NC}"
printf "\n${PURPLE}##### Also, check the refcounters and back pointers\n${NC}"

NCS_IPC_PORT=4569 ncs_cli -u admin -C << EOF
show running-config ncs:devices device config $RFS_VLAN | display xml | display service-meta-data
EOF

printf "\n${PURPLE}##### At this stage we have a CFS node built for version $NEW_VERSION and 2 RFS nodes built for version $OLD_VERSION.\n${NC}"
printf "${PURPLE}##### The cfs-vlan and $RFS_NED packages should still have the same ned-id.\n${NC}"
printf "${PURPLE}##### Thus no migration was needed and no models have been discarded.\n${NC}"
NCS_IPC_PORT=4569 ncs_cli -u admin -C << EOF
show packages
EOF

printf "\n${GREEN}##### DONE!\n${NC}"
