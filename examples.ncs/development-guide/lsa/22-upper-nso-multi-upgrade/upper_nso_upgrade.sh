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
   printf "  -o  Old NSO version. 5.x or newer Default: Output of the ncs --version command\n"
   printf "  -n  New NSO version. 5.4.1 or newer. Default: Output of the ncs --version command\n\n"
   printf "I.e. if only default values are used the upgrade will be to the ${RED}same NSO version${GREEN},\n"
   printf "still performing the same upgrade steps for demo purposes.\n"
   printf "\nTo, for example, upgrade the 5.4.5 22-layered-service-architecture example upper layer NSO to 5.7:\n\n"
   printf "  \$ ./upper_nso_upgrade.sh -o 5.4.5 -d /Users/tailf/nso-5.4.5 -n 5.7 -p /Users/tailf/nso-5.7\n\n${NC}"
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

if version_lt $OLD_VERSION $NSO5; then
    printf "${RED}Not possible to upgrade from $OLD_VERSION to a multi-version NED. First upgrade $OLD_VERSION to a 5.x single version NED\n${NC}"
    exit 1
fi

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

printf "\n${PURPLE}##### Run the NSO $OLD_VERSION example and commit some example CFS configuration.\n${NC}"
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

MULTIVER_RFS_NED=rfs-vlan-nc-${OLD_VERSION::3}
printf "${RED}##### Multi-version step: ${PURPLE}Copy the $RFS_NED to $MULTIVER_RFS_NED \n${NC}"
cp -r $RFS_NED $MULTIVER_RFS_NED

printf "${RED}##### Multi-version step: ${PURPLE}Have the RFS NED makefile --ncs-ned-id flag use the multi-version NED ID\n${NC}"
# Replace the existing lsa-netconf NED ID with the multiversion variant
sed -i.bak "s/--ncs-ned-id tailf-ncs-ned:lsa-netconf/--ncs-ned-id cisco-nso-nc-${OLD_VERSION::3}:cisco-nso-nc-${OLD_VERSION::3}/" $MULTIVER_RFS_NED/src/Makefile
rm $MULTIVER_RFS_NED/src/Makefile.bak
sed -i.bak "s%NCSCPATH   =%NCSCPATH   = --yangpath \.\./\.\./cisco-nso-nc-${OLD_VERSION::3}/src --ncs-depend-package \.\./\.\./cisco-nso-nc-${OLD_VERSION::3} #%" $MULTIVER_RFS_NED/src/Makefile
rm $MULTIVER_RFS_NED/src/Makefile.bak
sed -i.bak "s%NCSCPATH   =%NCSCPATH   = --yangpath \.\./\.\./cisco-nso-nc-${OLD_VERSION::3}/src/yang #%" $RFS_NED/src/Makefile
rm $RFS_NED/src/Makefile.bak

printf "${RED}##### Multi-version step: ${PURPLE}Update the package-meta-data.xml for the $MULTIVER_RFS_NED package\n${NC}"
sed -i.bak -e "s%$RFS_NED%$MULTIVER_RFS_NED%g" $MULTIVER_RFS_NED/src/package-meta-data.xml.in
rm $MULTIVER_RFS_NED/src/package-meta-data.xml.in.bak
sed -i.bak -e "s%<ned-id xmlns:id=\"http://tail-f.com/ns/ncs-ned\">id:lsa-netconf</ned-id>%<ned-id xmlns:id=\"http://tail-f.com/ns/ned-id/cisco-nso-nc-${OLD_VERSION::3}\">id:cisco-nso-nc-${OLD_VERSION::3}</ned-id>%g" $MULTIVER_RFS_NED/src/package-meta-data.xml.in
rm $MULTIVER_RFS_NED/src/package-meta-data.xml.in.bak

printf "${RED}##### Multi-version step: ${PURPLE}Rebuild the single and multi-version RFS packages\n${NC}"
make clean all -C $MULTIVER_RFS_NED/src
make clean all -C $RFS_NED/src

printf "${RED}##### Important step: ${PURPLE}Make the necessary changes to ncs.conf to upgrade from NSO $OLD_VERSION to $NEW_VERSION\n${NC}"
cd "$EXAMPLE_DIR/$LSA_EXAMPLE_DIR/upper-nso"
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

printf "${RED}##### Multi-version step: ${PURPLE}Migrate the upper NSO single version NED ID to a multi-version NED ID\n${NC}"
NCS_IPC_PORT=4569 ncs_cli -u admin -C << EOF
config
ncs:devices device lower-nso-* ssh fetch-host-keys
ncs:devices device lower-nso-1 migrate new-ned-id cisco-nso-nc-${OLD_VERSION::3}
ncs:devices device lower-nso-2 migrate new-ned-id cisco-nso-nc-${OLD_VERSION::3}
ncs:devices device lower-nso-* out-of-sync-commit-behaviour accept
commit
EOF

printf "${RED}##### Multi-version step: ${PURPLE}Delete the single version $RFS_NED package and reload packages\n${NC}"
printf "${RED}##### Warning: ${PURPLE}If the migrate to the multi-version NED ID earlier failed, we will loose data if we delete the single version NED\n${NC}"
rm -rf $EXAMPLE_DIR/$LSA_EXAMPLE_DIR/upper-nso/packages/$RFS_NED
NCS_IPC_PORT=4569 ncs_cli -u admin -C << EOF
packages reload force
EOF

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
