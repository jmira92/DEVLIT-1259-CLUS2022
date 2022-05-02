#!/bin/sh

set -e

echo "Initialize NSO nodes:"

echo "On lower-nso-1: fetch ssh keys from devices"
echo "On lower-nso-1: perform sync-from"
ncs_cli --port 4570 -u admin >/dev/null <<EOF
request devices fetch-ssh-host-keys
request devices sync-from
EOF

echo "On lower-nso-2: fetch ssh keys from devices"
echo "On lower-nso-2: perform sync-from"
ncs_cli -u admin --port 4571 >/dev/null <<EOF2
request devices fetch-ssh-host-keys
request devices sync-from
EOF2

echo "On lower-nso-3: fetch ssh keys from devices"
echo "On lower-nso-3: perform sync-from"
ncs_cli -u admin --port 4572 >/dev/null <<EOF2
request devices fetch-ssh-host-keys
request devices sync-from
EOF2

## Must sync-from nso-upper last, since their sync-froms
## change their CDB

echo "On upper-nso: fetch ssh keys from devices"
echo "On upper-nso: perform sync-from"
echo "On upper-nso: configure cluster remote nodes: lower-nso-1 and lower-nso-2"
echo "On upper-nso: enable cluster device-notifications and cluster commit-queue"
echo "On upper-nso: fetch ssh keys from cluster remote nodes"
ncs_cli --port 4569 -u admin >/dev/null <<EOF2
request devices fetch-ssh-host-keys
request devices sync-from
config
set cluster device-notifications enabled
set cluster remote-node lower-nso-1 address 127.0.0.1 port 2023 authgroup default username admin
set cluster remote-node lower-nso-2 address 127.0.0.1 port 2024 authgroup default username admin
commit
set cluster commit-queue enabled
commit
request cluster remote-node lower-nso-1 ssh fetch-host-keys
request cluster remote-node lower-nso-2 ssh fetch-host-keys
exit
EOF2
