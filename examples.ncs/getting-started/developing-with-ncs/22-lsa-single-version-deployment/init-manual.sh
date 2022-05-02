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
