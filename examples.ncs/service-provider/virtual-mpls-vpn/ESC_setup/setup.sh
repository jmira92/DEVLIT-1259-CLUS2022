#!/bin/bash

ncs_cli -u admin << EOF
  configure
  load merge auth.xml
  commit
  load merge ESC_device.xml
  commit
  request devices device esc0 ssh fetch-host-keys
  commit
  request devices device esc0 sync-from
  set devices device esc0 config esc:esc_datamodel networks network p_net
  commit
  set devices device esc0 config esc:esc_datamodel networks network ce_net
  commit
EOF

#Have to sleep a little after creating the networks

sleep 2
ncs_cli -u admin << EOF
  configure
  load merge p_net.xml
  commit
  load merge ce_net.xml
  commit
  load merge tenant.xml
  commit
  load merge service_reg.xml
  commit
EOF

