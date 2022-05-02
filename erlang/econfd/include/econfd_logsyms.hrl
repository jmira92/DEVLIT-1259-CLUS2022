%% File generated from lib/build_tools/src/logdefs.txt
%% 

%% generated from lib/build_tools/src/logdefs.txt


%% ----------------------------------------------------------------------
%% All of these can be produced by NCS while starting, they all
%% are fatal and NCS will stop. They are syslog messages.
%% ----------------------------------------------------------------------

%% 
%%
%% "CDB failed to start. Some grave error in the cdb data files prevented CDB from starting - a recovery from backup is necessary."
-define( LOGSYM_CDB_BOOT_ERR, 1).
%% 
%%
%% "NCS failed to bind to one of the internally used listen sockets."
-define( LOGSYM_BIND_ERR, 2).
%% 
%%
%% "While validating the consistency of the config - a required namespace was missing."
-define( LOGSYM_MISSING_NS, 3).
%% 
%%
%% "While validating the consistency of the config - a required namespace was missing."
-define( LOGSYM_MISSING_NS2, 4).
%% 
%%
%% "Two namespaces have the same hash value. The namespace hashvalue MUST be unique.  You can pass the flag --nshash <value> to confdc when linking the .xso files to force another value for the namespace hash."
-define( LOGSYM_BAD_NS_HASH, 5).
%% 
%%
%% "The fxs file with the base identity is not loaded"
-define( LOGSYM_NO_SUCH_IDENTITY, 6).
%% 
%%
%% "A nonexistent namespace was referred to. Typically this means that a .fxs was missing from the loadPath."
-define( LOGSYM_NO_SUCH_NS, 7).
%% 
%%
%% "A nonexistent type was referred to from a ns. Typically this means that a bad version of an .fxs file was found in the loadPath."
-define( LOGSYM_NO_SUCH_TYPE, 8).
%% 
%%
%% "NCS failed to bind to one of the externally visible listen sockets."
-define( LOGSYM_EXT_BIND_ERR, 9).
%% 
%%
%% "NCS failed to accept a connection due to reaching the process or system-wide file descriptor limit."
-define( LOGSYM_ACCEPT_FDLIMIT, 10).
%% 
%%
%% "NCS encountered an OS-specific error indicating that networking support is unavailable."
-define( LOGSYM_ACCEPT_FATAL, 11).
%% 
%%
%% "Duplicate prefix found."
-define( LOGSYM_DUPLICATE_PREFIX, 12).
%% 
%%
%% "File error"
-define( LOGSYM_FILE_ERROR, 13).

%% ----------------------------------------------------------------------
%% All of these are regular syslog messages.
%% ----------------------------------------------------------------------

%% 
%%
%% "An external database daemon closed its control socket."
-define( LOGSYM_DAEMON_DIED, 14).
%% 
%%
%% "An external database daemon did not respond to a query."
-define( LOGSYM_DAEMON_TIMEOUT, 15).
%% 
%%
%% "NCS tried to populate an XML tree but no code had registered under the relevant callpoint."
-define( LOGSYM_NO_CALLPOINT, 16).
%% 
%%
%% "CDB found it's data schema file but not it's data file. CDB recovers by starting from an empty database."
-define( LOGSYM_CDB_DB_LOST, 17).
%% 
%%
%% "CDB found it's data files but no schema file. CDB recovers by starting from an empty database."
-define( LOGSYM_CDB_CONFIG_LOST, 18).
%% 
%%
%% "Automatic CDB upgrade failed. This means that the data model has been changed in a non-supported way."
-define( LOGSYM_CDB_UPGRADE_FAILED, 19).
%% 
%%
%% "CDB is processing an initialization file."
-define( LOGSYM_CDB_INIT_LOAD, 20).
%% 
%%
%% "The operational DB was deleted and re-initialized (because of upgrade or corrupt file)"
-define( LOGSYM_CDB_OP_INIT, 21).
%% 
%%
%% "A CDB client failed to answer within the timeout period. The client will be disconnected."
-define( LOGSYM_CDB_CLIENT_TIMEOUT, 22).
%% 
%%
%% "A NCS internal error - should be reported to support@tail-f.com."
-define( LOGSYM_INTERNAL_ERROR, 23).
%% 
%%
%% "Failed to load the AAA data, it could be that an external db is misbehaving or AAA is mounted/populated badly"
-define( LOGSYM_AAA_LOAD_FAIL, 24).
%% 
%%
%% "Authentication is external and the external program returned badly formatted data."
-define( LOGSYM_EXTAUTH_BAD_RET, 25).
%% 
%%
%% "NCS is configured to start the confd_aaa_bridge and the C program died."
-define( LOGSYM_BRIDGE_DIED, 26).
%% 
%%
%% "NCS has just started its start phase 0."
-define( LOGSYM_PHASE0_STARTED, 27).
%% 
%%
%% "NCS has just started its start phase 1."
-define( LOGSYM_PHASE1_STARTED, 28).
%% 
%%
%% "NCS has started."
-define( LOGSYM_STARTED, 29).
%% 
%%
%% "In-service upgrade initialization has started."
-define( LOGSYM_UPGRADE_INIT_STARTED, 30).
%% 
%%
%% "In-service upgrade initialization succeeded."
-define( LOGSYM_UPGRADE_INIT_SUCCEEDED, 31).
%% 
%%
%% "In-service upgrade has been performed (not committed yet)."
-define( LOGSYM_UPGRADE_PERFORMED, 32).
%% 
%%
%% "In-service upgrade was committed."
-define( LOGSYM_UPGRADE_COMMITTED, 33).
%% 
%%
%% "In-service upgrade was aborted."
-define( LOGSYM_UPGRADE_ABORTED, 34).
%% 
%%
%% "NCS is reading its configuration file."
-define( LOGSYM_CONSULT_FILE, 35).
%% 
%%
%% "NCS is stopping (due to e.g. ncs --stop)."
-define( LOGSYM_STOPPING, 36).
%% 
%%
%% "Reload of daemon configuration has been initiated."
-define( LOGSYM_RELOAD, 37).
%% 
%%
%% "ncs.conf contained bad data."
-define( LOGSYM_BADCONFIG, 38).
%% 
%%
%% "Writing of a state file failed"
-define( LOGSYM_WRITE_STATE_FILE_FAILED, 39).
%% 
%%
%% "Reading of a state file failed"
-define( LOGSYM_READ_STATE_FILE_FAILED, 40).
%% 
%%
%% "Typically errors where the client doesn't properly send the \"subsystem\" command."
-define( LOGSYM_SSH_SUBSYS_ERR, 41).
%% 
%%
%% "Session limit reached, rejected new session request."
-define( LOGSYM_SESSION_LIMIT, 42).
%% 
%%
%% "Configuration transaction limit reached, rejected new transaction request."
-define( LOGSYM_CONFIG_TRANSACTION_LIMIT, 43).
%% 
%%
%% "Aborting candidate commit, request from user, reverting configuration."
-define( LOGSYM_ABORT_CAND_COMMIT, 44).
%% 
%%
%% "Candidate commit timer expired, reverting configuration."
-define( LOGSYM_ABORT_CAND_COMMIT_TIMER, 45).
%% 
%%
%% "Candidate commit session terminated, reverting configuration."
-define( LOGSYM_ABORT_CAND_COMMIT_TERM, 46).
%% 
%%
%% "Found half created rollback0 file - removing and creating new."
-define( LOGSYM_ROLLBACK_REMOVE, 47).
%% 
%%
%% "Found half created rollback0 file - repairing."
-define( LOGSYM_ROLLBACK_REPAIR, 48).
%% 
%%
%% "Failed to repair rollback files."
-define( LOGSYM_ROLLBACK_FAIL_REPAIR, 49).
%% 
%%
%% "Error while creating rollback file."
-define( LOGSYM_ROLLBACK_FAIL_CREATE, 50).
%% 
%%
%% "Failed to rename rollback file."
-define( LOGSYM_ROLLBACK_FAIL_RENAME, 51).
%% 
%%
%% "System tried to process a loaded namespace and failed."
-define( LOGSYM_NS_LOAD_ERR, 52).
%% 
%%
%% "System tried to process a loaded namespace and failed."
-define( LOGSYM_NS_LOAD_ERR2, 53).
%% 
%%
%% "System tried to load a file in its load path and failed."
-define( LOGSYM_FILE_LOAD_ERR, 54).
%% 
%%
%% "System starts to load a file."
-define( LOGSYM_FILE_LOADING, 55).
%% 
%%
%% "System skips a file."
-define( LOGSYM_SKIP_FILE_LOADING, 56).
%% 
%%
%% "System loaded a file."
-define( LOGSYM_FILE_LOAD, 57).
%% 
%%
%% "NCS starts or stops to listen for incoming connections."
-define( LOGSYM_LISTENER_INFO, 58).
%% 
%%
%% "The cleartext header indicating user and groups was badly formatted."
-define( LOGSYM_NETCONF_HDR_ERR, 59).
%% 
%%
%% "An application connecting to NCS used a library version that doesn't match the NCS version (e.g. old version of the client library)."
-define( LOGSYM_LIB_BAD_VSN, 60).
%% 
%%
%% "An application connecting to NCS used a library version that can't handle the depth and number of keys used by the data model."
-define( LOGSYM_LIB_BAD_SIZES, 61).
%% 
%%
%% "Access check failure occurred when an application connected to NCS."
-define( LOGSYM_LIB_NO_ACCESS, 62).
%% 
%%
%% "An UDP package was received on the trap receiving port, but it's not an SNMP trap."
-define( LOGSYM_SNMP_NOT_A_TRAP, 63).
%% 
%%
%% "An SNMP v1 trap was received on the trap receiving port, but forwarding v1 traps is not supported."
-define( LOGSYM_SNMP_TRAP_V1, 64).
%% 
%%
%% "An SNMP trap was to be forwarded, but couldn't be."
-define( LOGSYM_SNMP_TRAP_NOT_FORWARDED, 65).
%% 
%%
%% "An SNMP trap was to be forwarded, but the sender was not listed in ncs.conf."
-define( LOGSYM_SNMP_TRAP_UNKNOWN_SENDER, 66).
%% 
%%
%% "The port for listening to SNMP traps could not be opened."
-define( LOGSYM_SNMP_TRAP_OPEN_PORT, 67).
%% 
%%
%% "An SNMP trap was received on the trap receiving port, but its definition is not known"
-define( LOGSYM_SNMP_TRAP_NOT_RECOGNIZED, 68).
%% 
%%
%% "An error occurred while evaluating an XPath expression."
-define( LOGSYM_XPATH_EVAL_ERROR1, 69).
%% 
%%
%% "An error occurred while evaluating an XPath expression."
-define( LOGSYM_XPATH_EVAL_ERROR2, 70).
%% 
%%
%% "The candidate database file has a bad format. The candidate database is reset to the empty database."
-define( LOGSYM_CANDIDATE_BAD_FILE_FORMAT, 71).
%% 
%%
%% "The candidate database file is corrupt and cannot be read. The candidate database is reset to the empty database."
-define( LOGSYM_CANDIDATE_CORRUPT_FILE, 72).
%% 
%%
%% "DES3CBC keys were not found in ncs.conf"
-define( LOGSYM_MISSING_DES3CBC_SETTINGS, 73).
%% 
%%
%% "AESCFB128 keys were not found in ncs.conf"
-define( LOGSYM_MISSING_AESCFB128_SETTINGS, 74).
%% 
%%
%% "SNMP Agent loading a MIB file"
-define( LOGSYM_SNMP_MIB_LOADING, 75).
%% 
%%
%% "The SNMP Agent failed to load a MIB file"
-define( LOGSYM_SNMP_CANT_LOAD_MIB, 76).
%% 
%%
%% "Write SNMP agent state file failed"
-define( LOGSYM_SNMP_WRITE_STATE_FILE_FAILED, 77).
%% 
%%
%% "Read SNMP agent state file failed"
-define( LOGSYM_SNMP_READ_STATE_FILE_FAILED, 78).
%% 
%%
%%    "The SNMP agent requires CDB to be enabled in order to be started."
-define( LOGSYM_SNMP_REQUIRES_CDB, 79).
%% 
%%
%% "A slave connected to a master where the fxs files are different"
-define( LOGSYM_FXS_MISMATCH, 80).
%% 
%%
%% "A slave connected to a master with a bad auth token"
-define( LOGSYM_TOKEN_MISMATCH, 81).
%% 
%%
%% "A slave node didn't produce its ticks"
-define( LOGSYM_HA_SLAVE_KILLED, 82).
%% 
%%
%% "A slave arrived with a node id which already exists"
-define( LOGSYM_HA_DUPLICATE_NODEID, 83).
%% 
%%
%% "An attempted library become slave call failed because the slave couldn't connect to the master"
-define( LOGSYM_HA_FAILED_CONNECT, 84).
%% 
%%
%% "A slave connected to a master with an incompatible HA protocol version"
-define( LOGSYM_HA_BAD_VSN, 85).
%% 
%%
%% "NETCONF traffic log message"
-define( LOGSYM_NETCONF, 86).
%% 
%%
%% "Developer webui log message"
-define( LOGSYM_DEVEL_WEBUI, 87).
%% 
%%
%% "Developer aaa log message"
-define( LOGSYM_DEVEL_AAA, 88).
%% 
%%
%% "Developer C api log message"
-define( LOGSYM_DEVEL_CAPI, 89).
%% 
%%
%% "Developer CDB log message"
-define( LOGSYM_DEVEL_CDB, 90).
%% 
%%
%% "Developer NCS log message"
-define( LOGSYM_DEVEL_CONFD, 91).
%% 
%%
%% "Developer snmp GW log message"
-define( LOGSYM_DEVEL_SNMPGW, 92).
%% 
%%
%% "Developer snmp agent log message"
-define( LOGSYM_DEVEL_SNMPA, 93).
%% 
%%
%% "A failure occurred in the builtin notification replay store"
-define( LOGSYM_NOTIFICATION_REPLAY_STORE_FAILURE, 94).
%% 
%%
%% "An event notification subscriber did not reply within the configured timeout period"
-define( LOGSYM_EVENT_SOCKET_TIMEOUT, 95).
%% 
%%
%%        "Write on an event socket blocked for too long time"
-define( LOGSYM_EVENT_SOCKET_WRITE_BLOCK, 96).
%% 
%%
%%   "Data was committed toward a device with bad or unknown sync state"
-define( LOGSYM_COMMIT_UN_SYNCED_DEV, 97).
%% 
%%
%% "Failed to locate snmp_init.xml in loadpath"
-define( LOGSYM_NCS_SNMP_INIT_ERR, 98).
%% 
%%
%% "Starting the NCS Java VM"
-define( LOGSYM_NCS_JAVA_VM_START, 99).
%% 
%%
%% "The NCS Java VM failure/timeout"
-define( LOGSYM_NCS_JAVA_VM_FAIL, 100).
%% 
%%
%% "Syntax error in package file"
-define( LOGSYM_NCS_PACKAGE_SYNTAX_ERROR, 101).
%% 
%%
%% "Duplicate package found"
-define( LOGSYM_NCS_PACKAGE_DUPLICATE, 102).
%% 
%%
%% "A package is copied from the load path to private directory"
-define( LOGSYM_NCS_PACKAGE_COPYING, 103).
%% 
%%
%% "The CDB upgrade was aborted implying that CDB is untouched. However the package state is changed"
-define( LOGSYM_NCS_PACKAGE_UPGRADE_ABORTED, 104).
%% 
%%
%% "Bad NCS version for package"
-define( LOGSYM_NCS_PACKAGE_BAD_NCS_VERSION, 105).
%% 
%%
%% "Bad NCS package dependency"
-define( LOGSYM_NCS_PACKAGE_BAD_DEPENDENCY, 106).
%% 
%%
%% "Circular NCS package dependency"
-define( LOGSYM_NCS_PACKAGE_CIRCULAR_DEPENDENCY, 107).

%% ----------------------------------------------------------------------
%% Here come all the audit messages.
%% ----------------------------------------------------------------------

%% 
%%
%% "User executed a CLI command."
-define( LOGSYM_CLI_CMD, 108).
%% 
%%
%% "User was denied to execute a CLI command due to permissions."
-define( LOGSYM_CLI_DENIED, 109).
%% 
%%
%% "RESERVED_110"
-define( LOGSYM_RESERVED_110, 110).
%% 
%%
%% "RESERVED_111"
-define( LOGSYM_RESERVED_111, 111).
%% 
%%
%% "RESERVED_112"
-define( LOGSYM_RESERVED_112, 112).
%% 
%%
%% "RESERVED_113"
-define( LOGSYM_RESERVED_113, 113).
%% 
%%
%% "RESERVED_114"
-define( LOGSYM_RESERVED_114, 114).
%% 
%%
%% "RESERVED_115"
-define( LOGSYM_RESERVED_115, 115).
%% 
%%
%% "A user was assigned to a set of groups."
-define( LOGSYM_GROUP_ASSIGN, 116).
%% 
%%
%% "A user was logged in but wasn't assigned to any groups at all."
-define( LOGSYM_GROUP_NO_ASSIGN, 117).
%% 
%%
%% "A maapi user was logged out."
-define( LOGSYM_MAAPI_LOGOUT, 118).
%% 
%%
%% "RESERVED_119"
-define( LOGSYM_RESERVED_119, 119).
%% 
%%
%% "RESERVED_120"
-define( LOGSYM_RESERVED_120, 120).
%% 
%%
%% "RESERVED_121"
-define( LOGSYM_RESERVED_121, 121).
%% 
%%
%% "A user used the --noaaa flag to ncs_cli"
-define( LOGSYM_NOAAA_CLI_LOGIN, 122).
%% 
%%
%% "RESERVED_123"
-define( LOGSYM_RESERVED_123, 123).
%% 
%%
%% "RESERVED_124"
-define( LOGSYM_RESERVED_124, 124).
%% 
%%
%% "User executed a Web UI command."
-define( LOGSYM_WEB_CMD, 125).
%% 
%%
%% "User executed a Web UI action."
-define( LOGSYM_WEB_ACTION, 126).
%% 
%%
%% "User performed Web UI commit."
-define( LOGSYM_WEB_COMMIT, 127).
%% 
%%
%% "An SNMP authentication failed."
-define( LOGSYM_SNMP_AUTHENTICATION_FAILED, 128).
%% 
%%
%% "Authentication for a user was rejected by application callback."
-define( LOGSYM_LOGIN_REJECTED, 129).
%% 
%%
%% "Information about configuration changes committed to the running data store."
-define( LOGSYM_COMMIT_INFO, 130).
%% 
%%
%% "CLI command finished successfully."
-define( LOGSYM_CLI_CMD_DONE, 131).
%% 
%%
%% "CLI command aborted."
-define( LOGSYM_CLI_CMD_ABORTED, 132).
%% 
%%
%% "A check-sync action reported out-of-sync for a device"
-define( LOGSYM_NCS_DEVICE_OUT_OF_SYNC, 133).
%% 
%%
%% "A check-sync action reported out-of-sync for a service"
-define( LOGSYM_NCS_SERVICE_OUT_OF_SYNC, 134).
%% 
%%
%% "Starting the named NCS Python VM"
-define( LOGSYM_NCS_PYTHON_VM_START, 135).
%% 
%%
%% "The NCS Python VM failure/timeout"
-define( LOGSYM_NCS_PYTHON_VM_FAIL, 136).
%% 
%%
%% "The device failed to set the platform operational data at connect"
-define( LOGSYM_NCS_SET_PLATFORM_DATA_ERROR, 137).
%% 
%%
%% "Starting the NCS Smart Licensing Java VM"
-define( LOGSYM_NCS_SMART_LICENSING_START, 138).
%% 
%%
%% "The NCS Smart Licensing Java VM failure/timeout"
-define( LOGSYM_NCS_SMART_LICENSING_FAIL, 139).
%% 
%%
%% "Smart Licensing Global Notification"
-define( LOGSYM_NCS_SMART_LICENSING_GLOBAL_NOTIFICATION, 140).
%% 
%%
%% "Smart Licensing Entitlement Notification"
-define( LOGSYM_NCS_SMART_LICENSING_ENTITLEMENT_NOTIFICATION, 141).
%% 
%%
%% "Smart Licensing evaluation time remaining"
-define( LOGSYM_NCS_SMART_LICENSING_EVALUATION_COUNTDOWN, 142).
%% 
%%
%% "Developer smartlicensing api log message"
-define( LOGSYM_DEVEL_SLS, 143).
%% 
%%
%% "JSON-RPC method requested."
-define( LOGSYM_JSONRPC_REQUEST, 144).
%% 
%%
%% "Developer econfd api log message"
-define( LOGSYM_DEVEL_ECONFD, 145).
%% 
%%
%% "CDB encounterad an unrecoverable error"
-define( LOGSYM_CDB_FATAL_ERROR, 146).
%% 
%%
%% "Logging subsystem started"
-define( LOGSYM_LOGGING_STARTED, 147).
%% 
%%
%% "Logging subsystem terminating"
-define( LOGSYM_LOGGING_SHUTDOWN, 148).
%% 
%%
%% "Logging subsystem, reopening log files"
-define( LOGSYM_REOPEN_LOGS, 149).
%% 
%%
%% "Indicate target file for certain type of logging"
-define( LOGSYM_OPEN_LOGFILE, 150).
%% 
%%
%% "Write logs for a subsystem to a specific file"
-define( LOGSYM_LOGGING_STARTED_TO, 151).
%% 
%%
%% "The target logfile will change to another file"
-define( LOGSYM_LOGGING_DEST_CHANGED, 152).
%% 
%%
%% "Notify a change of logging status (enabled/disabled) for a subsystem"
-define( LOGSYM_LOGGING_STATUS_CHANGED, 153).
%% 
%%
%% "Notify change of log size for error log"
-define( LOGSYM_ERRLOG_SIZE_CHANGED, 154).
%% 
%%
%% "CGI script requested."
-define( LOGSYM_CGI_REQUEST, 155).
%% 
%%
%% "Failed to setup the shared memory schema"
-define( LOGSYM_MMAP_SCHEMA_FAIL, 156).
%% 
%%
%% "Failed to load kicker schema"
-define( LOGSYM_KICKER_MISSING_SCHEMA, 157).
%% 
%%
%% "JSON-RPC idle timeout."
-define( LOGSYM_JSONRPC_REQUEST_IDLE_TIMEOUT, 158).
%% 
%%
%% "JSON-RPC absolute timeout."
-define( LOGSYM_JSONRPC_REQUEST_ABSOLUTE_TIMEOUT, 159).
%% 
%%
%% "A dependency was not found"
-define( LOGSYM_BAD_DEPENDENCY, 160).
%% 
%%
%% "A rest authenticated user logged in."
-define( LOGSYM_REST_AUTH_SUCCESS, 161).
%% 
%%
%% "Rest authentication for a user failed."
-define( LOGSYM_REST_AUTH_FAIL, 162).
%% 
%%
%% "NCS is starting."
-define( LOGSYM_STARTING, 163).
%% 
%%
%% "Package upgrade has been aborted due to warnings."
-define( LOGSYM_NCS_PACKAGE_UPGRADE_UNSAFE, 164).
%% 
%%
%% "A locally authenticated user logged in."
-define( LOGSYM_LOCAL_AUTH_SUCCESS, 165).
%% 
%%
%% "Authentication for a locally configured user failed."
-define( LOGSYM_LOCAL_AUTH_FAIL, 166).
%% 
%%
%% "A PAM authenticated user logged in."
-define( LOGSYM_PAM_AUTH_SUCCESS, 167).
%% 
%%
%% "A user failed to authenticate through PAM."
-define( LOGSYM_PAM_AUTH_FAIL, 168).
%% 
%%
%% "An externally authenticated user logged in."
-define( LOGSYM_EXT_AUTH_SUCCESS, 169).
%% 
%%
%% "External authentication failed for a user."
-define( LOGSYM_EXT_AUTH_FAIL, 170).
%% 
%%
%% "A user logged into NCS."
-define( LOGSYM_AUTH_LOGIN_SUCCESS, 171).
%% 
%%
%% "A user failed to log in to NCS."
-define( LOGSYM_AUTH_LOGIN_FAIL, 172).
%% 
%%
%% "A user was logged out from NCS."
-define( LOGSYM_AUTH_LOGOUT, 173).
%% 
%%
%% "A new user session was created"
-define( LOGSYM_SESSION_CREATE, 174).
%% 
%%
%% "A user session was terminated due to specified reason"
-define( LOGSYM_SESSION_TERMINATION, 175).
%% 
%%
%% "A user failed to create a new user sessions due to exceeding sessions limits"
-define( LOGSYM_SESSION_MAX_EXCEEDED, 176).
%% 
%%
%% "NCS restarted while having a ongoing candidate commit timer, reverting configuration."
-define( LOGSYM_ABORT_CAND_COMMIT_REBOOT, 177).
%% 
%%
%% "Failed to rollback candidate commit"
-define( LOGSYM_CAND_COMMIT_ROLLBACK_FAILURE, 178).
%% 
%%
%% "Candidate commit rollback done"
-define( LOGSYM_CAND_COMMIT_ROLLBACK_DONE, 179).
%% 
%%
%% "JSON-RPC traffic log message"
-define( LOGSYM_JSONRPC_LOG_MSG, 180).
%% 
%%
%% "JSON-RPC warning message"
-define( LOGSYM_JSONRPC_WARN_MSG, 181).
%% 
%%
%% "REST request"
-define( LOGSYM_REST_REQUEST, 182).
%% 
%%
%% "REST response"
-define( LOGSYM_REST_RESPONSE, 183).
%% 
%%
%% "RESTCONF request"
-define( LOGSYM_RESTCONF_REQUEST, 184).
%% 
%%
%% "RESTCONF response"
-define( LOGSYM_RESTCONF_RESPONSE, 185).
%% 
%%
%% "AES256CFB128 keys were not found in ncs.conf"
-define( LOGSYM_MISSING_AES256CFB128_SETTINGS, 186).
%% 
%%
%% "Starting a Python VM to run upgrade code"
-define( LOGSYM_NCS_PYTHON_VM_START_UPGRADE, 187).
%% 
%%
%% "Starting the NCS SNMP manager component"
-define( LOGSYM_NCS_SNMPM_START, 188).
%% 
%%
%% "The NCS SNMP manager component has been stopped"
-define( LOGSYM_NCS_SNMPM_STOP, 189).
%% 
%%
%% "WebUI access log message"
-define( LOGSYM_WEBUI_LOG_MSG, 190).
%% 
%%
%% "Authentication for a locally configured user failed due to user not found."
-define( LOGSYM_LOCAL_AUTH_FAIL_NOUSER, 191).
%% 
%%
%% "Authentication for a locally configured user failed due to providing bad password."
-define( LOGSYM_LOCAL_AUTH_FAIL_BADPASS, 192).
%% 
%%
%% "An externally token authenticated user logged in."
-define( LOGSYM_EXT_AUTH_TOKEN_SUCCESS, 193).
%% 
%%
%% "External token authentication failed for a user."
-define( LOGSYM_EXT_AUTH_TOKEN_FAIL, 194).
%% 
%%
%% "An external challenge authenticated user logged in."
-define( LOGSYM_EXT_AUTH_2FA_SUCCESS, 195).
%% 
%%
%% "External challenge authentication failed for a user."
-define( LOGSYM_EXT_AUTH_2FA_FAIL, 196).
%% 
%%
%% "External challenge sent to a user."
-define( LOGSYM_EXT_AUTH_2FA, 197).
%% 
%%
%% "A change to NCS configuration has taken place, e.g., by a reload of the configuration file"
-define( LOGSYM_CONFIG_CHANGE, 198).
%% 
%%
%%    "Duplicate namespace found."
-define( LOGSYM_DUPLICATE_NAMESPACE, 199).
%% 
%%
%% "An error occurred while evaluating a when-expression."
-define( LOGSYM_EXEC_WHEN_CIRCULAR_DEPENDENCY, 200).
%% 
%%
%% "Failed to delete rollback file."
-define( LOGSYM_ROLLBACK_FAIL_DELETE, 201).
