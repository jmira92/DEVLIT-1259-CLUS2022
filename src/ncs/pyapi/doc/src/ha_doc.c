/*
 * dp documentation to be included in _ha.c
 */

#define HA_MODULE_DOCSTR(PROD) \
"Low level module for connecting to " PROD " HA subsystem.\n"\
"\n"\
"This module is used to connect to the " PROD " High Availability (HA)\n"\
"subsystem. " PROD " can replicate the configuration data on several nodes\n"\
"in a cluster. The purpose of this API is to manage the HA\n"\
"functionality. The details on usage of the HA API are described in the\n"\
"chapter High availability in the User Guide.\n"\
"\n"\
"This documentation should be read together with the confd_lib_ha(3) man page."

#define DOC(name) PyDoc_STRVAR(_ha_ ## name ## __doc__,

/* ------------------------------------------------------------------------- */
DOC(connect)
/* ------------------------------------------------------------------------- */
"connect(sock, token, ip, port, pstr) -> None\n\n"

"Connect a HA socket which can be used to control a NCS HA node. The token\n"
"is a secret string that must be shared by all participants in the cluster.\n"
"There can only be one HA socket towards NCS. A new call to\n"
"ha_connect() makes NCS close the previous connection and reset the token to\n"
"the new value.\n\n"

"Keyword arguments:\n\n"

"* sock -- a Python socket instance\n"
"* token -- secret string\n"
"* ip -- the ip address if socket is AF_INET or AF_INET6 (optional)\n"
"* port -- the port if socket is AF_INET or AF_INET6 (optional)\n"
"* pstr -- a filename if socket is AF_UNIX (optional)."
);

/* ------------------------------------------------------------------------- */
DOC(bemaster)
/* ------------------------------------------------------------------------- */
"bemaster(sock, mynodeid) -> None\n\n"

"Instruct a HA node to be master and also give the node a name.\n\n"

"Keyword arguments:\n\n"

"* sock -- a previously connected HA socket\n"
"* mynodeid -- name of the node (Value or string)"
);

/* ------------------------------------------------------------------------- */
DOC(beslave)
/* ------------------------------------------------------------------------- */
"beslave(sock, mynodeid, masterid, masterip, waitreply) -> None\n\n"

"Instruct a NCS HA node to be slave with a named master. If waitreply is True\n"
"the function is synchronous and it will hang until the node has initialized\n"
"its CDB database. This may mean that the CDB database is copied in its\n"
"entirety from the master. If False, we do not wait for the reply, but it is\n"
"possible to use a notifications socket and get notified asynchronously via\n"
"a HA_INFO_BESLAVE_RESULT notification. In both cases, it is also possible\n"
"to use a notifications socket and get notified asynchronously when CDB at\n"
"the slave is initialized.\n\n"

"Keyword arguments:\n\n"

"* sock -- a previously connected HA socket\n"
"* mynodeid -- name of this slave node (Value or string)\n"
"* masterid -- name of the master node (Value or string)\n"
"* masterip -- ip address of the master node\n"
"* waitreply -- synchronous or not (bool)"
);

/* ------------------------------------------------------------------------- */
DOC(benone)
/* ------------------------------------------------------------------------- */
"benone(sock) -> None\n\n"

"Instruct a node to resume the initial state, i.e. neither master nor slave."
"\n\n"

"Keyword arguments:\n\n"

"* sock -- a previously connected HA socket"
);

/* ------------------------------------------------------------------------- */
DOC(berelay)
/* ------------------------------------------------------------------------- */
"berelay(sock) -> None\n\n"

"Instruct an established HA slave node to be a relay for other slaves.\n\n"

"Keyword arguments:\n\n"

"* sock -- a previously connected HA socket"
);

/* ------------------------------------------------------------------------- */
DOC(status)
/* ------------------------------------------------------------------------- */
"status(sock) -> None\n\n"

"Query a ConfD HA node for its status.\n\n"

"Returns a 2-tuple of the HA status of the node in the format\n"
"(State,[list_of_nodes]) where 'list_of_nodes' is the master/slave(s)\n"
"connected with node.\n\n"

"Keyword arguments:\n\n"

"* sock -- a previously connected HA socket"
);

/* ------------------------------------------------------------------------- */
DOC(slave_dead)
/* ------------------------------------------------------------------------- */
"slave_dead(sock, nodeid) -> None\n\n"

"This function must be used by the application to inform NCS HA subsystem\n"
"that another node which is possibly connected to NCS is dead.\n\n"

"Keyword arguments:\n\n"

"* sock -- a previously connected HA socket\n"
"* nodeid -- name of the node (Value or string)"
);


#undef DOC
