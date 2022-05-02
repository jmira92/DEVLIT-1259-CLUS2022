%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc An Erlang interface equivalent to the HA C-API
%%% (documented in confd_lib_ha(3)).
%%% @end
%%%-------------------------------------------------------------------

-module(econfd_ha).

%%% external exports

-export([connect/2,
         connect/3,
         close/1,
         bemaster/2,
         slave_dead/2,
         beslave/4,
         benone/1,
         berelay/1,
         getstatus/1]).

-import(econfd_internal,
        [
         confd_call/2,
         term_write/2
        ]).

-include("../include/econfd.hrl").
-include("../include/econfd_errors.hrl").
-include("econfd_internal.hrl").


%% Types
-type ha_node() :: #ha_node{}.

%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%% @equiv connect(Address, 4565, Mask)
-spec connect(Address, Token) -> econfd:connect_result() when
      Address :: econfd:ip(),
      Token :: binary().
connect(Address, Token) ->
    connect(Address, ?CONFD_PORT, Token).

%% @doc Connect to the HA subsystem on host with address Address:Port.
%%
%% If the port is changed it must also be changed in confd.conf
%% To close a HA socket, use {@link close/1}.
-spec connect(Address, Port, Token) -> econfd:connect_result() when
      Address :: econfd:ip(),
      Port :: non_neg_integer(),
      Token :: binary().
connect(Address, Port, Token) ->
    case econfd_internal:connect(Address, Port, ?CLIENT_HA, []) of
        {ok, Socket} ->
            econfd_internal:bin_write(Socket, Token),
            {ok, Socket};
        Error ->
            Error
    end.

%% @doc Close the HA connection.
-spec close(Socket) -> Result when
      Socket :: econfd:socket(),
      Result :: 'ok' | {'error', econfd:error_reason()}.
close(Socket) ->
    econfd_internal:close(Socket).

%% @doc Instruct a HA node to be master in the cluster.
-spec bemaster(Socket, NodeId) -> Result when
      Socket :: econfd:socket(),
      NodeId :: econfd:value(),
      Result :: 'ok' | {'error', econfd:error_reason()}.
bemaster(Socket, MyNodeId) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_BEMASTER, MyNodeId}) of
        {ok, Reply} -> Reply;
        Err -> Err
    end.

%% @doc Instruct ConfD that another node is dead.
-spec slave_dead(Socket, NodeId) -> Result when
      Socket :: econfd:socket(),
      NodeId :: econfd:value(),
      Result :: 'ok' | {'error', econfd:error_reason()}.
slave_dead(Socket, NodeId) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_SLAVE_DEAD, NodeId}) of
        {ok, Reply} -> Reply;
        Err -> Err
    end.

%% @doc Instruct a HA node to be slave in the cluster where
%% MasterNodeId is master.
-spec beslave(Socket, NodeId, MasterNodeId, WaitReplyBool) -> Result when
      Socket :: econfd:socket(),
      NodeId :: econfd:value(),
      MasterNodeId :: ha_node(),
      WaitReplyBool :: integer(),
      Result :: 'ok' | {'error', econfd:error_reason()}.
beslave(Socket, MyNodeId, Master, WaitP) ->
    Request = {?CONFD_HA_ORDER_BESLAVE, MyNodeId,
               {Master#ha_node.nodeid, Master#ha_node.addr},
               WaitP},
    if
        WaitP == 1 ->
            case confd_call(Socket, Request) of
                {ok, Reply} -> Reply;
                Err -> Err
            end;
        WaitP == 0 ->
            term_write(Socket, Request)
    end.

%% @doc Instruct a HA node to be nothing in the cluster.
-spec benone(Socket) -> Result when
      Socket :: econfd:socket(),
      Result :: 'ok' | {'error', econfd:error_reason()}.
benone(Socket) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_BENONE}) of
        {ok, Reply} -> Reply;
        Err -> Err
    end.

%% @doc Instruct a HA slave to be a relay for other slaves.
-spec berelay(Socket) -> Result when
      Socket :: econfd:socket(),
      Result :: 'ok' | {'error', econfd:error_reason()}.
berelay(Socket) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_BERELAY}) of
        {ok, Reply} -> Reply;
        Err -> Err
    end.

%% @doc Request status from a HA node.
-spec getstatus(Socket) -> Result when
      Socket :: econfd:socket(),
      Result :: 'ok' | {'error', econfd:error_reason()}.
getstatus(Socket) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_GETSTATUS}) of
        {ok, Reply} ->
            {Status, List} = Reply,
            {ok, #ha_status{status = Status,
                            data = lists:map(fun({Nid, Addr}) ->
                                                     #ha_node{nodeid = Nid,
                                                              addr = Addr}
                                             end, List)}};
        Err -> Err
    end.

