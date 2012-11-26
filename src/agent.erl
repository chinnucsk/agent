%%%----------------------------------------------------------------------
%%% File    : agent.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : host and node monitor agent.
%%% Created : 27 Dec 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2010, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(agent).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-import(extbif, [appvsn/0]).

-import(erlang, [send_after/3]).

-export([start_link/0,
		host_info/0,
		stop/0]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3 ]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

host_info() ->
	gen_server:call(?MODULE, host_info).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    ?INFO("agent is started...[ok]", []),
    {ok, state}.

handle_call(host_info, _From, State) ->
    WorkerNum = length(nodes() -- [node()]),
    Nodes = string:join([atom_to_list(N) || N <- nodes()], ","),
    {ok, HostName} = inet:gethostname(),
    HostDn = list_to_binary(["host=", HostName]),
	Reply = 
    try 
        {unix, OsType} = os:type(),
        case OsType of
        'hp-ux' ->
            mon_hpux:run([{dn, HostDn}]);
        'aix' ->
            mon_aix:run([{dn, HostDn}]);
        _ ->
            mon_unix:run([{dn, HostDn}]) 
        end
    of
    {ok, HostInfo, Metrics} ->
        HostInfo1 = [{dn, HostDn},
					 {name, HostName},
					 {presence, 1},
					 {jid, node()},
					 {worker_num, WorkerNum},
					 {workers, Nodes} | HostInfo],
        {ok, HostInfo1, Metrics}
    catch
    _:Ex ->
		{error, Ex}
    end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    ?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

handle_cast(Msg, State) ->
    ?ERROR("badmsg: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}, 
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(tick, State) ->
    {noreply, State};

handle_info(Info, State) ->
    ?ERROR("badinfo: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

