%%%----------------------------------------------------------------------
%%% File    : agent_app.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : agent application
%%% Created : 13 Jan 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(agent_app).

-author('ery.lee@gmail.com').

-behavior(application).

-export([start/2, stop/1]).
    
start(normal, _Args) ->
	application:start(sasl),
    {_, OsType} = os:type(),
	start_osmon(OsType),
	agent_sup:start_link().

stop(_) ->
	ok.

start_osmon('hp-ux') ->
    ignore;
start_osmon(aix) ->
    ignore;
start_osmon(_) ->
    application:start(os_mon).
	
