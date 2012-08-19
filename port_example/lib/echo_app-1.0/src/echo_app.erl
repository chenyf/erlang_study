%%%----------------------------------------------------------------------
%%% File    : echo_app.erl
%%% Author  : Pete Kazmier <pete-trapexit@kazmier.com>
%%% Purpose : Port Tutorial
%%% Created : Fri Jan 13 12:39:27 EST 2006
%%%----------------------------------------------------------------------

-module(echo_app).
-author('pete-trapexit@kazmier.com').

-behavior(application).

%% application callbacks
-export([start/2, 
         stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

start(_Type, _Args) ->
    PrivDir = code:priv_dir(echo_app),
    {ok, ExtProg} = application:get_env(echo_app, extprog),
	
	Path = filename:join([PrivDir, ExtProg]),
    io:format("fuck: ~s ~s ~s~n", [PrivDir, ExtProg, Path]),
	echo_sup:start_link("python " ++ Path).

stop(_State) ->
    ok.
