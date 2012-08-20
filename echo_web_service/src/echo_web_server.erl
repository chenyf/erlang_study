%% Author: rstevens
%% Created: 2012-8-20
%% Description: TODO: Add description to echo_web_server
-module(echo_web_server).

-behaviour(gen_web_server).


%%
%% Include files
%%

%%
%% Exported Functions
%%

-export([
		 start_link/1,
		 start_link/2
		]).


-export([
		 init/1,
		 get/3,
		 delete/3,
		 put/4,
		 post/4,
		 head/3,
		 options/4,
		 trace/4,
		 other_methods/4]).

%%
%% API Functions
%%

start_link(Port) ->
	gen_web_server:start_link(?MODULE, Port, []).

start_link(IP, Port) ->
	gen_web_server:start_link(?MODULE, IP, Port, []).




init([]) ->
	{ok, []}.

get(_Request, _Head, _UserData) ->
	gen_web_server:http_reply(501).

delete(_Request, _Head, _UserData) ->
	gen_web_server:http_reply(501).

put(_Request, _Head, _Body, _UserData) ->
	gen_web_server:http_reply(501).

post(_Request, _Head, _Body, _UserData) ->
	gen_web_server:http_reply(501).

head(_Request, _Head, _UserData) ->
	gen_web_server:http_reply(501).

options(_Request, _Head, _Body, _UserData) ->
	gen_web_server:http_reply(501).

trace(_Request, _Head, _Body, _UserData) ->
	gen_web_server:http_reply(501).

other_methods(_Request, _Head, _Body, _UserData) ->
	gen_web_server:http_reply(501).

%%
%% Local Functions
%%

