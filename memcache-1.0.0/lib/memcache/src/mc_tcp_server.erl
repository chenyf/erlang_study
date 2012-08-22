-module(mc_tcp_server).


-export([
		 start/1,
		 start/2,
		 start_link/1,
		 start_link/2,
		 init/2
		]).


-define(DEFAULT_PORT, 11211).


start(Handler) ->
	start(?DEFAULT_PORT, Handler).

start(Port, Handler) when is_integer(Port) ->
	{ok, spawn(?MODULE, init, [Port, Handler])}.


start_link(Handler) ->
	start_link(?DEFAULT_PORT, Handler).


start_link(Port, Handler) when is_integer(Port) ->
	{ok, spawn_link(?MODULE, init, [Port, Handler])}.


init(Port, StorageServer) ->
	{ok, LS} = gen_tcp:listen(Port, [binary,
									 {reuseaddr, true},
									 {packet, raw},
									 {active, false}]),
	accept_loop(LS, StorageServer).

accept_loop(LS, StorageServer) ->
	{ok, S} = gen_tcp:accept(LS),
	Pid = spawn(mc_conn, loop, [S, StorageServer]),
	gen_tcp:controlling_process(S, Pid),
	accept_loop(LS, StorageServer).

