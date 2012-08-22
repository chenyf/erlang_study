-module(mc_conn).

-include("mc_constants.hrl").


-export([loop/2]).
-export([respond/4]).

bin_size(undefined) -> 0;
bin_size(List) when is_list(List) -> bin_size(list_to_binary(List));
bin_size(Binary) -> size(Binary).

xmit(Sock, undefined) -> ok;
xmit(Sock, List) when is_list(List) -> xmit(Sock, list_to_binary(List));
xmit(Sock, Binary) -> gen_tcp:send(Sock, Binary).


respond(Sock, OpCode, Opaque, Res) ->
	KeyLen = bin_size(Res#mc_response.key),
	ExtraLen = bin_size(Res#mc_response.extra),
	BodyLen = bin_size(Res#mc_response.body) + (KeyLen + ExtraLen),
	Status = Res#mc_response.status,
	CAS = Res#mc_response.cas,
	ok = gen_tcp:send(Sock, <<?RES_MAGIC,
							  OpCode:8,
							  KeyLen:16,
							  ExtraLen:8,
							  0:8,
							  Status:16,
							  BodyLen:32,
							  Opaque:32,
							  CAS:64>>),
	ok = xmit(Sock, Res#mc_response.extra),
	ok = xmit(Sock, Res#mc_response.key),
	ok = xmit(Sock, Res#mc_response.body).


read_data(Sock, 0, _For) -> <<>>;
read_data(Sock, N, For) ->
	{ok, Data} = gen_tcp:recv(Sock, N),
	Data.


process_message(Sock, StorageServer, {ok, <<?REQ_MAGIC:8,
											?STAT:8,
											KeyLen:16,
											ExtraLen:8, 0:8, 0:16,
											BodyLen:32,
											Opaque:32,
											CAS:64>>}) ->
	Extra = read_data(Sock, ExtraLen, extra),
	Key = read_data(Sock, KeyLen, key),
	Body = read_data(Sock, BodyLen-(KeyLen+ExtraLen), body),
	gen_server:cast(StorageServer, {?STAT, Extra, Key, Body, CAS, Sock, Opaque});

process_message(Sock, StorageServer, {ok, <<?REQ_MAGIC:8,
											OpCode:8,
											KeyLen:16,
											ExtraLen:8, 0:8, 0:16,
											BodyLen:32,
											Opaque:32,
											CAS:64>>}) ->
	
	Extra = read_data(Sock, ExtraLen, extra),
	Key = read_data(Sock, KeyLen, key),
	Body = read_data(Sock, BodyLen-(KeyLen+ExtraLen), body),
	Res = gen_server:call(StorageServer, {OpCode, Extra, Key, Body, CAS}),
	respond(Sock, OpCode, Opaque, Res).


loop(Sock, Handler) ->
	process_message(Sock, Handler, gen_tcp:recv(Sock, ?HEADER_LEN)),
	loop(Sock, Handler).
