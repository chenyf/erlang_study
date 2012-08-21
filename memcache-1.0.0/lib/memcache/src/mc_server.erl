%%% -------------------------------------------------------------------
%%% Author  : rstevens
%%% Description :
%%%
%%% Created : 2012-8-19
%%% -------------------------------------------------------------------
-module(mc_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, talk/1, insert/2, get/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {count}).

-define(TABLE_ID, ?MODULE).


%% ====================================================================
%% External functions
%% ====================================================================
start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, mc_server, Args, []).


talk(Msg) ->
	gen_server:call(?MODULE, Msg, get_timeout()).
	%%gen_server:call(?MODULE, Msg).


insert(Key, Val) ->
	gen_server:call(?MODULE, {insert, Key, Val}, get_timeout()).

get(Key) ->
	gen_server:call(?MODULE, {get, Key}, get_timeout()).

delete(Key) ->
	gen_server:call(?MODULE, {delete, Key}, get_timeout()).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Args) ->
	%%process_flag(trap_exit, true},
	%%io:format("enter echosvr:init() ~n"),
	ets:new(?TABLE_ID, [public, named_table]),
    {ok, #state{count=0}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({insert, Key, Val}, From, State) ->
	ets:insert(?TABLE_ID, {Key, Val}),
	{reply, ok, State};

handle_call({get, Key}, From, State) ->
	Val = case ets:lookup(?TABLE_ID, Key) of
		[{Key, V}] -> V;
		[]           -> not_found
	end,
	{reply, Val, State};

handle_call({delete, Key}, From, State) ->
	ets:delete(?TABLE_ID, Key),
	{reply, ok, State};

handle_call(Request, From, #state{count=Count}=State) ->
    {reply, integer_to_list(Count) ++ ": " ++ Request, #state{count=Count+1}}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


get_timeout() ->
	{ok, Value} = application:get_env(memcache, timeout),
	Value.
