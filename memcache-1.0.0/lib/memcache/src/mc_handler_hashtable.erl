%%% -------------------------------------------------------------------
%%% Author  : rstevens
%%% Description :
%%%
%%% Created : 2012-8-22
%%% -------------------------------------------------------------------
-module(mc_handler_hashtable).

-behaviour(gen_server).

-include("mc_constants.hrl").


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(cached_item, 
		{flags=0, cas=0, data}).

-record(state, {cas=0, store=dict:new()}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


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
    {ok, #state{}}.

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

handle_call({?FLUSH, <<0:32>>, <<>>, <<>>, _CAS}, _From, _State) ->
	{reply, #mc_response{}, #state{}};

handle_call({?FLUSH, <<Delay:32>>, <<>>, <<>>, _CAS}, _From, State) ->
	erlang:send_after(Delay * 1000, self(), flush),
	{reply, #mc_response{}, State};


handle_call({?GET, <<>>, Key, <<>>, CAS}, From, State) ->
	case dict:find(Key, State#state.store) of
		{ok, Item} ->
			Flags = Item#cached_item.flags,
			FlagsBin = <<Flags:32>>,
			{
				reply, 
				#mc_response{extra=FlagsBin,
								cas=Item#cached_item.cas,
								body=Item#cached_item.data},
				State
			};
		_ ->
			{
				reply, 
				#mc_response{status=1, body="Does not exit"}, 
				State
			}
	end;

handle_call({?SET, <<Flags:32, Expire:32>>, Key, Val, CAS}, From, State) ->
	NewCAS = State#state.cas + 1,
	schedule_expiry(Expire, Key, NewCAS),
	{
	 reply, 
	 #mc_response{cas=NewCAS},
	 State#state{cas=NewCAS, store=dict:store(Key, 
											  #cached_item{flags=Flags,
														   cas=NewCAS,
														   data=Val},
											  State#state.store)}
	};

handle_call({_OpCode, _Header, _Key, _Body, _CAS}, _From, State) ->
	{reply,
		#mc_response{status=?UNKNOWN_COMMAND, body="Unknown command"},
		State}.



%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({?STAT, _Extra, _Key, _Body, _CAS, Sock, Opaque}, State) ->
	mc_conn:respond(Sock, ?STAT, Opaque, mk_stat("stat1", "val1")),
	mc_conn:respond(Sock, ?STAT, Opaque, mk_stat("stat2", "val2")),
	mc_conn:respond(Sock, ?STAT, Opaque, mk_stat("", "")),
	{noreply, State}.

	


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(flush, _State) ->
	{noreply, #state{}};

handle_info({delete_if, Key, Cas}, State) ->
	case dict:find(Key, State#state.store) of
		{ok, #cached_item{cas=Cas}} ->
			{noreply, State#state{store=dict:erase(Key, State#state.store)}};
		_ ->
			{noreply, State}
	end;

handle_info(X, State) ->
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

schedule_expiry(0, _Key, _Cas) -> ok;
schedule_expiry(Expire, Key, Cas) ->
	erlang:send_after(Expire*1000, self(), {delete_if, Key, Cas}).


mk_stat(K, V) ->
	#mc_response{key=K, body=V}.
