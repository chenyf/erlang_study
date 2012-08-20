%%% -------------------------------------------------------------------
%%% Author  : rstevens
%%% Description :
%%%
%%% Created : 2012-8-20
%%% -------------------------------------------------------------------
-module(gws_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, 
		{
		 lsock, socket, request_line, headers = [],
		 body = <<>>, content_remaining = 0,
		 callback, user_data, parent}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Callback, LSock, UserArgs) ->
	%% 此刻的self() 返回的是调用者的Pid， 即监控进程
	gen_server:start_link(?MODULE,
						  [Callback, LSock, UserArgs, self()],
						  []).


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
init([Callback, LSock, UserArgs, Parent]) ->
	%% 调用Callback的init/1，初始化工作进程
	{ok, UserData} = Callback:init(UserArgs),
	State = #state{lsock = LSock, callback = Callback, 
				   user_data = UserData,
				   parent = Parent},
	
	%% 返回的timeout为0，立刻导致产生一个 timeout消息
    {ok, State, 0}.

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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({tcp_closed, _Sock}, State) ->
	{stop, normal, State};


handle_info(timeout, #state{lsock=LSock, parent=Parent}=State) ->
	{ok, Socket} = gen_tcp:accept(LSock),
	gws_connection_sup:start_child(Parent),
	inet:setopts(Socket, [{active, once}]),
	{noreply, State#state{socket=Socket}}.

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

