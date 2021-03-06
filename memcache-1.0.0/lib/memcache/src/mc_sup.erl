%%% -------------------------------------------------------------------
%%% Author  : rstevens
%%% Description :
%%%
%%% Created : 2012-8-19
%%% -------------------------------------------------------------------
-module(mc_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================



%% ====================================================================
%% Server functions
%% ====================================================================

start_link(Args) ->
	supervisor:start_link(mc_sup, Args).


%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(Args) ->
	%% io:format("mc_sup:init/1"),
    AChild = {mc_server, {mc_server,start_link,[Args]},
	      permanent,2000,worker,[mc_server]},
    {ok,{{one_for_one, 3, 10}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

