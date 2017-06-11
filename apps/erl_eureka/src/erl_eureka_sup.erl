%%%-------------------------------------------------------------------
%% @doc erl_eureka top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erl_eureka_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Conf) ->
    
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Conf]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Conf]) ->
     ChildSpec = {erl_eureka_srv,{erl_eureka_srv,start_link,[Conf]},permanent,2000,worker,[erl_eureka_srv]},
    {ok, { {one_for_all, 0, 1}, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
