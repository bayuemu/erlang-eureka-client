%%%-------------------------------------------------------------------
%% @doc erl_eureka public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_eureka_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
        inets:start(),
    	case code:priv_dir(erl_eureka) of
        	{error, bad_name} ->
            		% This occurs when not running as a release; e.g., erl -pa ebin
            		% Of course, this will not work for all cases, but should account 
            		% for most
            		PrivDir = "../priv";
        	PrivDir ->
            		% In this case, we are running in a release and the VM knows
            		% where the application (and thus the priv directory) resides
            		% on the file system
            		ok
     end,
     File = filename:join([PrivDir, "erl_eureka.cfg"]),
     io:format("cfg ~p~n",[File]),
     {ok, [Conf]} = file:consult(File),
     %io:format("test ~p~n",[proplists:get_value(app_name,Conf)]),
     %io:format("cfg ~p~n",[Conf]),
     erl_eureka_sup:start_link(Conf).

%%--------------------------------------------------------------------
stop(_State) ->
    inets:stop(),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
