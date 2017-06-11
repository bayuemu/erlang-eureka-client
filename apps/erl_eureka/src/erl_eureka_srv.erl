-module(erl_eureka_srv).
-author('lyf').


-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1,
		 register_host/1,
		 update_status_put/1,
		 update_status_delete/1,
		 heartbeat/0,
		 de_register/0,
		 app/0,
		 vip/0,
		 svip/0,
		 instance/0,
		 app_instance/0]).
-define(SERVER, ?MODULE).

-record(state,{conf,app_name ,durationInSecs,renewalIntervalInSecs,home_page_uri,status_page_uri,ipAddr ,eureka_url,data_center,vip_address,secure_vip_address,port,secure_port,use_dns,region,prefer_same_zone,eureka_domain_name,eureka_port,context,health_check_uri,eureka_urls,host_name,instance_id,app_id}).
-record(instance, {hostName, app , vipAddr="" , secureVipAddr="" ,  status, port, securePort,dataCenterInfo,healthCheckUrl=""}).

start_link(Conf) ->
	gen_server:start_link({local,?SERVER},?MODULE,[Conf],[]).

init([Conf]) ->
        io:format("erl_eureka_srv........~p~n",["Conf"]),

        App_name = proplists:get_value(app_name,Conf),
        Eureka_url = proplists:get_value(eureka_url,Conf),
        Data_center = proplists:get_value(data_center,Conf),
        IpAddr = proplists:get_value(ipAddr,Conf),
        Vip_address = string:to_lower(proplists:get_value(app_name,Conf)),
        Secure_vip_address = string:to_lower(proplists:get_value(app_name,Conf)),
        Port = proplists:get_value(port,Conf),
        Secure_port = proplists:get_value(secure_port,Conf),
        Use_dns = proplists:get_value(use_dns,Conf),
        Region = proplists:get_value(region,Conf),
        Prefer_same_zone = proplists:get_value(prefer_same_zone,Conf),
        Eureka_domain_name = proplists:get_value(eureka_domain_name,Conf),
        Eureka_port = proplists:get_value(eureka_port,Conf),
        Context = proplists:get_value(context,Conf),
        Health_check_uri = proplists:get_value(health_check_uri,Conf),
        Eureka_urls = get_eureka_urls(Eureka_url),
        
		Tmp_HostName = proplists:get_value(host_name,Conf),
		
		RenewalIntervalInSecs = proplists:get_value(renewalIntervalInSecs,Conf,30),
		DurationInSecs = proplists:get_value(durationInSecs,Conf,90),  
        Host_name = if  Data_center =:= "Amazon" , Tmp_HostName =:= "" ->
            			   selec2metadata:get("public-hostname");
        	          true ->
            			    proplists:get_value(host_name,Conf)
                      end,
        
        {ok,#state{conf=Conf,durationInSecs= DurationInSecs,renewalIntervalInSecs=RenewalIntervalInSecs ,status_page_uri = proplists:get_value(status_page_uri,Conf),home_page_uri = proplists:get_value(home_page_uri,Conf),app_name=App_name,ipAddr=IpAddr,instance_id=get_instance_id(Host_name,IpAddr,App_name,Port), eureka_url=Eureka_url,data_center=Data_center,vip_address=Vip_address,secure_vip_address=Secure_vip_address,port=Port,secure_port=Secure_port,use_dns=Use_dns,region=Region,prefer_same_zone=Prefer_same_zone,eureka_domain_name=Eureka_domain_name,eureka_port=Eureka_port,context=Context,health_check_uri=Health_check_uri,eureka_urls=Eureka_urls,host_name=Host_name},infinity}.

get_instance_id(Host_name,IpAddr,App_name,Port) ->
	       if IpAddr /= "" ->
				  IpAddr ++ ":" ++ App_name ++ ":" ++ erlang:integer_to_list(Port);
			  true ->
	              Host_name ++ ":" ++ App_name ++ ":" ++ erlang:integer_to_list(Port)
		   end.

update_status_put(S)->
	   gen_server:cast(?SERVER, {update_status_put,S}).

update_status_delete(S)->
	   gen_server:cast(?SERVER, {update_status_delete,S}).

register_host(S) ->
	   gen_server:cast(?SERVER, {register,S}).

heartbeat() ->
	   gen_server:cast(?SERVER, {heartbeat}).

app() ->
         
        gen_server:cast(?SERVER, {app}).

vip() ->
         
        gen_server:cast(?SERVER, {vip}).

svip() ->
         
        gen_server:cast(?SERVER, {svip}).

de_register() ->
	    gen_server:cast(?SERVER, {de_register}).

instance() ->
	    gen_server:cast(?SERVER, {instance}).

app_instance() ->
	    gen_server:cast(?SERVER, {app_instance}).

task_heart_beat(Url,T) ->
	receive
		after T*1000 ->
			  gen_server:cast(?SERVER, {heartbeat,Url})
	end,
	task_heart_beat(Url,T).


registerHost(Initial_status,State) ->
        
	    Port = #{ 
				 << "$" >> => erlang:integer_to_binary(State#state.port),
				 << "@enabled" >> => <<"true">> 
				},
		SecurePort = #{ 
					   << "$" >> => erlang:integer_to_binary(443),
					   << "@enabled" >> => <<"false">> 
					  },
		Metadata = #{ 
					 << "@class" >> => <<"com.netflix.appinfo.InstanceInfo$DefaultDataCenterInfo">>,
					 << "name" >> => <<"MyOwn">>
					 },
		LeaseInfo = #{ 
					 << "renewalIntervalInSecs" >> => erlang:integer_to_binary(State#state.renewalIntervalInSecs),
					 << "durationInSecs" >> => erlang:integer_to_binary(State#state.durationInSecs)
					 },
		
		HomePageUrl = if State#state.ipAddr /= "" ->
							 lists:flatten(["http://",State#state.ipAddr,":",erlang:integer_to_list(State#state.port),State#state.home_page_uri]);
						 true ->
							 lists:flatten(["http://",State#state.host_name,":",erlang:integer_to_list(State#state.port),State#state.home_page_uri])
					  end,
		StatusPageUrl = if State#state.ipAddr /= "" ->
							 lists:flatten(["http://",State#state.ipAddr,":",erlang:integer_to_list(State#state.port),State#state.status_page_uri]);
						 true ->
							 lists:flatten(["http://",State#state.host_name,":",erlang:integer_to_list(State#state.port),State#state.status_page_uri])
					  end,
		HealthCheckUrl = if State#state.ipAddr /= "" ->
							 lists:flatten(["http://",State#state.ipAddr,":",erlang:integer_to_list(State#state.port),State#state.health_check_uri]);
						 true ->
							 lists:flatten(["http://",State#state.host_name,":",erlang:integer_to_list(State#state.port),State#state.health_check_uri])
					  end,
		Instance_data = #{
							<<"instanceId">> =>  erlang:list_to_binary(State#state.instance_id),
							<<"hostName">> =>  erlang:list_to_binary(State#state.host_name),
							<<"app">> => erlang:list_to_binary(State#state.app_name),
							<<"ipAddr">> => erlang:list_to_binary(State#state.ipAddr),
							<<"vipAddr">> => erlang:list_to_binary(State#state.vip_address),
							<<"secureVipAddr">> => erlang:list_to_binary(State#state.secure_vip_address ),
							<<"securePort">> =>  SecurePort,
							<<"status">> => erlang:list_to_binary(Initial_status),
							<<"port">> => Port,
							<<"leaseInfo">> => LeaseInfo,
							<<"countryId">> => erlang:integer_to_binary(1),
							<<"dataCenterInfo">> => Metadata,
							<<"healthCheckUrl">> => erlang:list_to_binary(HealthCheckUrl),
							<<"homePageUrl">> => erlang:list_to_binary(HomePageUrl),
							<<"statusPageUrl">> => erlang:list_to_binary(StatusPageUrl)
						 },
		
		Instance = #{<<"instance">> => Instance_data },
        
		io:format("Instance ~p~n",[jsx:encode(Instance)]),
		lists:foreach( fun(Url) ->
							   case httpc:request(post, {lists:flatten([Url,State#state.context,"apps/" ++ State#state.app_name]), [{"accept", "application/json"},{"accept-encoding", "gzip"}], "application/json",jsx:encode(Instance)}, [],[]) of
					            {ok, Result} ->
					                          {{Version, Status, ReasonPhrase}, Headers, Body} = Result,
											  io:format("body ~p~n",[lists:flatten([Url,"apps/" ++ State#state.app_name])]),
											  io:format("body ~p~n",[Result]),
					                          if Status == 204 ->
													   io:format("State#state.durationInSecs ~p~n",[State#state.durationInSecs]),
												       spawn(fun() -> task_heart_beat(Url,State#state.durationInSecs) end);
					                             true ->
					                                   {error,"not register"}
					                          end;
					            {error, Reason} ->
					                          nil
					           end
							   end, State#state.eureka_urls).
      
         
get_eureka_urls(Urls) ->
	     try
	       string:tokens(Urls,",")
		 catch
			 E:R ->io:format("e:~p ~n r ~p",[E,R]),
				   []
		 end.

get_from_any_instance(Endpoint,State,Method) ->
	
	    lists:foreach( fun(Url) ->
							   get_from_any_instance(Url,Endpoint,State,Method)
					   end, State#state.eureka_urls).

get_from_any_instance(Url,Endpoint,State,Method) ->
	
							   Url2 = lists:flatten([Url,State#state.context,Endpoint]),
							   if Method =:= get ->
									  Request = {Url2, []};
								  true ->
                                      Request = {Url2, [{"accept", "application/json"},{"accept-encoding", "gzip"}], "application/json",[]}
							   end,
							  case url_request(Method,Request) of
								 {404,_Body} ->
									         register_host("UP");
								 {_,_Body} ->
									         ok;
								 _ ->
									 ok
							  end.

url_request(Method,  Request) ->
			case httpc:request(Method,  Request, [],[]) of
					            {ok, Result} ->
					                          {{Version, Status, ReasonPhrase}, Headers, Body} = Result,
											  io:format("body ~p~n",[Request]),
											  io:format("Result ~p~n",[Result]),
					                          if Status == 400 ->
					                                   {Status};
					                             true ->
					                                   {Status,Body}
					                          end;
					            {error, Reason} ->
					                          nil
			end.				   
         
terminate(_Reason,_State) ->
   ok.

handle_call(_Request,From,State) ->
    Reply = ok,
    {reply,Reply,State}.


handle_cast(Msg,State) ->
	 case Msg of
        {app} ->
               get_from_any_instance("apps/" ++ State#state.app_name,State,get);
        {vip} ->
               get_from_any_instance("vips/" ++ State#state.vip_address,State,get);
		{svip} ->
               get_from_any_instance("svips/"  ++ State#state.vip_address,State,get);
		{instance} ->
			   get_from_any_instance("instances/"  ++ State#state.instance_id,State,get);
		{app_instance} ->
			   get_from_any_instance("apps/" ++ State#state.app_name ++ "/"  ++ State#state.instance_id,State,get);
		{register,Initial_status} ->
			   registerHost(Initial_status,State);
		{update_status_put,NewState} ->
			   get_from_any_instance("apps/" ++ State#state.app_name ++ "/"  ++ State#state.instance_id ++ "/status?value=" ++ NewState ,State,put);
		{update_status_delete,NewState} ->
			   get_from_any_instance("apps/" ++ State#state.app_name ++ "/"  ++ State#state.instance_id ++ "/status?value=" ++ NewState ,State,delete);
		{heartbeat} ->
			   get_from_any_instance("apps/" ++ State#state.app_name ++ "/"  ++ State#state.instance_id,State,put);
		{heartbeat,Url} ->
			   get_from_any_instance(Url,"apps/" ++ State#state.app_name ++ "/"  ++ State#state.instance_id,State,put);
		{de_register} ->
			   get_from_any_instance("apps/" ++ State#state.app_name ++ "/"  ++ State#state.instance_id,State,delete);
        _ ->
           ok
     end,
     {noreply,State}.

handle_info(_Info,State)->
       {noreply,State}.

code_change(_OldVersion,State,_Extra) ->
       {ok,State}.

