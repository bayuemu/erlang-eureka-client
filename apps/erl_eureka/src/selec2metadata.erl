-module(selec2metadata).
-compile(export_all).
-define(METAOPTS,['ami-id', 'ami-launch-index', 'ami-manifest-path','ancestor-ami-id', 'availability-zone', 'block-device-mapping','instance-id', 'instance-type', 'local-hostname', 'local-ipv4','kernel-id', 'product-codes', 'public-hostname', 'public-ipv4','public-keys', 'ramdisk-id', 'reservation-id', 'security-groups','user-data']).

-define(ADDR,"169.254.169.254").
-define(API,"2008-02-01").

get(Metaopt) ->
         Flag = lists:member(Metaopt,?METAOPTS),
         if Flag ->
              get_info(Metaopt);
         true ->
              error
	end.

get_info('availability-zone') ->
     http_get('meta-data/placement/availability-zone');


get_info('user-data') ->
       http_get('meta-data/placement/availability-zone');

%% get_info('public-keys') ->
%%             Public_keys = [],
%%             Data = http_get('meta-data/public-keys'),
%%             if Data /= nil ->
%%                      Public_keys;
%%             true ->
%%             	Keyids = [ line.split('=')[0] for line in Data.splitlines() ]
%%             	for keyid in keyids:
%%                         Uri = io_lib:format("meta-data/public-keys/%B/openssh-key" ,[Keyid]),
%%                 
%%                 	Public_keys ++ http_get(Uri)
%%                 Public_keys
%%             end;

get_info(Metaopt) ->
       http_get("meta-data/" ++ Metaopt).
   


http_get(Uri) ->
        Url = lists:flatten(io_lib:format("http://~s/~s/~s" ,[?ADDR, ?API, Uri])),
        case httpc:request(get, {Url, []}, [], []) of
            {ok, Result} ->
                          {{Version, Status, ReasonPhrase}, Headers, Body} = Result,
                          if Status == 400 ->
                                   nil;
                             true ->
                                   Body
                          end;
            {error, Reason} ->
                          nil
         end.
