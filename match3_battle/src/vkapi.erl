%%%-------------------------------------------------------------------
%%% File    : vkapi.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Функции работы API vk.com
%%%
%%% Created :  28 Apr 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(vkapi).

-include("vkapi.hrl").

-export([start_link/0, withdrawVotes/2, setUserLevel/2, sendNotification/2, sendNotificationAll/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").

-record(state, {
  
}).

-define(SERVER, ?MODULE).


start_link() ->
    Sl = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ?INFO_MSG("vkapi start: ~p; ", [Sl]),
    Sl
.




setUserLevel(Jid, Level) ->
    Name = <<"vkapi">>,
    Pid4pro = gproc:lookup_local_name(Name),
  
    
    case (libs_functions:alive(Pid4pro)) of
        false -> {ok,Pid} = start_link(),
                gen_server:cast(Pid, {setUserLevel, Jid, Level});
            _ -> gen_server:cast(Pid4pro, {setUserLevel, Jid, Level})
    end.

sendNotification(JidList, Text) ->
    Name = <<"vkapi">>,
    Pid4pro = gproc:lookup_local_name(Name),
  
    case (libs_functions:alive(Pid4pro)) of
        false -> {ok,Pid} = start_link(),
                 gen_server:cast(Pid, {sendNotification, JidList, Text}) ;
            _ -> gen_server:cast(Pid4pro, {sendNotification, JidList, Text})
    end.
    


init(_Any) ->

    Name = <<"vkapi">>,
    gproc:add_local_name(Name),
  
    State = #state{},
    {ok, State}.



handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({setUserLevel, Jid, Level}, State) ->
    setUserLevel(Jid, Level, State),
{noreply, State};

handle_cast({sendNotification, JidList, Text}, State) ->
    sendNotification(JidList, Text, State),
{noreply, State};


handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

withdrawVotes(Jid, Votes) ->
%http://api.vk.com/api.php?v=3.0&api_id=1901988&method=getProfiles&format=json&rnd=343&uids=100172&fields=photo%2Csex&sid=10180116c4fd93480439bca47d636d6dd75fac30b851d4312e82ec3523&sig=5be698cf7fa09d30f58b941a4aea0e9b

Sn_id = make_sn_id(Jid),


Current = calendar:local_time(), %% {{2012,4,16},{9,44,33}}
Now_timestamp = integer_to_list(calendar:datetime_to_gregorian_seconds(Current)),

Now_timestamp_len = string:len(Now_timestamp),

Rand = string:substr(Now_timestamp, Now_timestamp_len-4, 5),


%?INFO_MSG("~p; ~n ~p;~n ~p;~n", [Sn_id, Rand, Now_timestamp]),

%%$sig=md5($viewer_id.'api_id='.$api_id['vk'].'method='.$method.'random='.$random.'timestamp='.$timestamp.'uid='.$uids.'v='.$v.'votes='.$votes.''.$api_secret['vk']);
%SIG_text = io_lib:format("~papi_id=~pmethod=secure.withdrawVotes&format=jsonrandom=~ptimestamp=~puid=~pv=2.0votes=~p~p", [Sn_id, ?APP_ID, Rand, Now_timestamp, Sn_id, integer_to_list(Votes), ?SECRET_ID]),

SIG_text =  "api_id=" ++ ?APP_ID ++ "format=jsonmethod=secure.withdrawVotesrandom=" ++ Rand ++ "timestamp=" ++ Now_timestamp ++ "uid=" ++ Sn_id ++ "v=2.0votes=" ++ integer_to_list(Votes) ++ ?SECRET_ID,

%?INFO_MSG("SIG: ~p;", [unicode:characters_to_binary(SIG_text)]),

SIG = md5_hex(SIG_text),
%if (@$balance = file_get_contents($api_url."?api_id=".$api_id['vk']."&method=".$method."&random=".$random."&timestamp=".$timestamp."&uid=".$uids."&v=".$v."&votes=".$votes."&sig=".$sig))

%Body = io_lib:format("api_id=~p&method=secure.withdrawVotes&format=json&random=~p&timestamp=~p&uid=~p&v=2.0&votes=~p&sig=~p", [?APP_ID, Rand, Now_timestamp, Sn_id, Votes, SIG]),


Body = "api_id=" ++ ?APP_ID ++ "&format=json&method=secure.withdrawVotes&random=" ++ Rand  ++ "&timestamp=" ++ Now_timestamp ++ "&uid=" ++ Sn_id ++ "&v=2.0&votes=" ++ integer_to_list(Votes) ++ "&sig=" ++ SIG,

%?INFO_MSG("~p;", [unicode:characters_to_binary(?URL++ "?" ++ Body)]),

%Post_rez = post(?URL, "application/x-www-form-urlencoded", Body),
%Post_rez = post("http://www.google.com/", "application/x-www-form-urlencoded", "hl=en&q=erlang&btnG=Google+Search&meta="),


Post_rez = httpc:request(post, {?URL, [], "application/x-www-form-urlencoded", Body }, [], []),

%%Post_rez = httpc:request("http://www.google.com/"),

case (Post_rez) of

    {ok,{_Req, _Header, Rezult_text}} ->
    % если все ок
                {struct,  [{Json_rez, Json_str}]} = mochijson2:decode(unicode:characters_to_binary(Rezult_text)),

                case Json_rez of 
                                            <<"error">> -> 
                                                            {struct, Mess_list} = Json_str,
                                                            [EC | _T] = Mess_list,

                                                            case EC of 
                                                                {<<"error_code">>, EC_num} -> ok;
                                                                                        _ -> EC_num = 0
                                                            end,
                                                            case (EC_num) of
                                                                M when M =/= 502 -> ?INFO_MSG("Error  Jid: ~p; ~n Text: ~p;", [jlib:jid_to_string(Jid), Json_str]);
                                                                            _ -> ok
                                                            end,

                                                            Rezult_fun = {error, 1};

                                        <<"response">> -> 
                                                            Votes_add = Json_str,
                                                            Rezult_fun = {votes, Votes_add},
                                                            ?INFO_MSG("All_good votes = ~p; Jid = ~p", [Votes_add, jlib:jid_to_string(Jid)]);

                                                        _ -> Rezult_fun = {error, 2}
                end;
        _ -> %если по таймауту отвалилось или какой другой косяк
                Rezult_fun = {error, 2},
                ?INFO_MSG("Error  Jid: ~p; ~n Post_rez: ~p;", [jlib:jid_to_string(Jid), Post_rez])
end,

%Text = io_lib:format("Post_rez: ~p; ~n", [Json_str]),
%p_sender:send_msg(jlib:string_to_jid("user10@im.xlab.su"),  list_to_binary(Text)),
Rezult_fun
.


%% устанавливает уровень пользователя

setUserLevel(Jid, Level, _State) ->

Sn_id = make_sn_id(Jid),


Current = calendar:local_time(), %% {{2012,4,16},{9,44,33}}
Now_timestamp = integer_to_list(calendar:datetime_to_gregorian_seconds(Current)),

Now_timestamp_len = string:len(Now_timestamp),

Rand = string:substr(Now_timestamp, Now_timestamp_len-4, 5),

SIG_text =  "api_id=" ++ ?APP_ID ++ "format=jsonlevel="++ integer_to_list(Level) ++"method=secure.setUserLevelrandom=" ++ Rand ++ "timestamp=" ++ Now_timestamp ++ "uid=" ++ Sn_id ++ "v=2.0" ++ ?SECRET_ID,

SIG = md5_hex(SIG_text),


Body = "api_id=" ++ ?APP_ID ++ "&format=json&level="++ integer_to_list(Level) ++ "&method=secure.setUserLevel&random=" ++ Rand  ++ "&timestamp=" ++ Now_timestamp ++ "&uid=" ++ Sn_id ++ "&v=2.0&sig=" ++ SIG,


Post_rez = httpc:request(post, {?URL, [], "application/x-www-form-urlencoded", Body }, [], []),

{ok,{_Req, _Header, Rezult_text}} = Post_rez, 

{struct,  [{Json_rez, _Json_str}]} = mochijson2:decode(unicode:characters_to_binary(Rezult_text)),

%?INFO_MSG("level  Json_str= ~p;", [Json_str]),


case Json_rez of 
                             <<"error">> -> %?INFO_MSG("Error: ~p;", [Json_str]),
                                            Rezult_fun = {error, 1};

                           <<"response">> -> 
                                            %Votes_add = Json_str,
                                            Rezult_fun = {level, Level};
                                            %?INFO_MSG("All_good level = ~p;", [Level]);

                                        _ -> Rezult_fun = {error, 2}
end,

%Text = io_lib:format("Post_rez: ~p; ~n", [Json_str]),
Rezult_fun
.

%% отправка извещений всем пользователям
sendNotificationAll(Text) ->

Link = plg_redis_wrapper:get_link_redis(?EREDIS_PROFILE_SERV),

{ok, AA} = plg_redis_wrapper:q(Link, ["KEYS", "vk_*xiff"]),

SendNot = fun(A, NL) ->
    {NList, Num} = NL,
    
    if ((Num+1) == 100) ->
            % отправляем и обнуляем список
            sendNotification(lists:flatten([NList, A]), unicode:characters_to_binary(Text)),
            {[], 0};
    true -> % добавляем в список
            {lists:flatten([NList, A]), (Num+1)}
    end
            
end,

{FList, _Num} = lists:foldl(SendNot, {[], 0} , AA),

if (FList=/=[]) ->
    sendNotification(FList, unicode:characters_to_binary(Text));
true -> final_empty
end
.


%% отправка извещений пользователю 

sendNotification(JidList, Text, _State) ->

Sn_id_list = lists:flatten(libs_functions:p8_fun([make_sn_id(Jid) || Jid <- JidList])),


Current = calendar:local_time(), %% {{2012,4,16},{9,44,33}}
Now_timestamp = integer_to_list(calendar:datetime_to_gregorian_seconds(Current)),

Now_timestamp_len = string:len(Now_timestamp),

Rand = string:substr(Now_timestamp, Now_timestamp_len-4, 5),

SIG_text =  "api_id=" ++ ?APP_ID ++ "format=jsonmessage="++ binary_to_list(Text) ++"method=secure.sendNotificationrandom=" ++ Rand ++ "timestamp=" ++ Now_timestamp ++ "uids=" ++ Sn_id_list ++ "v=2.0" ++ ?SECRET_ID,

SIG = md5_hex(SIG_text),


Body = "api_id=" ++ ?APP_ID ++ "&format=json&message="++ binary_to_list(Text) ++ "&method=secure.sendNotification&random=" ++ Rand  ++ "&timestamp=" ++ Now_timestamp ++ "&uids=" ++ Sn_id_list ++ "&v=2.0&sig=" ++ SIG,


Post_rez = httpc:request(post, {?URL, [], "application/x-www-form-urlencoded", Body }, [], []),

{ok,{_Req, _Header, Rezult_text}} = Post_rez, 

{struct,  [{Json_rez, Json_str}]} = mochijson2:decode(unicode:characters_to_binary(Rezult_text)),

?INFO_MSG("Result  Json_str= ~p;", [Json_str]),


case Json_rez of 
                             <<"error">> -> %?INFO_MSG("Error: ~p;", [Json_str]),
                                            Rezult_fun = {error, 1};

                           <<"response">> -> 
                                            %Votes_add = Json_str,
                                            Rezult_fun = {response, ok};
                                            %?INFO_MSG("All_good level = ~p;", [Level]);

                                        _ -> Rezult_fun = {error, 2}
end,

%Text = io_lib:format("Post_rez: ~p; ~n", [Json_str]),
Rezult_fun
.

make_sn_id(Jid) ->

case (Jid) of
    W when is_binary(W) -> Jid_to_jid = jlib:string_to_jid(binary_to_list(W));
    M when is_list(M) -> Jid_to_jid = jlib:string_to_jid(M);
               _ -> Jid_to_jid = Jid
end,

{jid, Name_user, _P2, _P3, _P4, _P5, _P6} = Jid_to_jid,
Sn_id = string:substr(Name_user, 4),
Sn_id
.

% Функция, конвертирующая 4-битное целое в одну шестнадцатеричную цифру
hex(V) ->
    if
    V < 10 ->
            $0 + V;
    true ->
        $a + (V - 10)
    end.

% Функция, конвертирующая бинарную строку в шестнадцатеричное представление
binary_to_hex(Bin) ->
    lists:foldl(fun (E, Acc) ->
            [hex(E bsr 4) | [hex(E band 16#F) | Acc]] end,
        [],
        lists:reverse(binary_to_list(Bin))).

% md5 в hex виде
md5_hex(Bin) ->
    binary_to_hex(erlang:md5(Bin)).



%post(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).
%get(URL)                     -> request(get,  {URL, []}).
%head(URL)                    -> request(head, {URL, []}).

%request(Method, Request) ->    httpc:request(Method, Request, [], []).
