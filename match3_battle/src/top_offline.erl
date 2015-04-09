%%%-------------------------------------------------------------------
%%% File    : top_offline.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Рейтинг пользователей вне зависимости от того онлайн они или нет.
%%%
%%% Created :  03 Apr 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(top_offline).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").

%% API
-export([start_link/0, get_top/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
redis_link %% линк редиса
, top_100 = [] %% json список первой 100 в рейтинге
, stop_timer=[] %% таймеры которые надо остановить при завершении процесса
}).

-define(SERVER, ?MODULE).



%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    Sl = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ?INFO_MSG("top_offline start: ~p; ", [Sl]),
    Sl
.


get_top(Type, User) ->
    gen_server:cast(?SERVER, {get_top, Type, User}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Any) ->
    Name = <<"top_offline">>,
    gproc:add_local_name(Name),

    C = plg_redis_wrapper:get_link_redis(?EREDIS_INF),
    State = #state{redis_link=C},
    gen_server:cast(self(), {make_top_100}),

    Job = {{daily, {12, 20, am}}, {gen_server, cast, [self(), {make_top_100}]}},
   Rt =  erlcron:cron(Job),

   {ok, State#state{ stop_timer=[Rt]}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------


handle_cast({get_top, Type, User}, State) ->
%% топы

    case (Type) of
        1 -> top_100(jlib:jid_to_string(User), State), ok;
        2 -> top_49(jlib:jid_to_string(User), State), ok;
        3 -> top_friends(jlib:jid_to_string(User), State), ok;
        _ -> ok
    end,
    

   {noreply, State};

handle_cast({make_top_100}, State) ->
%% меняет топ 100 в статусе, чтобы отдавать всем 

    Sn_prefixs = ?SN_PREFIXS

    , Make_new_top = fun(Pref) ->
           {Pref, make_top_100(Pref, State)}
    end
    , New_top = [  Make_new_top(Pref) || Pref <- Sn_prefixs]
    %%, ?INFO_MSG("New_top: ~p;", [New_top])

    , State_new = State#state{top_100 = New_top}
    , {noreply, State_new};



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
terminate(_Reason, _State=#state{stop_timer=MFJob}) ->
[erlcron:cancel(R) || R <- MFJob],
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

make_top_100(Pref, State) ->
    
    Redis_link = State#state.redis_link


    , GetOt = -100
    , GetDo = -1

    , {ok, RedisList} = plg_redis_wrapper:q(Redis_link, ["ZRANGE", list_to_binary([Pref, <<"allusers">>]), GetOt, GetDo])

    , P8 = fun(A, AccIn) -> 
         %% анонимная функция для расстановки запятых между обьектами
         A1 = get_user_info(Redis_link, A),
         %%A1 = A,
         B =  unicode:characters_to_binary(","), 
              case AccIn of 
                  [] when A1 =/= [] ->  [ A1 | AccIn ];
                  _ when A1 == []  -> AccIn;
                  _ ->   M = [B | AccIn], [A1 | M ] 
              end 
     end

    %% ставим запятые
    , UserInfoList = lists:foldl(P8,[] , RedisList)

    %%, LLR1 = <<"{\"reply\": {\"type\":2, \"body\": {\"user_list\":{[">>
    , LLR1 = <<"{\"reply\": {\"type\":50, \"body\": {\"reit_list\":{\"sort\":1,\"list\":[">>
    , LLR2 = <<"]}} }}">>
    , LoginListResult = list_to_binary([LLR1, UserInfoList, LLR2])

    , LoginListResult
.

top_100(To, State) ->
    
    Pref = get_sn_prefix(To)

    , Get_top_pref = fun(A, AccIn) -> 
         %% анонимная функция для расстановки запятых между обьектами
         {A1, Top_out} = A,
         
              case A1 of 
                    Pref ->   Top_out;
                  _ ->   AccIn
              end 
     end

    %% ставим запятые
                

    , Top_100 = State#state.top_100
    %%, LoginListResult_st = lists:filter(Get_top_pref, Top_100)
    , LoginListResult = lists:foldl(Get_top_pref,[] , Top_100)
    %%, ?INFO_MSG("LoginListResult_st: ~p;", [LoginListResult])

    , send_message(To,  LoginListResult)
.


top_49(To, State) ->

    Redis_link = State#state.redis_link

    , Pref = get_sn_prefix(To)

    , Zrank = plg_redis_wrapper:q(Redis_link, ["ZRANK", list_to_binary([Pref, <<"allusers">>]), list_to_binary(To)])


    , {ok, RedisListPosition} = Zrank

    , case (RedisListPosition) of
        undefined -> RedisListPositionNow = <<0>>;
        _ -> RedisListPositionNow = RedisListPosition
    end
    

    , Minus_pos = fun(Pos) -> Pos_new = Pos-49, if Pos_new > 0 -> Pos_new; true -> 0 end end

    , GetOt = Minus_pos(list_to_integer(binary_to_list(RedisListPositionNow)))


%%    , GetOt = lists:sum(binary_to_list(RedisListPosition))
    , GetDo = GetOt+100

    



    , Zrange = plg_redis_wrapper:q(Redis_link, ["ZRANGE", list_to_binary([Pref, <<"allusers">>]), GetOt, GetDo])


    , {ok, RedisList} = Zrange

    , P8 = fun(A, AccIn) -> 
         %% анонимная функция для расстановки запятых между обьектами
         A1 = get_user_info(Redis_link, A),
         %%A1 = A,
         B =  unicode:characters_to_binary(","), 
              case AccIn of 
                 [] when A1 =/= [] ->  [ A1 | AccIn ];
                  _ when A1 == []  ->  AccIn;
                  _ ->   M = [B | AccIn],[A1 | M ] 
              end 
     end

    %% ставим запятые
    , UserInfoList = lists:foldl(P8,[] , RedisList)

    %%, LLR1 = <<"{\"reply\": {\"type\":2, \"body\": {\"user_list\":{[">>
    , LLR1 = <<"{\"reply\": {\"type\":50, \"body\": {\"reit_list\":{\"sort\":2,\"list\":[">>
    , LLR2 = <<"]}} }}">>
    , LoginListResult = list_to_binary([LLR1, UserInfoList, LLR2])
 
%%    , ?INFO_MSG("send_message top49: ~p;", [To])
    , send_message(To,  LoginListResult)
.



top_friends(To, State) ->

    _Redis_link = State#state.redis_link

    %, Pref = get_sn_prefix(To)
    , Pid4Jid=gproc:lookup_local_name(To)


    , case (libs_functions:alive(Pid4Jid)) of
        false -> Friends_list = [];
            _ -> Friends_list = gen_server:call(Pid4Jid, {get_friends_list_offline})
      end

    , LLR1 = <<"{\"reply\": {\"type\":50, \"body\": {\"reit_list\":{\"sort\":3,\"list\":[">>
    , LLR2 = <<"]}} }}">>
    , LoginListResult = list_to_binary([LLR1, Friends_list, LLR2])

    , send_message(To,  LoginListResult)
.



get_user_info(C, Jid) ->
    {ok, UInfo} = plg_redis_wrapper:q(C, ["GET", list_to_binary([Jid, <<"info">>])]),
    case (UInfo) of
        undefined -> [];
        B when is_binary(B) -> {_Score, UInfo_out} = binary_to_term(UInfo), UInfo_out;
        _ -> []
    end
.

send_message(To,  Text) ->
send_msg( To,  Text).

%%--------------------------------------------------------------------
%% @doc функция отправки сообщения клиенту 
%% @spec send_msg( To,  Text) -> ok.
%% @end
%%--------------------------------------------------------------------
send_msg( To,  Text) ->
p_sender:send_msg(To,  Text).



%%--------------------------------------------------------------------
%% @doc функция парсинага Jid и определения префикса
%% @spec   get_sn_prefix(Jiv) -> Pref.
%% @end
%%--------------------------------------------------------------------
get_sn_prefix(Jid) ->
    [Pref1, Pref2|_T]= Jid,
    Pref = list_to_binary([Pref1, Pref2]),
    Pref.
