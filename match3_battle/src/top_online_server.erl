%%%-------------------------------------------------------------------
%%% File    : top_online_server.erl
%%% Author  : Mihail Bogatyrev <ksupipr@yandex.ru>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  16 Feb 2012 by Mihail Bogatyrev <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(top_online_server).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").

%% API
-export([start_link/0, get_session_list/0, get_session_list/1, get_top/0,  send_top/2, set_redis/1, set_redis2/2, set_SJS/3, dell_SJS/3, dell_all/4, change_flag/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
 send_flag=1 %% отправлять ли топ? т.е. изменилось ли что-то
, top_100 = []
, online_num = 0
, stop_timer={0,0,0} %% список идентификаторов таймеров которые надо завершить при умирании
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================


get_session_list() ->
    gen_server:cast(?SERVER, {get_session_list})
.
    
get_session_list(_) ->
    gen_server:cast(?SERVER, {get_session_list}).

get_top() ->
    gen_server:cast(?SERVER, {get_top}).


change_flag() ->
    gen_server:cast(?SERVER, {change_flag}).


set_redis(Jid) ->
    gen_server:cast(?SERVER, {set_redis, Jid}).

set_redis2(Jid, Info) ->
    gen_server:cast(?SERVER, {set_redis2, Jid, Info}).

set_SJS(Score, Jid, Sort)  ->
    gen_server:cast(?SERVER, {set_SJS, Score, Jid, Sort}).

dell_SJS(Score, Jid, Sort)  ->
    gen_server:cast(?SERVER, {dell_SJS, Score, Jid, Sort}).

dell_all(Score, Jid, Sort, Res)  ->
    gen_server:cast(?SERVER, {dell_all, Score, Jid, Sort, Res}).

send_top(Jid, Sort) ->
    gen_server:cast(?SERVER, {send_top, Jid, Sort}).





%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->


    State = #state{},
    get_session_list(),

    Self = self(),

    {ok, TRef} = libs_functions:wrap_cron_once(30, {top_online_server, get_top, []}),

    gen_server:cast(Self, {make_top_100}),

    {ok, State#state{stop_timer={TRef,0,0}}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%%handle_call({send_top, Jid, Sort}, _From, State) ->
    %%?INFO_MSG("send_top: Jid: ~p; Sort: ~p", [Jid, Sort]),
%%    send_all({0, Jid, Sort},  State),
%%    Reply = Sort,
%%    State1 = State#state{send_flag = 1},
    %%{reply, Reply, State1};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({set_SJS, Score, Jid, Sort}, State) ->
    State1 = State#state{send_flag = 1},
    set_SJS({Score, Jid, Sort}, State1),
    {noreply, State1};


handle_cast({dell_SJS, Score, Jid, Sort}, State) ->
    State1 = State#state{send_flag = 1},
    dell_SJS({Score, Jid, Sort}, State1),
    {noreply, State1};

handle_cast({dell_all, Score, Jid, Sort, Rez}, State) ->
    State1 = State#state{send_flag = 1},
    dell_all({Score, Jid, Sort, Rez}, State1),
    {noreply, State1};


handle_cast({change_flag}, State) ->
    State1 = State#state{send_flag = 1},
    {noreply, State1};

handle_cast({set_redis, Jid}, State) ->
    State1 = State#state{send_flag = 1},
    user_info_list(jlib:string_to_jid(Jid), State1),
    {noreply, State1};


handle_cast({set_redis, Jid, Info}, State) ->
    State1 = State#state{send_flag = 1},
    user_info_list2(jlib:string_to_jid(Jid), Info, State1),
    {noreply, State1};

handle_cast({send_top, Jid, Sort}, State) ->
    %%?INFO_MSG("send_top: Jid: ~p; Sort: ~p", [Jid, Sort]),
    send_all({0, Jid, Sort},  State),
    State1 = State#state{send_flag = 1},
    {noreply, State1};


handle_cast({get_session_list}, State) ->
    %% получаем всех кто онлайн есть
    UserList = ejabberd_sm:dirty_get_sessions_list()

    , Length_list = length(UserList)

    %%, ?INFO_MSG("get_session_list: ~p;", [UserList])

    , Redis_link = plg_redis_wrapper:get_link_redis(?EREDIS_INF)


    , Sn_prefixs = ?SN_PREFIXS


    , Dell_top = fun(Pref) ->    
        plg_redis_wrapper:q(Redis_link, ["DEL", list_to_binary([Pref, <<"_online_users">>])])
        , plg_redis_wrapper:q(Redis_link, ["DEL", list_to_binary([Pref, <<"_online_users_sort">>])])
        , plg_redis_wrapper:q(Redis_link, ["DEL", list_to_binary([Pref, <<"_online_users_info">>])])
    end


    , [ Dell_top(Pref) || Pref <- Sn_prefixs]
%%    , UserFullInfoList = [ user_info_list(jlib:make_jid(Jid), State) || Jid <- UserList]


    , UserFullInfoList = [ user_info_list(jlib:make_jid(Jid), State) || Jid <- UserList]


   
 %%   , UserFullInfoListSort = lists:sort(fun({A1, _, _, _}, {B1, _, _, _}) -> A1<B1 end, UserFullInfoList)


%%    , P8 = fun(A, AccIn) -> 
%%		 %% анонимная функция для расстановки запятых между обьектами
%%         {_S,_J, A1, _Sort} = A,
%%		 B =  unicode:characters_to_binary(","), 
%%			  case AccIn of [] ->  [ A1 | AccIn ];
%%			      _ ->   M = [B | AccIn],[A1 | M ] 
%%			  end 
%%	 end

    %% ставим запятые
%%    , UserInfoList = lists:foldl(P8,[] , UserFullInfoListSort)

    %% собираем json
%%    , LLR1 = <<"{\"reply\": {\"type\":2, \"body\": {\"user_list\":{[">>
%%    , LLR2 = <<"]}} }}">>
%%    , LoginListResult = lists:flatten([LLR1,UserInfoList,LLR2])


    %% отправляем всем в списке
%%    , From = "io@echo.localhost"
%%    , [send_all(Jid, State) || {_, Jid, _, _} <- UserFullInfoListSort]
    , [send_all({Score, Jid, Sort}, State) || {Score, Jid, _, Sort} <- UserFullInfoList]

    , {T1, _T2, T3} = State#state.stop_timer
    , {ok, TPid} = libs_functions:wrap_cron_once(300, {top_online_server, get_session_list, []})

    , State1 = State#state{send_flag = 0, online_num = Length_list, stop_timer={T1,TPid,T3}}
    , {noreply, State1};


handle_cast({get_top}, State) ->
    Send_flag = State#state.send_flag, 

case Send_flag of

  1->

    Redis_link = plg_redis_wrapper:get_link_redis(?EREDIS_INF)
    , Sn_prefixs = ?SN_PREFIXS




    , Send_top = fun (Pref) ->
        {ok, RedisList} = plg_redis_wrapper:q(Redis_link, ["ZRANGE", list_to_binary([Pref, <<"_online_users_sort">>]), 0, -1]),

        Send_from_redis = fun(Inf) ->
            Inf1 = binary_to_term(Inf),
            {Score, Jid, Sort} = Inf1,
            send_all({binary_to_list(Score), binary_to_list(Jid), lists:sum(binary_to_list(Sort))}, State)
        end,

        [ Send_from_redis(Inf) || Inf  <- RedisList]
    end

    , [ Send_top(Pref) || Pref <- Sn_prefixs]
    
    , State1 = State#state{send_flag = 0};
  
    _ ->   State1 = State
end

, {_T1, T2, T3} = State1#state.stop_timer
, {ok, TPid} = libs_functions:wrap_cron_once(30, {top_online_server, get_top, []})
, State2 = State1#state{stop_timer = {TPid, T2, T3}}

    , {noreply, State2};
 

handle_cast({make_top_100}, State) ->
%% меняет топ 100 в статусе, чтобы отдавать всем 

    Sn_prefixs = ?SN_PREFIXS

    , Make_new_top = fun(Pref) ->
           {Pref, make_top_100(Pref, State)}
    end
    , New_top = [  Make_new_top(Pref) || Pref <- Sn_prefixs]
    %%, ?INFO_MSG("New_top: ~p;", [New_top])

    , {T1, T2, _T3} = State#state.stop_timer
    , {ok, TPid} = libs_functions:wrap_cron_once(300, {gen_server, cast, [self(), {make_top_100}]})

    , State_new = State#state{top_100 = New_top, stop_timer = {T1, T2, TPid}}

    
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
terminate(Reason, State=#state{stop_timer=MFJob}) ->
    ?INFO_MSG("Top_online_server has TERMINATE!! Reason: ~p ~n State: ~p;", [Reason, State]),
    {T1, T2, T3} = MFJob,
    libs_functions:wrap_cron_cancel(T1),
    libs_functions:wrap_cron_cancel(T2),
    libs_functions:wrap_cron_cancel(T3),
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


user_info_list(Jid, State) ->

  %%  Link = plg_redis_wrapper:get_link_redis(?EREDIS_PROFILE_SERV),
    JidStr= jlib:jid_to_string(Jid),
    Pid4Jid=gproc:lookup_local_name(JidStr),
    %Pref = get_sn_prefix(JidStr), 

    case (libs_functions:alive(Pid4Jid)) of
        true  ->
        % отправить запрос туда
            UserInfo = gen_server:call(Pid4Jid, {top_state_user});
        _ ->
        %запустить нужный процесс
            %{ok,Pid4Jid2} = match_profile:start_link(JidStr,Link),
            %UserInfo = gen_server:call(Pid4Jid2, {top_state_user})
            UserInfo = []
    end,

    user_info_list2(Jid, UserInfo, State)
.

user_info_list2(Jid, UserInfo, _State ) ->
    
    JidStr= jlib:jid_to_string(Jid),
 
    case UserInfo of
        [] -> {0, [], [], 0};
        _  ->
                {Score, _Jid, Result, Sort, _Frends} = UserInfo,

                UserInfoOut = {Score, _Jid, Result, Sort}, 

                Pref = get_sn_prefix(JidStr)

                   , Redis_link = plg_redis_wrapper:get_link_redis(?EREDIS_INF)
                , plg_redis_wrapper:q(Redis_link, ["ZADD", list_to_binary([Pref, <<"_online_users">>]), Score, list_to_binary(JidStr)]),
                plg_redis_wrapper:q(Redis_link, ["ZADD", list_to_binary([Pref, <<"_online_users_sort">>]), Score, term_to_binary({<<Score>>, list_to_binary(JidStr), <<Sort>>})]),
                plg_redis_wrapper:q(Redis_link, ["ZADD", list_to_binary([Pref, <<"_online_users_info">>]), Score, Result]),

    UserInfoOut
    end
.

set_SJS(Params, _State) ->

     Redis_link = plg_redis_wrapper:get_link_redis(?EREDIS_INF)

    ,{Score, JidStr, Sort} = Params
    ,Pref = get_sn_prefix(JidStr) 
    ,plg_redis_wrapper:q(Redis_link, ["ZADD", list_to_binary([Pref, <<"_online_users_sort">>]), Score, term_to_binary({<<Score>>, list_to_binary(JidStr), <<Sort>>})])
.

dell_SJS(Params, _State) ->

    Redis_link = plg_redis_wrapper:get_link_redis(?EREDIS_INF)

    ,{Score, JidStr, Sort} = Params
    ,Pref = get_sn_prefix(JidStr) 
    ,plg_redis_wrapper:q(Redis_link, ["ZREM", list_to_binary([Pref, <<"_online_users_sort">>]), term_to_binary({<<Score>>, list_to_binary(JidStr), <<Sort>>})])
.

dell_all(Params, _State) ->

    Redis_link = plg_redis_wrapper:get_link_redis(?EREDIS_INF)

    ,{Score, JidStr, Sort, Rez} = Params
   , Pref = get_sn_prefix(JidStr)
    
    ,plg_redis_wrapper:q(Redis_link, ["ZREM", list_to_binary([Pref, <<"_online_users">>]), list_to_binary(JidStr)])
    ,plg_redis_wrapper:q(Redis_link, ["ZREM", list_to_binary([Pref, <<"_online_users_sort">>]), term_to_binary({<<Score>>, list_to_binary(JidStr), <<Sort>>})])
    ,plg_redis_wrapper:q(Redis_link, ["ZREM", list_to_binary([Pref, <<"_online_users_info">>]), Rez])
.


send_all(Params,  State1) -> 

    Length_list = ejabberd_sm:connected_users_number(),
    
    State = State1#state{online_num = Length_list},

%%?INFO_MSG("send_all Params: ~p;", [Params]),

    {_Score, To, Sort} = Params
    
    , case (Sort) of
          1 -> top_100(To, State), ok;
          2 -> top_49(To, State), ok;
          3 -> top_friends(To, State), ok;
          _ -> ok
      end
.


top_49(To, State) ->

    Redis_link = plg_redis_wrapper:get_link_redis(?EREDIS_INF)
    , Pref = get_sn_prefix(To)
    , Zrank = plg_redis_wrapper:q(Redis_link, ["ZRANK", list_to_binary([Pref, <<"_online_users">>]), list_to_binary(To)])
    , case  Zrank of 
        {ok, _RLP} -> {ok, RedisListPosition} = Zrank;
                 _ -> RedisListPosition = undefined
      end

    , case (RedisListPosition) of
        undefined -> RedisListPositionNow = <<"0">>;
        _ -> RedisListPositionNow = RedisListPosition
    end
    

    , Minus_pos = fun(Pos) -> Pos_new = Pos-15, if Pos_new > 0 -> Pos_new; true -> 0 end end

    , GetOt = Minus_pos(list_to_integer(binary_to_list(RedisListPositionNow)))


%%    , GetOt = lists:sum(binary_to_list(RedisListPosition))
    , GetDo = GetOt+30

    , Zrange = plg_redis_wrapper:q(Redis_link, ["ZRANGE", list_to_binary([Pref, <<"_online_users_info">>]), GetOt, GetDo])

    , {ok, RedisList} = Zrange

    , P8 = fun(A, AccIn) -> 
		 %% анонимная функция для расстановки запятых между обьектами
         A1 = A,
		 B =  unicode:characters_to_binary(","), 
			  case AccIn of [] ->  [ A1 | AccIn ];
			      _ ->   M = [B | AccIn],[A1 | M ] 
			  end 
	 end

    %% ставим запятые
    , UserInfoList = lists:foldl(P8,[] , RedisList)

    %%, LLR1 = <<"{\"reply\": {\"type\":2, \"body\": {\"user_list\":{[">>
    , Online_num = State#state.online_num
        , LLR1 = <<"{\"reply\": {\"type\":2, \"body\": { \"user_list\":{\"sort\":2, ">>
    , LLR1_1 = <<"\"online_num\":">>
    , LLR1_2 = <<", \"list\":[">>
    , LLR2 = <<"]}} }}">>
    , LoginListResult = list_to_binary([LLR1, LLR1_1, integer_to_list(Online_num), LLR1_2, UserInfoList, LLR2])
 
%%    , ?INFO_MSG("send_message top49: ~p;", [To])
    , send_message(To,  LoginListResult)
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
    , UserInfoList = lists:foldl(Get_top_pref,[] , Top_100)

    , Online_num = State#state.online_num
    , LLR1 = <<"{\"reply\": {\"type\":2, \"body\": { \"user_list\":{\"sort\":1, ">>
    , LLR1_1 = <<"\"online_num\":">>
    , LLR1_2 = <<", \"list\":[">>
    , LLR2 = <<"]}} }}">>
    , LoginListResult = list_to_binary([LLR1, LLR1_1, integer_to_list(Online_num), LLR1_2, UserInfoList, LLR2])

    %%, ?INFO_MSG("LoginListResult_st: ~p;", [LoginListResult])

    , send_message(To,  LoginListResult)
.


make_top_100(Pref, _State) ->
    
    Redis_link = plg_redis_wrapper:get_link_redis(?EREDIS_INF)


    , GetOt = -50
    , GetDo = -1

    , {ok, RedisList} = plg_redis_wrapper:q(Redis_link, ["ZRANGE", list_to_binary([Pref, <<"_online_users_info">>]), GetOt, GetDo])

    , P8 = fun(A, AccIn) -> 
         %% анонимная функция для расстановки запятых между обьектами
         A1 = A,
         B =  unicode:characters_to_binary(","), 
              case AccIn of [] ->  [ A1 | AccIn ];
                  _ ->   M = [B | AccIn],[A1 | M ] 
              end 
     end

    %% ставим запятые
    , UserInfoList = lists:foldl(P8,[] , RedisList)


    , UserInfoList
.

top_friends(To, State) ->
    %%JidStr= jlib:jid_to_string(Jid),
    Pid4Jid=gproc:lookup_local_name(To)


    , case (libs_functions:alive(Pid4Jid)) of
        %% отправить запрос туда
        false -> Friends_list = [];
            _ -> Friends_list = gen_server:call(Pid4Jid, {get_friends_list})
    end

    , Online_num = State#state.online_num
        , LLR1 = <<"{\"reply\": {\"type\":2, \"body\": { \"user_list\":{\"sort\":3, ">>
    , LLR1_1 = <<"\"online_num\":">>
    , LLR1_2 = <<", \"list\":[">>
    , LLR2 = <<"]}} }}">>
    , LoginListResult = list_to_binary([LLR1, LLR1_1, integer_to_list(Online_num), LLR1_2, Friends_list, LLR2])
 

    , send_message(To,  LoginListResult)
.

get_sn_prefix(Jid) ->
    [Pref1, Pref2|_T]= Jid,
    Pref = list_to_binary([Pref1, Pref2]),
    Pref.

send_message(To,  Text) ->
send_msg( To,  Text).


%%--------------------------------------------------------------------
%% @doc функция отправки сообщения клиенту 
%% @spec send_msg( To,  Text) -> ok.
%% @end
%%--------------------------------------------------------------------
send_msg( To,  Text) ->
p_sender:send_msg(To,  Text).


