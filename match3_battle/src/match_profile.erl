%%%-------------------------------------------------------------------
%%% File    :
%%% Author  : Marat Yusupov <marat@yusupov.me> and Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Представление игрока в инфраструктуре игры
%%% Created :
%%%-------------------------------------------------------------------
-module(match_profile).
-behaviour(gen_server).

%% API
-export([start_link/1
,get_or_run_user_profile_pid/1
,recovery_user_state/2
,periodic_to_user_send/3 
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(libs_functions, [base64_encode/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").
-include("shared.hrl").
-include("shop.hrl").
-include("achiv.hrl").

-record(user, ?PLG_TOURNAMET_USER_REC ).

-record(state,  ?PLG_USER_PROFILE_REC ).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Jid) ->
%%Первый аргумент - {local, ch3} – определяет имя сервера. В нашем случае сервер
%%будет локально зарегистрирован под именем ch3.
%% Если имя будет опущено, то gen_server не будет зарегистрирован.
%% поэтому мы запускаем без имени то есть имя будет pid
  gen_server:start_link(?MODULE, [Jid], []).

%%--------------------------------------------------------------------
%% @doc функция получения живого пида процесса профиля
%% @spec get_or_run_user_profile_pid(Jid) ->pid()
%% @end
%%--------------------------------------------------------------------
get_or_run_user_profile_pid(User0) when is_list(User0) ->
Pid=gproc:lookup_local_name(User0),
Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
case (Profile_is_live(Pid)) of
        true  -> Pid;
        _ ->
        {ok,Pid_new}=match_profile:start_link(User0),
        Pid_new
  end;

get_or_run_user_profile_pid(User0) when is_tuple(User0) ->
Jid1=jlib:jid_to_string(User0),
get_or_run_user_profile_pid(Jid1).

%%--------------------------------------------------------------------
%% @doc функция для переодического вызова сообщения make_friends
%% @spec periodic_to_user_send(Interval,Pid,) ->ok
%% @end
%%--------------------------------------------------------------------
periodic_to_user_send(Interval,Pid,Param)  ->
Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
case (Profile_is_live(Pid)) of
        true  ->    gen_server:cast(Pid, Param),  
		    libs_functions:wrap_cron_once(Interval, {match_profile, periodic_to_user_send, [Interval,Pid,Param]}) ;
        _ ->
	none
  end.


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
init([Jid]) ->
%%?INFO_MSG("init : ~p~n", [Jid]),
gproc:add_local_name(Jid),
 Link = plg_redis_wrapper:get_link_redis(?EREDIS_PROFILE_SERV),

{ok, AA} = plg_redis_wrapper:q(Link, ["GET", Jid]),

Prefix_sn = get_sn_prefix(Jid),
%% надо добавить дату входа в специальную переменную 
Current       = calendar:local_time(), %% {{2012,4,16},{9,44,33}}
Now_timestamp = calendar:datetime_to_gregorian_seconds(Current),
 plg_redis_wrapper:q(Link, ["zadd", list_to_binary([<<"all_init_users">>]),integer_to_list(Now_timestamp),Jid]),

ResultState0 =case AA of
		 undefined -> plg_redis_wrapper:q(Link, ["zadd", list_to_binary([Prefix_sn, <<"allusers">>]) ,0, Jid]),  
			      Time = ?SAVE_STATE_TIME_INTERVAL
			      ,get_noob_state(Jid) ;
                 <<"saved_in_db">> -> 
                      libs_functions:wrap_cron_once(0, {top_online_server, set_redis, [Jid]}), 
		        Time = 1500,
		      plg_save_2db:restore_profile_from_db(Jid);
		 B when is_binary(B) ->
                        libs_functions:wrap_cron_once(0, {top_online_server, set_redis, [Jid]}), 
		        Time = ?SAVE_STATE_TIME_INTERVAL,
                        recovery_user_state(Link,B);
		 _ -> %% TODO какая то проблема сделать логи
		      Time = ?SAVE_STATE_TIME_INTERVAL,
                    plg_redis_wrapper:q(Link, ["zadd", list_to_binary([Prefix_sn, <<"allusers">>]),-1, Jid]),
                    get_noob_state(Jid)
                 end,

 ResultState00 = ResultState0#state {link_to_redis = Link},
 ResultState = last_init_time_check(ResultState00),
%%?INFO_MSG("step : ~p~n", [3]),
  Pid = self(),
  Msg = {save},
  erlang:send_after(Time, Pid, Msg),

%%  {ok,MFJob} = libs_functions:wrap_cron_once(20, {gen_server, cast, [Pid, {make_friends}]}),
  libs_functions:wrap_cron_once(20, {match_profile, periodic_to_user_send, [20,Pid,{make_friends}]}),

  {ok,RefT} = libs_functions:wrap_cron_once(?PROFILE_TIMEOUT, {gen_server, cast, [self(), {timeout_profile}]}),
%%?INFO_MSG("init done : ~p~n", [Jid]),
    Name = online_profiles,
    pg2:create(Name),
    pg2:join(Name, self()), 
{Mega, Seconds, _} = erlang:now(),

top_online_server:change_flag(),

JidJid = jlib:string_to_jid(Jid),

% запись в лог
{Premium, Pdate} = get_premium_state(ResultState),
    if Premium == 1 -> set_log(JidJid, 0, {lists:flatten(["У вас действует премиум-аккаунт. Вам доступны все виды бонусов в неограниченном количестве. Срок действия премиум-аккаунта – до <b>", Pdate, "</b>."])}, "#000080");
        true ->  set_log(JidJid, 0, {"Чтобы иметь все виды бонусов в неограниченном количестве, купите премиум-аккаунт. Премиум-аккаунт действует в течение недели."}, "#ff0033")
    end,


set_log(JidJid, 3, {(ResultState#state.achiv)#getted.rank}, "#000080"),

{ok, ResultState#state{timeout_ref=RefT
,timer_start_time={1, Mega * 1000000 + Seconds}
}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% использую для вызова сообщений из внутри игровых процессов сервера
%%
%%--------------------------------------------------------------------
handle_call({local_state_on_battle, 0}, _From, State=#state{transaction_state = User_stat}) ->
X2 =    case User_stat of
	2 -> X1 = State#state{route_to   = 0},change_status(rbattle_exit,X1); %% выход из поединка или из ожидания поединка только при ожидании :)
	_ -> State
		end,
{reply, ok, X2};

handle_call({local_state_on_battle, Value}, _From, State=#state{transaction_state = User_stat}) ->
%% функция изменяет состояние
%%   вроде как используется только модулем случайного боя
X1 =    case User_stat of
	0 when Value==2  -> State#state{route_to   = Value}; %% выход из поединка или из ожидания поединка только при ожидании :)
	_ -> State
		end,

Reply = ok,
{reply, Reply, X1};

handle_call({change_ref,Ref}, _From, State) ->
%% функция изменяет %% on_battle    = 3 чел в битве

X1 = State#state{
 ref_battle_max_timeout =Ref
},
{reply, ok, X1};

handle_call({begin_battle,Id_map,User1,User2,Turn,
             Bet,Score,Pid,Ref,Text,EnemyFio}, _From, State=#state{transaction_state = User_stat}) ->
%% функция изменяет состояние
%% on_battle    = 3 чел в битве

% в битву можем уходить из состояний 2,1,3
    case User_stat of
	Sa13 when Sa13>0,Sa13<4 ->   ok;
	_ ->    ?INFO_MSG("begin_battle : ~p~n", [User_stat])
    end,
    


%% Надо определить в какой тип битвы чел ушел
    Battle_type =case State#state.duel_state  of
		     M when M>0 -> 2; %% дуэль
		     _ ->
			 case State#state.tour_state of
			     B when B > 0 -> 3; %% Турнир
			     _ -> 1    end  %% поединок
		 end,


 {Premium, _Pdate} = get_premium_state(State),    
test_change_status(State,4,224),
X1 = State#state{
route_to = 3,
battle_id = Id_map,
player0 = User1,
player1 = User2, battle_serv=Pid
, premium_in_battle = Premium
, you_turn = Turn
, bet = Bet
, addScore=Score
, duel_state=0  %% терь вроде как мы в битве и на остпльные запросы нам пофик
, duel_bet=0 %% то есть так как битва знает ставку то считаем что ставка которая снята перед дуэлью автоматом идет в ставку битвы
, battle_type = Battle_type
, stat_bonus_count = 0 
, stat_turn_count  = 0
, kd_gr1 = 0 %% кд первой группы бонусов 0 кд нет
, kd_gr2 = 0 %% кд второй группы бонусов 0 кд нет
, stat_change_in_turn = 0
, ref_battle_max_timeout =Ref
, transaction_state=4
, battle_invite_msg = Text
, enemyFio = EnemyFio
},
%% чел в битве его запросы транслировать в указанную битву

Reply = ok,
{reply, Reply, ?VALID_STATE(X1)};

handle_call({change_turn_on_battle, Value, FlagNB}, _From, State = #state{you_turn = OldTurn, 
									  stat_turn_count = OldCount, stat_change_in_turn=OldC1
									 , kd_gr1 = OldGr1 %% кд первой группы бонусов 0 кд нет
									 , kd_gr2 = OldGr2 %% кд второй группы бонусов 0 кд нет
									 }) ->
%% если пришла 1 в Value значит следующий твой ход

%% функция изменяет состояние чей ход
% ?INFO_MSG(" jid ~p : ~p ~p ~n", [State#state.jid,OldGr1,OldGr2]),
% ?INFO_MSG(" jid ~p : ~p ~p ~n", [State#state.jid,OldTurn,Value]),
    {NewCount,NewC1,Nkd1,Nkd2} =case {OldTurn,Value} of %% был мой ход стал не мой 
		  {1, 0} -> msg_self_change_stat([{82,OldC1,max}]),   {OldCount+1,0,erlang:max(OldGr1-1,0),erlang:max(OldGr2-1,0)}; 
                  %% был мой  сработал таймер на ход   терь ход меняется
		  {-1, 0} -> msg_self_change_stat([{82,OldC1,max}]),   {OldCount+1,0,erlang:max(OldGr1-1,0),erlang:max(OldGr2-1,0)}; 
		  _ ->{OldCount,OldC1,OldGr1,OldGr2} %% нет перехода хода
	      end,
    
X1 = State#state{you_turn   = Value, stat_turn_count=NewCount, stat_change_in_turn=NewC1, kd_gr1=Nkd1, kd_gr2 = Nkd2 },
X2 = init_cron_timer_battle(X1,Value,FlagNB),                   %% FlagNB флаг что битва новая для длительности таймера по переходу хода
debug_send_me_state(X2#state.you_turn,X2#state.jid_bin),
% ?INFO_MSG(" change_turn_on_battle jid ~p : ~p ~p ~n", [State#state.jid,Nkd1, Nkd2]),
Reply = ok,
{reply, Reply, ?VALID_STATE(X2)};


handle_call({end_battle,Result,Battle_mode}, _From, State = #state{   ref_battle_max_timeout =Ref, premium_in_battle = Premium }) ->
%% функция изменяет состояние
%% битва кончилась надо перевести состояние профиля в обычное и добавить рейтинга и денег

%%"win_player":0 ты проиграл
%%"win_player":10 Ты пропустил 5 ходов
%%"win_player":20 Ты дезертир
%%"win_player":30 Время битвы вышло ты проиграл 
%%"win_player":1 ты победил 
%%"win_player":11  по пропускам хода
%%"win_player":21  в результате выхода
%%"win_player":31 Время битвы вышло ты выйграл

%% отменяем таймер всего боя
libs_functions:wrap_cron_cancel(Ref),
%% Тип битвы в зависимости от того в турнире мы или нет

 Btype =case State#state.tour_num>0   of
		     true-> 3;
	       _ -> 1  end,

%% получается вне зависимости от победы или проигрыша бабло начисляется в конце битвы
%% ставка при корректном заврешении битвы
Bet = case Result of
    _L when Premium == 1 -> 0; %% ставку не снимать и не прибавлять ибо премиум
    Lose  when Lose  < 0 -> Result*State#state.bet; %% снять ставку ибо премиума нет а чел проиграл
     _ -> State#state.bet div 2
      end,

Reit =case Battle_mode of
		   0 when Result==1 ->	State#state.addScore; %% победа честная рейтинг начисляется 
		   3 when Result==1 ->	State#state.addScore; %% победа честная рейтинг начисляется 
		   _ ->0
			   end,
%%?INFO_MSG(" end b : ~p ~p~n", [Battle_mode,Result]),
%% функция расчета статистики по окончании битвы
%%?INFO_MSG(" end stat : ~p~n", [LOG]),
Stat_mod =case Battle_mode of
              3 -> 0; %% битвы завершенный по таймауту считаются нормальными битвой
	      _ ->  Battle_mode
	  end,

stat_end_battle(State,Result,Stat_mod,{Bet*3,Reit}),

XA1 = set_state_at_endbattle(State,"334"),

User=get_jid_from_state(XA1),
%%?INFO_MSG("Result: ~p~n", [Result]),
sovets:get_sovet(User),

case (Result) of
        -1 ->
                Text_lose = mochijson2:encode({struct,[{<<"reply">>,{struct,[{<<"type">>,100},{<<"body">>,
                        [{struct,[{<<"end">>,
                           {struct,[
                                {<<"win_player">>,Battle_mode*10 + 0},

                                {<<"result">>, {struct,[
                                        {<<"type">>,Btype},
                                        {<<"param0">>,Bet },
                                        {<<"param1">>,0},
                                        {<<"param2">>,0}
                                        ]}}
                                ] }}
                        ]}]
                        }]}}]}),
              %%          send_msg( User, Text_lose), 

	       libs_functions:wrap_cron_once(?END_BATTLE_SLEEP_INTERVAL, {p_sender, send_msg, [User, Text_lose]}),
	     X1= win_or_lose_tournir(lose,Btype,XA1,User);

    1 -> 

	   Send_bet = Bet*3,
        Text_win = mochijson2:encode({struct,[{<<"reply">>,{struct,[{<<"type">>,100},{<<"body">>,
                        [{struct,[{<<"end">>,
                           {struct,[
                                {<<"win_player">>,Battle_mode*10+1},

                                {<<"result">>, {struct,[
                                        {<<"type">>  , Btype	},
					{<<"param0">>, Send_bet },
					{<<"param1">>, Reit	},
					{<<"param2">>, 0	}
					]}}
				] }}
			]}]
			}]}}]}),
	    %% send_msg( User, Text_win),
	libs_functions:wrap_cron_once(?END_BATTLE_SLEEP_INTERVAL, {p_sender, send_msg, [User, Text_win]}),
	     X1 = win_or_lose_tournir(win,Btype,XA1,User)
end,

X2 = add_money_0(X1, Bet,notify),
%%?INFO_MSG("change reit: ~p~n", [Reit]),
X3 = add_score(X2, Reit),

%% проверяем получение достижений по окончанию боя
get_achiv_after(1),

X5 = (stop_turn_timer(X3))#state{ref_turn_timer=0},
{reply, ok, ?VALID_STATE(X5)};

handle_call({end_battle_nt,1,Battle_mode}, _From, State=#state{	 premium_in_battle = Premium, ref_battle_max_timeout =Ref }) ->
%% Функция вызывается при убегании противника из турнира
%% противник покинул битву - мы победили
%% Mode вероятнее всего равен 5
%% отменяем таймер боя
libs_functions:wrap_cron_cancel(Ref),

%% битва кончилась надо перевести состояние профиля в обычное и добавить рейтинга и денег

%% Тип битвы в зависимости от того в турнире мы или нет
    Btype =case State#state.tour_num >0	 of
	       true -> 3;
	       _ -> 1  end,
    

Bet = case Premium of
    1 -> 0; %% ставку не снимать и не прибавлять ибо премиум
     _ -> State#state.bet div 2
      end,

Send_bet = Bet*3,

%%?INFO_MSG(" end stat : ~p~n", [2]),
stat_end_battle(State,1,Battle_mode,{Send_bet,0}),
XA1 = set_state_at_endbattle(State,"417"),
valid_state(XA1),

User=get_jid_from_state(XA1),
sovets:get_sovet(User),
Reit =State#state.addScore,
Text_win = mochijson2:encode({struct,[{<<"reply">>,{struct,[{<<"type">>,100},{<<"body">>,
			[{struct,[{<<"end">>,
			   {struct,[
				{<<"win_player">>,Battle_mode*10+1},

				{<<"result">>, {struct,[
					{<<"type">>,  Btype    },
					{<<"param0">>,Send_bet },
					{<<"param1">>,Reit     },
					{<<"param2">>,1}
					]}}
				] }}
			]}]
			}]}}]}),
libs_functions:wrap_cron_once(?END_BATTLE_SLEEP_INTERVAL, {p_sender, send_msg, [User, Text_win]}),


X1 = win_or_lose_tournir(win,Btype,XA1,User),
valid_state(X1),
X3 = add_money_0(X1, Bet ,notify),
valid_state(X3),

X4 = (stop_turn_timer(X3))#state{ref_turn_timer=0},
get_achiv_after(1),
{reply, ok, ?VALID_STATE(X4)};


handle_call({top_state_user}, _From, State) ->
%% функция получения профиля для топа из состояния


%%Score = lists:sum(binary_to_list(crypto:rand_bytes(3))),
Score = get_score(State),
Jid = State#state.jid,
Sort = State#state.top_status,
%%Sn_id_bin = State#state.sn_id,
Frends = State#state.frends,
Result= get_info(State),
{reply, {Score, Jid, Result, Sort, Frends}, State};


handle_call({top_state_user_info}, _From, State) ->
Result= get_info(State),
{reply, Result, State};


handle_call({get_user_achivs}, _From, State) ->
    Result= State#state.achiv,
%% ?INFO_MSG("{get_user_achivs} call  ~p ~n", [Result]),
{reply, Result, State};



%%--------------------------------------------------------------------
%% Function: handle_call({get_friends_list}... -> 
%% Description: Возвращает список друзей онлайн
%%-----------------------------------------------------------------------

handle_call({get_friends_list}, _From, State) ->
    Friends_list_out = frends:get_frends_online(State#state.frends_list),
{reply, Friends_list_out, State};


%%--------------------------------------------------------------------
%% Function: handle_call({get_friends_list}... -> 
%% Description: Возвращает список друзей оффлайн
%%-----------------------------------------------------------------------

handle_call({get_friends_list_offline}, _From, State) ->
    Friends_list_out = frends:get_frends(State#state.frends_list_offline),
{reply, Friends_list_out, State};


handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%					{noreply, State, Timeout} |
%%					{stop, Reason, State}
%% Description: Handling cast messages
%% {To, From} ибо нам надо откуда пришло туда отправить от имени куда пришло :))
%% то есть отправить пользователю от имени gs
%% в майлбокс запрос как бы от игрока профиля
%%--------------------------------------------------------------------
handle_cast({query_type, To_me, From, Body}, State = #state{timeout_ref=Reft}) ->
%% в майлбокс запрос как бы от игрока но это мой запрос
libs_functions:wrap_cron_cancel(Reft), %% отменяем таймер который завершает профиль при отсутсвии запросов от игрока

Parsed_body = (catch mochijson2:decode(Body)),
case Parsed_body  of

    {struct, [{<<"query">>,Value}|Other]} ->
	%% запрос валидный json

	{res,Tt,NewState}=case_query(Value, <<"query">>,State,Other),
	echo(To_me,From, Tt);
    _ -> NewState =State,
    %% TODO выдать в лог ошибок Parsed_body ибо там могут быть попытки взлома от редиски
    ?INFO_MSG("profile Pid4pro Q: ~p~n", [To_me]),
    ?INFO_MSG("profile Pid4pro Q: ~p~n", [From]),
    ?INFO_MSG("profile Pid4pro Q: ~p~n", [Body]),
    send_error( To_me, From, <<"invalid json">>,3,-1)
end,
{ok,NewRef} = libs_functions:wrap_cron_once(?PROFILE_TIMEOUT, {gen_server, cast, [self(), {timeout_profile}]}),
{noreply, ?VALID_STATE(NewState#state{timeout_ref=NewRef})};


handle_cast({top_state_user_info, ToProc}, State) ->
Result= get_info(State),

gen_server:cast(ToProc, {add_friend_info, {get_score(State), Result}}),

{noreply, State};

handle_cast({set_achiv, NewUserParams}, State) ->
 NewState =   set_achiv(State, NewUserParams),
{noreply, NewState};


handle_cast({send_achiv, User, Type}, State) ->
    case (Type) of
	3 -> AResult = user_achivs(State),
	     send_msg(User,  AResult);
	2 -> AResult = user_medals(State),
	     send_msg(User,  AResult);
	1 -> AResult = personal_data(State),
	     send_msg(User,  AResult);
	_ -> ok
    end,
{noreply, State};


handle_cast({add_money, Val1, Val2}, State) ->
%%    ?INFO_MSG("add_money 1: ~p; 2: ~p;", [Val1, Val2]),
    X1 = add_money_0(State, Val1, 1),
    X2 = add_money_1(X1, Val2, 1),
{noreply, X2};

%%--------------------------------------------------------------------
%% Function: handle_cast({add_friend_info...
%% Description: добавляет человека в список друзей
%%-----------------------------------------------------------------------

handle_cast({add_friend_info, FrInfo}, State) ->
    NewState = State#state{frends_list = frends:add_frend(State#state.frends_list, FrInfo) },
{noreply, NewState};

%%--------------------------------------------------------------------
%% Function: handle_cast({add_friend_info...
%% Description: формирует список JSON друзей
%%-----------------------------------------------------------------------

handle_cast({make_friends}, State) ->
    Frends = lists:append(State#state.frends, [State#state.jid_bin]),
    MyJid = binary_to_list(State#state.jid_bin),
    frends:make_frends_online(Frends, MyJid),
    X1 = State#state{frends_list = []},
{noreply, X1};


handle_cast({make_friends_offline}, State) ->
    Frends	 = lists:append(State#state.frends, [State#state.jid_bin]),
    MyJid	 = binary_to_list(State#state.jid_bin),
    Friends_list = frends:make_frends(Frends, MyJid),

    X1 = State#state{frends_list_offline = Friends_list},

{noreply, ?VALID_STATE(X1)};



handle_cast({add_stat,List}, State) when is_list(List) ->
%% [{Num,Value}..]
%%?INFO_MSG(" add stat : ~p ~n", [List]),
    P8 = fun(A, AccIn) -> 
		 %% анонимная функция изменения стейта (наращивание параметров статистики)
		 case A of
		     {7,Value,add}  ->
			 Ask1 = <<"0LrQsNC60L7QuSDQvdC40YLRjCDRgtC10LrRgdGCINC/0YDQviDRh9GC0L4g0LLRiyDQstGB0YLQsNC70Lgg0LIg0L7Rh9C10YDQtdC00Ywg0Lgg0LHQu9CwINCx0LvQsCA=">>,
			 User=jlib:string_to_jid(State#state.jid),
			 Result=mochijson2:encode(
				  {struct,[{<<"reply">>,
					    {struct,[{<<"type">>,1},
						     {<<"body">>,
						      {struct,[{<<"game_window">>,{struct,[{<<"player">>,{
												struct,[{<<"reit">>, get_score(State)+Value},
									{<<"msg">>,{struct,[{<<"id">>,8},{<<"comment">>,Ask1}]}}
								       ]}}]   }}]}}]}}]}),
			 send_msg(User,	 Result), 
%%			 ?INFO_MSG(" add acore : ~p ~n", [Value]),
			 S2 = change_user_stat(AccIn, 7	    , Value, add),
			 S3 = change_user_stat(S2   , 7 + 20, Value, add), %% 20 - 33
			 S4 = change_user_stat(S3   , 7 + 40, Value, add), %% 40 - 53
			      change_user_stat(S4   , 7 + 60, Value, add)  %% 60 - 73
			     ;
		     {M,Value,T} when M < 20, M >= 0 ->
			 S2 = change_user_stat(AccIn, M	    , Value,T), %% 0  - 19
			 S3 = change_user_stat(S2   , M + 20, Value,T), %% 20 - 39
			 S4 = change_user_stat(S3   , M + 40, Value,T), %% 40 - 59
			      change_user_stat(S4   , M + 60, Value,T)	%% 60 - 79
			     ;
		     {D0,Value,T} when D0 == 86 ->  %%парметр  покупки премиума
			 %% TODO  если премиум кончился в течении суток то нарастить еще и 85- Максимальное количество премиумов, 
			 %%  купленных подряд(в тот же день (сутки, до перерасчета достижений) когда кончился режим.)
			 change_user_stat(AccIn, D0, Value,T);
		     {D,Value,T} when D < ?STATISTICS_PARAM_COUNT, D >= 0 -> 
			 change_user_stat(AccIn, D, Value,T);
		     _ ->AccIn
		 end %% case
	 end, %% fun


%% изменение параметров	 статистики игрока
%% размножить изменения на остальные массивы

X1 = lists:foldl(P8, State , List),
{noreply, ?VALID_STATE(X1)};



handle_cast({timeout_profile}, State = #state{battle_serv = A0,duel_state = A1,route_to = A2,tour_state = A3, transaction_state = A4})
 when is_pid(A0) =:= true; A1+A2+A3+A4 >0 ->
%% мы в бою в турнире или в дуэли ждем пока это все кончится и завершаемся
?INFO_MSG("spets timeout : ~p~n", [State#state.jid]),
    Ref = libs_functions:wrap_cron_once(?PROFILE_TIMEOUT, {gen_server, cast, [self(), {timeout_profile}]}),
{noreply, State#state{timeout_ref=Ref}};

handle_cast({timeout_profile}, State=#state{battle_serv = A0,duel_state = A1,route_to = A2,tour_state = A3, transaction_state = A4} ) ->
%% завершаем профиль

?INFO_MSG("user timeout : ~p  << ~p >>	[ ~p ~p ~p ~p] ~n", [State#state.jid,A4, A0,A1,A2,A3]),
%%?INFO_MSG("State : ~p~n", [State#state.battle_serv]),
{stop, normal, State#state{timeout_ref=0}};

handle_cast({premium_timeout}, State = #state{thing_change_param = Thin_con}) ->
%% Премиум завершен  действие премиума обеспечивается самой подсистемой битвы
    {_Pstat, Pdate} = Thin_con#thing_change_param_rec.premium,
    New_thin_con = Thin_con#thing_change_param_rec {premium = {0,Pdate} },
    Jid = get_jid_from_state(State),
%%   ?INFO_MSG("timeout : ~p~n", [Jid]),
    set_log(Jid, 0, {"Срок действия премиум-аккаунта истек. Чтобы по-прежнему использовать все виды бонусов в неограниченном количестве, купите премиум-аккаунт снова."}, "#000080"),

LLR1 = <<"{\"reply\":{\"type\":1,\"body\":{\"game_window\":{\"player\":{\"premium\":0,\"premium_date\":\" \"}}}}}">>,
send_msg(Jid,LLR1),

{noreply, State#state{thing_change_param =New_thin_con }};

handle_cast(timeout_turn, State = #state{battle_serv = S, you_turn = N}) when is_pid(S) =:= true andalso N == 1	 ->
%% если ход игрока и мы в битве то надо устроить переход хода
 ?INFO_MSG("timeout_turn    ~p	: ~p  ~n", [State#state.jid,State#state.ref_turn_timer]),     
send_battle_serv_skip(State),
NewState=State#state{you_turn	= -1},%% блокируем прием хода от игрока
{noreply, NewState};

handle_cast({duel_timeout,Text}, State = #state{ transaction_state = 1 })   ->
%% время ответа по дуэлю вышло	20 секунд прошло
%% duel_bet=Bet,duel_state=1,duel_ref=Ref
 User = get_jid_from_state(State),
 send_msg(User,Text), %% пользователю кто вызвал в дуэль отправляем результат
 libs_functions:wrap_cron_cancel(State#state.duel_ref), %% отменяем таймер 
%%  поменять состояние
test_change_status(State,0,672),
{noreply, State#state{ transaction_state = 0, duel_ref=0 , duel_state=0,duel_bet=0 } };

handle_cast({duel_timer_stop}, State)  ->
%% противник согласился на дуэль надо остановить таймер
 libs_functions:wrap_cron_cancel(State#state.duel_ref), %% отменяем таймер 
%%  поменять состояние
{noreply, State#state{ duel_ref=0 } };

%% обработка внутреннего приглашения в дуэль 
handle_cast({duel, Enemy,_Bet,_EnemPerk}, State = #state{duel_state = S,duel_allow = M}) when S >0; M =/= 1  ->
%% мы заняты в дуэли  или запретили дуэли в настройках
  User = Enemy#user.jid, 
    Text1 =case M of
	       1 ->  make_json_duel_deny(8);
	       _ ->  make_json_duel_deny(1)
		       end,
  Pid4pro = gproc:lookup_local_name(jlib:jid_to_string(User)),
  Query = {duel_timeout,Text1},
Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
    case Profile_is_live(Pid4pro) of
	true ->	 gen_server:cast(Pid4pro, Query) ;
	_ ->	ok	end,
{noreply, State};

handle_cast({duel, Enemy,_Bet,_EnemPerk}, State = #state{ transaction_state = S } ) when S > 0	->
%% Чел в битве!!
%% или в ожидании поединка
  User = Enemy#user.jid, 
%%  Состояние профиля 1 игрок в дуэли %%  Состояние профиля 2 игрок в поединке
%%  Состояние профиля 3 игрок в турнире %%  Состояние профиля 4 игрок в битве
?INFO_MSG("duel in battle : ~p~n", [S]),
  Text1 =  
	case S of
	    1 -> make_json_duel_deny(8);
	    2 -> make_json_duel_deny(6);
	    3 -> make_json_duel_deny(7);
	    _ -> make_json_duel_deny(6) %% 4
		    end,
  Pid4pro = gproc:lookup_local_name(jlib:jid_to_string(User)),

  Query = {duel_timeout,Text1},
Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
  case Profile_is_live(Pid4pro) of
  true ->  gen_server:cast(Pid4pro, Query) ;
   _ ->	   ok	end,
 {noreply, State};


handle_cast({duel, Enemy,Bet,EnemPerk}, State = #state{duel_state = S,transaction_state = S}) when S == 0  ->
%% игрока приглашают в дуэль
 %% игрока строго могут пригласить в дль только когда он сам никого в дуэль не пригласил
 %% проверить время последнего приглашения на дуэль

{Mega, Seconds, _} = erlang:now(),
    User = get_jid_from_state(State),
    Timestamp = Mega * 1000000 + Seconds,
    Interval = ?DUEL_QUERY_INTERVAL,
    {Premium, _Pdate} = get_premium_state(State),    
    Duel_time = get_last_duel_time(State),
	TT = case {Premium,Bet =< get_money(State,0)} of
	    {0,false} ->
		  {false, (Duel_time + Interval) =< Timestamp }	   ;
	    _ -> %% есть бабло либо премиум
		  {true, (Duel_time + Interval) =< Timestamp }	  
	       end,
%% время + достаточность денег ;

X1= case TT of
	{true,true} -> %%  %% запрос на дуэль к игроку
       %%  отправляем запрос игроку
    NewUserJson_bin =unicode:characters_to_binary(plg_go_turnir:get_player_json(Enemy)),
    Bin11 = <<"{\"reply\":{\"type\":300,\"body\": {\"duel\": {	\"bet\":">>,
    Bin2 = <<", \"user\":">>,
    Bin1 = list_to_binary([Bin11, io_lib:format("~p", [Bet]),Bin2]),
    Bin3 = <<" }} }}">>,
    Text1=list_to_binary([Bin1,NewUserJson_bin,Bin3]), 
    send_msg(User,Text1),
    %%	запустить таймер дуэли
    {ok,Ref} = libs_functions:wrap_cron_once(21, {gen_server, cast, [self(), {duel_timeout,[]}]}),
    State1 = change_status(duel,State), %% ставку снимать не надо ибо потратить он не может
%%DELETE add_money_0(State, (-1)*Bet,no_send), %% сняли ставку
    State1#state{ duel_state = 2,duel_ref = Ref, duel_time=Timestamp,duel_enemy={Enemy#user.jid,EnemPerk},duel_bet=Bet}; 
    _ ->    %% я так понимаю его недавно приглашали высылаю авто отказ
	    %% отправляем отказ от дуэли игроку
	   ?INFO_MSG("duel in time : ~p~n", [S]),
	    UserA = Enemy#user.jid, 
	    Text1 =  make_json_duel_deny(2),
	    Pid4pro = gproc:lookup_local_name(jlib:jid_to_string(UserA)),
	    Query = {duel_timeout,Text1},
	    Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
	    case Profile_is_live(Pid4pro) of
		true ->	 gen_server:cast(Pid4pro, Query) ;
		_ ->	ok	end,
	      State	     
	  end,

{noreply, ?VALID_STATE(X1)};

handle_cast({set_num4tourn,Num}, State=#state{transaction_state =3}) ->
X1 = State#state{  tour_num = Num %% позиция в турнире 
, tour_state =1
 },
{noreply, X1};

handle_cast({get_bet4tourn,Bet,Num,From}, State=#state{transaction_state =D}) when D == 5;D == 3 ->
%%  Состояние профиля 5 игрок в ожидании начала турнира
%%  Состояние профиля 3 игрок в турнире

%%  перейти в режим турнира
%% взять прошлую неразыгранную ставку

   ?INFO_MSG(" Bet in tourn 1: ~p ~p~n", [State#state.tour_serv , State#state.jid]),
    X0 = change_status(enter_tournament,{State,Num,Bet,From}),

UserPerk = get_user_perk(X0,Bet),
 Jid = jlib:string_to_jid(X0#state.jid),
%% функция изменяет состояние
%% on_battle	= 2 чел в состоянии ожидания битвы
{Premium, _Pdate} = get_premium_state(X0),    

 If_term =   case Premium of
	1 -> true;
	_ ->get_money(X0,0) >= Bet
		end,
    case If_term of
	true -> %% Монеток достаточно для ставки или премиум
	    %% через 5 секунд выслать сообщение о готовности к турниру
	    %% намертво доверяем сообщению крон
	   {ok,Bet_ref} = libs_functions:wrap_cron_once(5, {gen_server, cast, [From, {bet,Num,UserPerk,Jid}]}),
	    X1= X0#state{bet=Bet, tour_ref_bet_timer=Bet_ref};
	_ -> %% выйти из режима турнира
	    X1= change_status(quit_tournament,X0),
	    gen_server:cast(From, {bet_no, Num, UserPerk,Jid}),
	    %% оповестить игрока
	     Text_lose = <<"{\"reply\":{\"type\":157,\"body\":{\"no_money\":{\"type\":\"break tournament\",\"val\":157}}}}">>,
	    send_msg(Jid, Text_lose)
    end,
{noreply, ?VALID_STATE(X1)};
handle_cast({get_bet4tourn,_Bet,Num,From}, State)  ->
%% мы не в режиме турнира 
 Jid = jlib:string_to_jid(State#state.jid),
	    gen_server:cast(From, {bet_no, Num, Num,Jid}),
{noreply, State};


%%--------------------------------------------------------------------
%% Function: handle_cast({private_message
%% Description: отправка личного сообщения
%%-----------------------------------------------------------------------

handle_cast({private_message, From, To, Text_in, Text_out}, State) ->
%if (State#state.msg_send>0) ->

    My_jid = jlib:jid_to_string(get_jid_from_state(State)),

    case (My_jid) of
    M when (M == To), (M =/= From) ->
	Text_out_my = lists:flatten(["> ", libs_functions:base64_decode(State#state.firstName), " ", libs_functions:base64_decode(State#state.lastName), ": "]),

	if (State#state.msg_send>0) ->
	    libs_functions:send_log_to_user(M, 5, {Text_in, Text_out, From}, "#9b2d30"),
	    libs_functions:send_log_to_user(From, 5, {Text_in, Text_out_my, To}, "#9b2d30");
	    true -> libs_functions:send_log_to_user(From, 5, {"Пользователь отключил личные сообщения.", Text_out_my, To}, "#9b2d30")
	end;

    W when (W == From) -> libs_functions:send_log_to_user(W, 5, {Text_in, Text_out, To}, "#9b2d30")
    end,

%true -> ok
%end,
{noreply, State};

handle_cast({add_auto_buy, Tid, Id, Count}, State) ->
 NewState =  add_auto_buy(Tid, Id, Count, State),
{noreply, NewState};

handle_cast({dell_auto_buy, Tid}, State) ->
 NewState =  dell_auto_buy(Tid, State), 
{noreply, NewState};

handle_cast({make_auto_buy, Tid}, State) ->

%?INFO_MSG("make_auto_buy tid: ~p~n", [Tid]),

%?INFO_MSG("make_auto_buy auto_buy: ~p~n", [State#state.auto_buy]),

    case (lists:keysearch(Tid, 1, State#state.auto_buy)) of
	{value, Vall} -> {_T, Th_list} = Vall,

%?INFO_MSG("make_auto_buy Vall: ~p~n", [Vall]),

			 BuyIt = fun(A, AccIn) ->
			    case A of
				    [] -> AccIn;
				     _ -> {Buy_id, Count} = A,
					  {res,_Tt,NState}=case_query(41, <<"query">>,AccIn, [{<<"th_id">>,Buy_id},{<<"count">>,Count}]),
					  NState
			    end
			 end,

			 NewState = lists:foldr(BuyIt, State, Th_list),
			 NewState2 =  dell_auto_buy(Tid, NewState);
		    _ -> NewState2 = State
    end,

{noreply, NewState2};

handle_cast(Msg, State) ->
?INFO_MSG("profile MSG: ~p~n", [Msg]),
{noreply, State}.
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%					 {noreply, State, Timeout} |
%%					 {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({statistic,	 Type}, State) ->
%%?INFO_MSG("{statistic,  Type: ~p [~p] ~n", [Type,State#state.jid]),
 get_achiv(Type, State),
{noreply, State};

handle_info({save}, State) ->
%% сохраняем профиль
%%  SaveState =set_state_at_endbattle((stop_turn_timer(State))#state{ref_turn_timer=0}),
 %% я так думаю при сохранении смысла останавливать таймер хода нет ибо игрок может быть в это время быть в бою
  SaveState =set_state_at_endbattle(State#state{ref_turn_timer=0},"save"),
  save_user_state(SaveState),
  Pid = self(),

  Time = ?SAVE_STATE_TIME_INTERVAL,
  Msg = {save},
 erlang:send_after(Time, Pid, Msg),
{noreply, State};

handle_info( {any_msg,[{Type,Dst,Body}|Tail]}, State) ->
%% то есть отправляет сообщение от имени самого себя куда следует
   %% ?INFO_MSG("any_msg : ~p ~p ~p ~n", [Type,Dst, Body]),
  Pid = self(),
    case Type of
	call ->	 gen_server:call(Dst, Body);
	cast -> gen_server:cast(Dst, Body);
	send_user -> send_msg(Dst,  Body)   ;
	 _ -> ok
		end,
 erlang:send_after(0, Pid, {any_msg,Tail}),
{noreply, State};
handle_info({any_msg,[]}, State) ->
%% список пуст больше нечиго и никуда слать не надо
{noreply, State};

handle_info(Info, State) ->
?INFO_MSG("packet received handle_info : ~p~n", [Info]),
{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State= #state{timeout_ref=RefT,route_to = S,battle_serv = Pid, jid =Jid,premium_ref=REF,stop_timer=MFJob}) when S > 0 ->
%% если мы в бою то таймаута быть не должно 
%% если мы в бою то фактически это экстремальный случай 
?INFO_MSG(" terminate in battle ~p : ~p~n", [S,Reason]),
%% сообщение о выходе отправили в сервер битв
  First_user=jlib:string_to_jid(Jid),
  gen_server:cast(Pid, {exit, State#state.battle_id,First_user, State#state.player0,State#state.player1}), 
 %% тут остановка таймера нормальна ибо мы вышли из боя
  SaveState =set_state_at_endbattle((stop_turn_timer(State))#state{ref_turn_timer=0},"term"),
libs_functions:wrap_cron_cancel(RefT),
libs_functions:wrap_cron_cancel(REF),
[erlcron:cancel(R) || R <- MFJob],
  save_user_state(SaveState),
 Name = online_profiles,
pg2:leave(Name, self()),
ok;

terminate(normal, State=#state{stop_timer=MFJob}) ->
%% по идее только таймаут 
    case MFJob of
	undefined ->ok;
	MF_jj when is_list(MF_jj) ->[erlcron:cancel(R) || R <- MFJob];
	_ -> ok
    end,
?INFO_MSG("normal terminate: ~p~n", [2]),

SaveState = State#state{route_to = 0
       , battle_id = 0
       , battle_serv=0
       , stat_change_in_turn=0
       , battle_type=0
       , ref_turn_timer=0
	},
 save_user_state(SaveState),
 Name = online_profiles,
pg2:leave(Name, self()),
   ok;

terminate(Reason, State=#state{premium_ref=REF,stop_timer=MFJob}) ->
%%?INFO_MSG("profile MSG: ~p~n", [State]),

?INFO_MSG("unnormal terminate: ~p~n", [Reason]),
    case MFJob of
	undefined ->ok;
	MF_jj when is_list(MF_jj) ->[erlcron:cancel(R) || R <- MFJob];
	_ -> ok
    end,
    

SaveState = State#state{route_to = 0
       , battle_id = 0
       , battle_serv=0
       , stat_change_in_turn=0
       , battle_type=0
       , ref_turn_timer=0
	},
libs_functions:wrap_cron_cancel(REF),
 save_user_state(SaveState),
 Name = online_profiles,
pg2:leave(Name, self()),
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

%%--------------------------------------------------------------------
%% Func: case_query(Type, Query, State,OtherTupleJson )
%% Description: function for route Msg to <Proc>
%%
%%--------------------------------------------------------------------
case_query(1, <<"query">>,State = #state{route_to   = Rbattle,	transaction_state = M, tour_serv =Pid_tour, jid =UserJid, tour_num = TNum},_Other ) ->
%%  Состояние профиля 3 игрок в турнире
%%  Состояние профиля 4 игрок в битве
	 PidMe = self(),
Re_init_turn =	 fun() -> 
	       Msg =   case Pid_tour of
		H when is_pid(H) ->  %% указанный пид турнира живой
			    case is_process_alive(H) of
				true ->
				      {cast,Pid_tour,{show,jlib:string_to_jid(UserJid)}};				    
				 _ -> {call,plg_go_turnir,{show,jlib:string_to_jid(UserJid)}}	    
					end;

		_ ->   {call,plg_go_turnir,{show,jlib:string_to_jid(UserJid)}}	    
			end,
	  lists:append([Msg],[])

	 end, %% fun

%% где и как игрок
    case M of
	Mt when Mt==3;Mt==5 -> %% чел в турнире %% чел в ожидании турнира
		erlang:send_after(1, PidMe, {any_msg, Re_init_turn()});
	4 -> %% чел в битве
	  Msg0 =  case TNum of
	       H when H > 0 -> %% чел в турнире
	       Re_init_turn();
		_ -> %% нет в турнире его
		    []	end,
	Msg1 = reinit_user_battle(State),
       erlang:send_after(1000, PidMe, {any_msg, Msg0}),
       erlang:send_after(2000, PidMe, {any_msg, Msg1})
       ; 
       2 when Rbattle==2 -> %% чел в ожидании выйти оттуда
	Name="random_battle",
	Pid4pro=gproc:lookup_local_name(Name),
	First_user=jlib:string_to_jid(State#state.jid),
	%% отправить запрос туда
	MeJID4send =  jlib:string_to_jid( ?SEND_FROM),
	gen_server:cast(Pid4pro, {deluser,First_user,MeJID4send})

       ;
	_ ->

	    []
    end,
    JidJid = jlib:string_to_jid(UserJid),
    %set_log(JidJid, 0, {lists:flatten(["Удачи в боях!"])}, "#000080"),
    sovets:get_sovet(JidJid),
% запрос правой панели
    case_query(2, <<"query">>,State, [{<<"val">>,2}] ),
% формирование списков друзей, если они таки есть, т.е. ченьджа не будет.
    if (State#state.frends =/= []) -> gen_server:cast(self(), {make_friends_offline}), gen_server:cast(self(), {make_friends});
	true -> ok
    end,


Param=State#state.user_param,
    {Pstat, Pdate} = get_premium_state(State),
    
Result=mochijson2:encode(
{struct,[{<<"reply">>,
	  {struct,[{<<"type">>,1},
		   {<<"body">>,
{struct,[{<<"game_window">>,{struct,[{<<"player">>,{struct,[
{<<"firstName">>,  State#state.firstName},
{<<"lastName">>,   State#state.lastName},
{<<"middleName">>, State#state.middleName},
{<<"id">>,	   State#state.jid_bin},
{<<"profile_link">>,   State#state.profile_link},
{<<"user_data">>,	  State#state.user_data},
{<<"rang">>,	   get_rank(State)},
{<<"money_0">>,	   get_money(State,0)},
{<<"money_1">>,	   get_money(State,1)},
{<<"premium">>,	       Pstat},
{<<"premium_date">>,   Pdate},
{<<"lrank">>,	   get_litleRank(State)},
{<<"img">>,	   get_ava(State)  },
{<<"reit">>,	   get_score(State) },
%% "razdel2":{"name":"Инвентарь","list":{
{<<"razdel2">>,{struct,[
{<<"name">>,  things:get_razdel_name (2)},
{<<"descr">>, things:get_razdel_descr(2)},
{<<"list">>,{struct,[
{<<"group0">>,{struct,[
{<<"descr">>, things:get_group_descr(2, 0)},
{<<"name">>,  things:get_group_name (2, 0)},
{<<"kd">>,  ?KD_GROUP_0},
{<<"list">>,{struct,[
get_slot(0,0,State),
get_slot(1,1,State),
get_slot(2,2,State)
 ]}}
 ]}},
{<<"group1">>,{struct,[
{<<"name">>,  things:get_group_name (2, 1)},
{<<"descr">>, things:get_group_descr(2, 1)},
{<<"kd">>,  ?KD_GROUP_1	 },
{<<"list">>,{struct,[
get_slot(3,0,State),
get_slot(4,1,State),
get_slot(5,2,State)
 ]}}
 ]}},
{<<"group2">>,{struct,[
{<<"name">>,  things:get_group_name (2, 2)},
{<<"descr">>, things:get_group_descr(2, 2)},
{<<"kd">>,  ?KD_GROUP_2},
{<<"list">>,{struct,[
%%get_slot(6,0,State),
%%get_slot(7,1,State),
%%get_slot(8,2,State)
 ]}}
 ]}}
 ]}}
]}},

{<<"panel">>,{struct,[
{<<"tx0">>,list_to_binary(io_lib:format("~p/~p", [Param#perk.soldier,Param#perk.soldier]))},
{<<"tx1">>,list_to_binary(io_lib:format("~p/~p", [Param#perk.brone,Param#perk.brone]))},
{<<"tx2">>,list_to_binary(io_lib:format("~p/~p", [Param#perk.avia,Param#perk.avia]))},
{<<"tx3">>,<<"0/220">>},
{<<"tx4">>,<<"0/100">>},
{<<"tx5">>,<<"0/160">>}]}}]}
},
{<<"rivals">>, {struct,[{<<"sort">>,3},
{<<"list">>,State#state.frends}]}
},
{<<"is_pi">>, State#state.is_admin }
]}}]}
}]}}]}),
%%  turnir_bet_list выйгрышы рассылаются
top_online_server:change_flag(),
%% TODO проанализировать M и если чел в битве отправитьнужные запросы чтоб игроку увидел карту боя
{res,Result,State};

case_query(2, <<"query">>,State, [{<<"val">>,Val}|_] ) ->
%% Топ
Val_now = State#state.top_status,
Jid = binary_to_list(State#state.jid_bin),
Score = get_score(State),

if 
    Val < 0  ->
	Val_new = Val_now;
    Val > 3  ->
	Val_new = Val_now;
    true -> Val_new = Val
end,
State_new = State#state{top_status = Val_new},
top_online_server:dell_SJS(Score, Jid, Val_now),
top_online_server:set_SJS(Score, Jid, Val_new),
top_online_server:send_top(Jid, Val_new),
{res,[],State_new};

case_query(3, <<"query">>,State, _Other ) ->
%% Настройки запрос
%%{   "reply": {
%%    "type": 3,
%%    "body": {
%%    "config": {	  "duel_bet_list":[  30,  150,	300  ],
%%    "turnir_bet_list":[ 30, 40, 50, 60 ],
%%    "curs":"1:7",
%%    "duel_access":1
%%    }	   }  }

{K1, K2} = libs_functions:get_kurs(libs_functions:get_sn_prefix(State#state.jid)),
Curs = list_to_binary(lists:flatten([integer_to_list(K1), ":", integer_to_list(K2)])),

Param=State#state.user_param,
     {res,
      mochijson2:encode(
	  {struct,[{<<"reply">>,
	  {struct,[{<<"type">>,3},
			{<<"body">>,
			{struct,[{<<"config">>,
			{struct,[
			{<<"curs">>,Curs}  %% цена одной партии кредитов равноа 7 голосам или чему то там
			, {<<"duel_bet_list">>, [100, 200 , 300] }
			, {<<"turnir_bet_list">>, [50, 100, 200, 300] } %% надо еще в стейте турнира
			, {<<"krit">>, 3}   %% коэфициент крита
			, {<<"cure">>, 40}  %%	аптечка
			, {<<"pass">>, 5 } %% на 5 пропуск хода проигрыш 
			, {<<"price_prem">>, 14 } %% цена премиума
			, {<<"id_premium">>, 3 } %%  id премиума
			, {<<"msg_on">>, State#state.msg_send}
			, {<<"trener_win">>,{struct,[
			   {<<"money0">>,10},  %%  выйгрыш в тренировке
			   {<<"reit">>,0}
			  ]}}
			, {<<"panel">>,{struct,[
			   {<<"tx0">>,list_to_binary(io_lib:format("~p/~p", [Param#perk.soldier,Param#perk.soldier]))},
			   {<<"tx1">>,list_to_binary(io_lib:format("~p/~p", [Param#perk.brone,Param#perk.brone]))},
			   {<<"tx2">>,list_to_binary(io_lib:format("~p/~p", [Param#perk.avia,Param#perk.avia]))},
			   {<<"tx3">>,<<"0/220">>},
			   {<<"tx4">>,<<"0/100">>},
			   {<<"tx5">>,<<"0/160">>}]}}

			]}
			}]}
			}]}}]}), 
       State};

case_query(10, <<"query">>,  State, [{<<"change">>,{struct,N}}|_]) ->
%% Редактировать данные о игроке

Jid = binary_to_list(State#state.jid_bin),
Score = get_score(State),
Sort = State#state.top_status,
Res= get_info(State),

top_online_server:dell_all(Score, Jid, Sort, Res),

NewState = changeState(State,N,[]),
Result=mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,10}, {<<"body">>,{struct,[{<<"empty">>,<<"1">>}]} }]}}]}),
%top_online_server:set_redis(Jid),

Jid = State#state.jid,
Sort = State#state.top_status,
%%Sn_id_bin = State#state.sn_id,
Frends = State#state.frends,
Result2= get_info(State),
Info = {Score, Jid, Result2, Sort, Frends},
top_online_server:set_redis2(Jid, Info),

{res,Result,NewState};


case_query(20, <<"query">>,  State, _Other) ->
%% идем в случайную битву Поединок

{Premium, _Pdate} = get_premium_state(State),	 
MeJID4send =  jlib:string_to_jid( ?SEND_FROM),
Name="random_battle",
Pid4pro=gproc:lookup_local_name(Name),
First_user=jlib:string_to_jid(State#state.jid),
%% проверить что у чела достаточно денег на игру или премиум

 Bet = ?RBATTLE_BET,
	case {Premium,Bet =< get_money(State,0)} of
	    {0,false} ->
		Reply = <<"{\"reply\":{\"type\":20,\"body\":{\"error\":{\"type\":\"0J3QtdC00L7RgdGC0LDRgtC+0YfQvdC+INC80L7QvdC10YIg0LLQvtC50L3RiyDQtNC70Y8g0YHRgtCw0LLQutC4\",\"val\":2020}}}}">>,
		{res,Reply,State}
		    ;
	    _ -> %% есть бабло либо премиум
		Num=get_user_perk(State,Bet),
		case (is_pid(Pid4pro)) of %% не требуется потверждать работоспособность данного процесса ибо это часть ядра 
		    true  ->
			%% отправить запрос туда
			gen_server:cast(Pid4pro, {adduser,First_user,Num,MeJID4send});
		    _ ->
			%%запустить нужный процесс
			{ok,Pid } = match_rbattle:start_link(),
			gen_server:cast(Pid, {adduser,First_user,Num,MeJID4send})
		end,
		{res,[],change_status(rbattle,State)}
   end;



case_query(21, <<"query">>,  State= #state{ transaction_state=S }, _Other) when S == 2 ->
%% выход из очереди

 MeJID4send =  jlib:string_to_jid( ?SEND_FROM),
 Name="random_battle",
 Result=[],
	Pid4pro=gproc:lookup_local_name(Name),
	First_user=jlib:string_to_jid(State#state.jid),
	case (is_pid(Pid4pro)) of
	true  ->
	%% отправить запрос туда
	gen_server:cast(Pid4pro, {deluser,First_user,MeJID4send});
	_ ->
	%%запустить нужный процесс
	{ok, Pid } = match_rbattle:start_link(),
	gen_server:cast(Pid, {deluser,First_user,MeJID4send})
       end,
{res,Result,State};
case_query(21, <<"query">>,  State= #state{ transaction_state=S }, _Other) when S =/= 2 ->
%% матчинг для других состояний
	   Ask1 = <<"0JLRiyDQvdC1INC80L7QttC10YLQtSDQv9C+0LrQuNC90YPRgtGMINC+0YfQtdGA0LXQtNGMINGB0LvRg9GH0LDQudC90YvRhSDQsdC+0LXQsg==">>,
   Result = mochijson2:encode(
			{struct,[{<<"reply">>,
			{struct,[{<<"type">>,21},
			{<<"body">>,
			{struct,[{<<"queue">>,
			{struct,[{<<"type">>,1},
			{<<"msg">>,{struct,[{<<"id">>,21},{<<"comment">>,Ask1}]}}
			]}}]}
			}]}}]}),
    
{res,Result,State};


%% запросы к магазину, запросы 40-49. Пока используется 40, 41 и 42
%% transaction_state это признак того не в бою ли или в каком нить ожидании (где учтены ставки) находится игрок

case_query(40, <<"query">>,State = #state{transaction_state = A}, [{<<"type">>, Shop_r}|_] ) when A == 0 ->

    Name = <<"shop">>,
    Pid4pro=gproc:lookup_local_name(Name),
    User_now=jlib:string_to_jid(State#state.jid),

     case (is_pid(Pid4pro)) of
	true  ->
	%% отправить запрос туда
	    gen_server:cast(Pid4pro, {show_all, Shop_r, User_now});
	_ ->
	%% запустить нужный процесс
	    {ok,Pid} = shop:start_link(),
	    gen_server:cast(Pid, {show_all, Shop_r, User_now})
       end,

Result = [],

{res,Result,State};

case_query(40, <<"query">>,State = #state{transaction_state = A}, _Other ) when A == 0 ->

    Name = <<"shop">>,
    Pid4pro=gproc:lookup_local_name(Name),
    User_now=jlib:string_to_jid(State#state.jid),

     case (is_pid(Pid4pro)) of
	true  ->
	%% отправить запрос туда
	    gen_server:cast(Pid4pro, {show_all, 0, User_now});
	_ ->
	%% запустить нужный процесс
	    {ok,Pid} = shop:start_link(),
	    gen_server:cast(Pid, {show_all, 0, User_now})
       end,

Result = [],

{res,Result,State};




%% покупка
%% transaction_state это признак того не в бою ли или в каком нить ожидании (где учтены ставки) находится игрок

case_query(41, <<"query">>,State = #state{transaction_state = A,thing_change_param=OldTh}, [{<<"th_id">>,Buy_id},{<<"count">>,Count},{<<"tid">>, Tid}|_] ) when A==0 ->

?INFO_MSG("buy_thing : ~p~n kredits_num:~p ~n", [Buy_id, Count]),

    User_now=jlib:string_to_jid(State#state.jid),

    {DoneJson, CState, Done, Count_out, Tid_out} = things:buy_thing(Buy_id, Count, OldTh, User_now, Tid),
      
    %%?INFO_MSG("buy_thing  Done: ~p~n CState:~p~n", [Done, CState]),  

ResultSt =   case (Done) of 
	1 ->	NewState = State#state{thing_change_param = CState},
		diff_state(State,NewState),
		set_log(User_now, 2, {Buy_id, Count_out}),
		case CState#thing_change_param_rec.money_1 > OldTh#thing_change_param_rec.money_1 of
		       true -> 
			{_,M1} = case State#state.buy_kredits of
			    {_,L1}->{0,L1};
			    _ -> {0,0}	 end,
			Date= calendar:datetime_to_gregorian_seconds(erlang:localtime()),
			NewState#state{buy_kredits={Date,M1+1}} ;
			_ ->   NewState 
			end
		    ;
	_ -> State
    end,

    Result = DoneJson,

    if (Tid_out>0) -> ResultSt2 = add_auto_buy(Tid_out, Buy_id, Count, ResultSt);
	true -> ResultSt2 = ResultSt
    end,


{res,Result,ResultSt2};
%% покупка вещей автоматическая вроде
case_query(41, <<"query">>,State = #state{transaction_state = A,thing_change_param=_OldTh}, [{<<"th_id">>,Buy_id},{<<"count">>,Count}|_] ) when A==0 ->

?INFO_MSG("buy_thing WOTID : ~p~n kredits_num:~p ~n", [Buy_id, Count]),

    case_query(41, <<"query">>,State, [{<<"th_id">>,Buy_id},{<<"count">>,Count}, {<<"tid">>, 0}])
;

%% чатик между игроками
case_query(49, <<"query">>,State, [{<<"msg_send">>,Msg_send}|_] ) ->
case (Msg_send) of
    M when is_integer(M), M>0 -> 
		State1 = State#state{msg_send = Msg_send},
		Result = <<"{\"reply\":{\"type\":49,\"body\":{\"msg_send\":1}}}">>;
    W when is_integer(W), W=<0 ->     
		State1 = State#state{msg_send = 0},
		Result = <<"{\"reply\":{\"type\":49,\"body\":{\"msg_send\":0}}}">>;
	_ -> State1 = State, 
	     Result = <<"{\"reply\":{\"type\":49,\"body\":{\"msg_send\":-1}}}">>
end,
{res,Result,State1};

case_query(49, <<"query">>,State, [{<<"jid">>,InJid0}, {<<"text">>,Text_in}|_] ) ->
My_jid = jlib:jid_to_string(get_jid_from_state(State)),
  Text_out = lists:flatten([libs_functions:base64_decode(State#state.firstName), " ", libs_functions:base64_decode(State#state.lastName), ": "]),

if (State#state.msg_send>0) ->

if (is_binary(InJid0)) -> InJid = binary_to_list(InJid0);
true -> InJid = InJid0
end,

  libs_functions:send_log_to_user(InJid, 4, {Text_in, Text_out, My_jid}, "#9b2d30");

true -> libs_functions:send_log_to_user(My_jid, 5, {"Вы отключили личные сообщения.", "", My_jid}, "#9b2d30")
end,

Result = [],

{res,Result,State};


%% запросы к рейтинга, запросы 50-59. Пока используется 50

case_query(50, <<"query">>,State, [{<<"val">>,Val}|_] ) ->

    User_now=jlib:string_to_jid(State#state.jid),

    Name = <<"top_offline">>,
    Pid4pro=gproc:lookup_local_name(Name),

     case (is_pid(Pid4pro)) of
	true  ->
	%% отправить запрос туда
	    gen_server:cast(Pid4pro, {get_top, Val, User_now});
	_ ->
	%% запустить нужный процесс
	    {ok,Pid} = top_offline:start_link(),
	    gen_server:cast(Pid, {get_top, Val, User_now})
       end,
      
    %%?INFO_MSG("buy_thing  Done: ~p~n CState:~p~n", [Done, CState]),  
Result = [],

{res,Result,State};


%% личное дело

case_query(51, <<"query">>, State, [{<<"val">>,Val}|_] ) ->

case Val of
    M when is_binary(M) -> Val1 = binary_to_list(Val);
    _ -> Val1 = Val
end,

Link = State#state.link_to_redis,

AA = get_user_state(1, Link, Val1, binary_to_list(State#state.jid_bin)),

case AA of
	 cast -> Result = [];
	 undefined ->  Result = <<"{\"reply\":{\"type\":51,\"body\":{\"error\":{\"type\":\"0JTQsNC90L3Ri9GFINC90LUg0L3QsNC50LTQtdC90L4=\",\"val\":1}}}}">>;
	 _ ->  Result = personal_data(AA)
		 end,

{res,Result,State};

case_query(51, <<"query">>, State, _Any ) ->
    Result = personal_data(State),
{res,Result,State};


case_query(52, <<"query">>, State, [{<<"val">>,Val}|_] ) ->

case Val of
    M when is_binary(M) -> Val1 = binary_to_list(Val);
    _ -> Val1 = Val
end,

Link = State#state.link_to_redis,
%% TODO Переписать!! как минимум встроить проверку что в val jid   а еще круче не лезть в редис руками а через init профиля
AA = get_user_state(2, Link, Val1, binary_to_list(State#state.jid_bin)),

case AA of
	 cast -> Result = [];
	 undefined ->  Result = <<"{\"reply\":{\"type\":51,\"body\":{\"error\":{\"type\":\"0JTQsNC90L3Ri9GFINC90LUg0L3QsNC50LTQtdC90L4=\",\"val\":1}}}}">>;
	 _ ->  Result = user_medals(AA)
end,

{res,Result,State};


case_query(52, <<"query">>,State, _Any ) ->
    Result = user_medals(State),
{res,Result,State};



case_query(53, <<"query">>, State, [{<<"val">>,Val}|_] ) ->
%% TODO Переписать!! как минимум встроить проверку что в val jid   а еще круче не лезть в редис руками а через init профиля
Link = State#state.link_to_redis,

case Val of
    M when is_binary(M) -> Val1 = binary_to_list(Val);
    _ -> Val1 = Val
end,

AA = get_user_state(3, Link, Val1, binary_to_list(State#state.jid_bin)),

case AA of
	 cast -> Result = [];
	 undefined -> Result = <<"{\"reply\":{\"type\":51,\"body\":{\"error\":{\"type\":\"0JTQsNC90L3Ri9GFINC90LUg0L3QsNC50LTQtdC90L4=\",\"val\":1}}}}">>;
	 _ ->  Result = user_achivs(AA)
end,

{res,Result,State};

case_query(53, <<"query">>,State, _Any ) ->
    Result = user_achivs(State),
{res,Result,State};


%% transaction_state=4 это признак того не в бою ли мы
case_query(100, <<"query">>,  State = #state{transaction_state=S}, [{<<"map">>,_N}|_]) when S == 4 ->

%% запрос карты надо запрос переслать серверу битвы если мы в битве
%%route_to = 0, %% куда роутить запросы типа то есть если чел в битве то 3
%%player0, %% игрок который ходит первым
%%player1, %% игрок который ходит вторым
%%you_turn, %% мой ли ход
%%battle_serv, %%pid боевого сервера который обслуживает битву
%% bet=0,    %% ставка
%% battle_id
    %%erlang:statistics(wall_clock), 
    
First_user=jlib:string_to_jid(State#state.jid),
   case (State#state.route_to ) of
	3  ->
	%% отправить запрос туда
	Result=[],
	%% {map,  Id,User,User0,User1,MeJid},
	gen_server:cast(State#state.battle_serv, {map, State#state.battle_id, First_user,
		State#state.player0,State#state.player1,send});

	_ ->
	%% мы не в битве но игрок почему то думает что он в битве
	%% отругатся ему
	%%send_error(MeJID4send, First_user, <<"you're not in battle">>,4,100)
	Result = mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,100}, {<<"body">>,[{struct,[{<<"error">>,
	  {struct,[{<<"type">>,<<"you're not in battle">>},{<<"val">>,4}]}}] }] }]}}]})
    end,
{res,Result,State};
case_query(100, <<"query">>,  State  = #state{transaction_state=S}, [{<<"skip">>,_N}|_]) when S ==4 ->
%%  {"query":100, "skip":1 }

   case {State#state.route_to,State#state.you_turn} of
	{3,1}  -> %%  ход  включиЛ
	%% отправить запрос боевому серверу
	First_user=jlib:string_to_jid(State#state.jid),
	gen_server:cast(State#state.battle_serv, {skip, State#state.battle_id,First_user,
	State#state.player0,State#state.player1}),
	Result=[];
	_ ->
	%% мы не в битве но игрок почему то думает что он в битве
	%% отругатся ему
	%%send_error(From, To, <<"you're not in battle">>,5,100)
	Result = mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,100}, {<<"body">>,[{struct,[{<<"error">>,
	  {struct,[{<<"type">>,libs_functions:base64_encode("Некорректное действие!")},{<<"val">>,5}]}}]}] }]}}]})
    end,
{res,Result,State};
case_query(100, <<"query">>,  State = #state{transaction_state=S, battle_type=T, tour_num = Num, premium_in_battle=Pstat,
					     enemyFio =Enemyfio}, [{<<"exit">>,_N}|_]) when S ==4 ->
%%  {"query":100, "exit":1 }
%% TODO подумать о завершении таймера максимальной длительности боя
 First_user=jlib:string_to_jid(State#state.jid),
   case {State#state.route_to,State#state.you_turn} of
	{3,1}  -> %%  мой ход  включиЛ
	%% отправить запрос боевому серверу

	gen_server:cast(State#state.battle_serv, {exit, State#state.battle_id,First_user, State#state.player0,State#state.player1}), 

	X3 = case Pstat of
		    No when No == 0  -> 
			Bet = (-1)*State#state.bet,
			add_money_0(State, Bet,send); %% снять ставку ибо премиума нет а чел проиграл
		    _  -> Bet=0, State %% ставку не снимать
		    end,
	X4 = (stop_turn_timer(X3))#state{ref_turn_timer=0},
	Stat_list = stat_local_param_add_list([{89,0,set}],{T,Num},0,[{81,1,add}]),
	msg_self_change_stat(Stat_list), %%	Количество капитуляций и проигрыш этого типа боя
	X5 = change_status(wait_tournament_exit,X4),
	NewState =set_state_at_endbattle( X5,"exit"),
%% #state{route_to = 0, battle_id = 0,	  battle_serv=0, bet = 0},
	%%{"reply":{"type":102,"body":{"exit":1}}}
	Result=mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,100}, {<<"body">>,[{struct,[{<<"exit">>,1}]}]}]}}]}),
	{Btype,Battle_type_for_log} = case Num >0  of
	       true -> %% турнир надо сообщить что мы вылетели из турнира
					   Text = <<"{\"reply\":{\"type\":159,\"body\":{\"turnir_game_over\":{\"type\":\"game_over\",\"val\":159}}}}">>,
					   libs_functions:wrap_cron_once(2, {p_sender, send_msg, [First_user, Text]}),
					      {3, 100+Num};
	       _ -> {1,T}  end,		

		Text_lose = mochijson2:encode({struct,[{<<"reply">>,{struct,[{<<"type">>,100},{<<"body">>,
			[{struct,[{<<"end">>,
			   {struct,[
				{<<"win_player">>,20}, %% дизертир хуле

				{<<"result">>, {struct,[
					{<<"type">>,Btype},
					{<<"param0">>, Bet },
					{<<"param1">>,0},
					{<<"param2">>,0}
					]}}
				] }}
			]}]
			}]}}]}),
	   

	   libs_functions:wrap_cron_once(2, { libs_functions, send_log_end_battle, [First_user, Battle_type_for_log,2,-1,{Bet*3,0},Enemyfio,Pstat]}),
	   %%	libs_functions:send_log_end_battle(First_user, Battle_type_for_log,2,-1,{0,0},Enemyfio,Pstat),
	   libs_functions:wrap_cron_once(1, {p_sender, send_msg, [First_user, Text_lose]});
	_ ->
	%% не ход чела
	NewState = State,
	Result = mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,102}, {<<"body">>,{struct,[{<<"error">>,
	  {struct,[{<<"type">>,libs_functions:base64_encode("Некорректное действие!")},{<<"val">>,6}]}}]} }]}}]})
    end,
sovets:get_sovet( First_user),
{res,Result,NewState};

case_query(100, <<"query">>,  State0= #state{stat_change_in_turn=NewC1,transaction_state=S },
	   _Other=[{<<"turn">>,N}|_]) when S ==4 ->
%% ход игрока в битве
%%  {"turn":{"go":{"x0":2,"y0":5, "x1":2,"y1":6}}
%%?INFO_MSG(" user: ~p	turn: ~p~n", [N,State0#state.jid]),

%% {struct, [{<<"go">>,{struct,[{<<"x0">>,2},{<<"y0">>,5},{<<"x1">>,2},{<<"y1">>,6}]}}]}
%%debug_send_me_state(State#state.you_turn,State#state.jid_bin),
State = stop_turn_timer(State0),
	First_user=jlib:string_to_jid(State#state.jid),
NewState =   case {State#state.route_to,State#state.you_turn} of
	{3,1}  -> %%  ход  включиЛ
	%% отправить запрос боевому серверу

	gen_server:cast(State#state.battle_serv, {change, State#state.battle_id,First_user,
	State#state.player0,State#state.player1, N}),
	  %%	    Result=[],
	 State#state{stat_change_in_turn = NewC1+1};
	_ ->
	%% мы не в битве но игрок почему то думает что он в битве или 
	%% отругатся ему
	send_timeout_to_turn(First_user,N),
%%	  Result=mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,100}, {<<"body">>,[{struct,[{<<"error">>,
%%		 {struct,[{<<"type">>,<<"0JLRiyDQvdC1INGD0YHQv9C10LvQuCDRgdC00LXQu9Cw0YLRjCDRhdC+0LQ=">>},{<<"val">>,123}]}}]}] }]}}]}),
	     State
    end,
{res,[],NewState};

case_query(100, <<"query">>,  State = #state{transaction_state=4,  kd_gr1 = Kd1, kd_gr2 = Kd2}, _Other=[{<<"slot">>,N}|_]) ->
%%  {"query":100,{"slot":{"id":1, "param0":1, "param1":2}} }
%% требуется получить данные 
%% ?INFO_MSG(" jid ~p : ~p ~p ~n", [State#state.jid,Kd1,Kd2]),
%% активация слота  в зависимости от типа слота применить указанный эффект
%% ?INFO_MSG(" go?? : ~p~n", [N]),
    {struct, [{<<"id">>,SlotNum} | Params]} = N,
     Par_array = array:new([{size, 4}, {default, 0}, {fixed, true}]),
	P8 = fun(A, AccIn) -> 
		 %%	A  = {<<"param0">>,1}
		     case A of
			 {<<"param0">>,Value0} ->    array:set(0, Value0, AccIn);
			 {<<"param1">>,Value1} ->    array:set(1, Value1, AccIn);
			 _ ->
			     AccIn
		    end
	 end,
    Slot_param_arr = lists:foldl(P8, Par_array , Params),	
    Is_KD_on = fun(Num) -> 
		     case Num of
			 Gr1 when Gr1 < 3, Kd1 == 0 ->		{4  , Kd2, ok};
			 Gr2 when Gr2 > 2, Gr2 < 6, Kd2 == 0 -> {Kd1, 6	 , ok};
			 _ -> 
%			     ?INFO_MSG(" kd logs: ~p~n", [{Kd1, Kd2  , fail}]),
			     {Kd1, Kd2	, fail}
		    end
	 end,

%% наш ход и не конец хода
%%  в бою
{Nkd1,Nkd2, KD} = Is_KD_on(SlotNum),
 case {KD, State#state.route_to, State#state.you_turn} of
	{ok, 3, 1}  -> %%  ход	включиЛ
	%% 
	{Result, NewState } = activate_user_slot(SlotNum,State,Slot_param_arr);

	_ ->
	%% мы не в битве но игрок почему то думает что он в битве или 
	%% отругатся ему
	 NewState = State,
	 First_user=jlib:string_to_jid(State#state.jid),
	send_timeout_to_turn(First_user,N),
	Result=[]
	%%    Молчим 

    end,
{res,Result, NewState#state{ kd_gr1 = Nkd1, kd_gr2 = Nkd2 } };


case_query(100, Query ,State=#state{transaction_state=S},Other) ->
%% матчинг для ошибок
%% для 100 особый тип ошибки там в боди Массив [] вместо {}
%% сюда надо логи изза чего суда сматчилось?!
?INFO_MSG("Error Jid : ~p~n", [State#state.jid]),
?INFO_MSG("Error Status : ~p~n", [S]),
?INFO_MSG("Error QUERY : ~p~n", [Query]),
?INFO_MSG("Error QUERY Other: ~p~n", [Other]),
%%Result = <<"{\"reply\":{\"type\":100,\"body\":[{\"error\":{\"type\":\"unknown request\",\"val\":4}}]}}">>,
{res,[],State};

case_query(150, <<"query">>,State = #state{transaction_state=V, tour_ref_bet_timer = Reff},_Other=[{<<"type">>,N}|_]) 
when V ==0 ; V == 3 ; V==5 ->
%% Турнир либо мы свободны либо в турнире либо в ожидании турнира
%%   требуется если мы в турнире переправить в турнир запросы о кол-во народа

%%?INFO_MSG(" State#state.tour_serv : ~p query ~p  ~p~n", [State#state.tour_serv , N , Bet]),

Reply =	   case N of
	0 -> %% выдать список на текущий турнир 
	    X0=State , 
	   case libs_functions:alive(State#state.tour_serv) of
		  true ->	    gen_server:cast(State#state.tour_serv, {show,jlib:string_to_jid(State#state.jid)}), [];
		  _ ->		    
		    
	    mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,150}, {<<"body">>,
	    {struct,[{<<"turnir_queue">>, {struct,[{<<"list">>,[]}]}  }]}
	    }]}}]})
			end;
	1 when V==0 -> %% встать в очередь  мы ни в турнире ни в ожидании

	   First_user  =  { State#state.firstName
	  , State#state.lastName 
	  , State#state.middleName 
	  , jlib:string_to_jid(State#state.jid)	 %% игрок
	  , get_rank(State) %% Звание
	  , get_litleRank(State)  %% краткое звание
	  , get_money(State,0)	%%  монеток
	  , get_score(State) %% очков рейтинга
	  , get_ava(State)  %% ава
	  , State#state.send_status
	  , State#state.sn_id
	   },  Num = gen_server:call(plg_go_turnir, {add,First_user}),

		X0 = change_status(wait_tournament,{State,Num}),
	       ?INFO_MSG(" State#state.tour_serv : ~p ~p~n", [X0#state.tour_serv , X0#state.transaction_state]),
		   []  ;
	2 when V==3 ->
		   %% покинуть очередь	   если мы не в битве 
		   RR = case libs_functions:alive(State#state.tour_serv) of
			    true ->
				 libs_functions:wrap_cron_cancel(Reff), %% отменяем таймер ставки
				gen_server:cast(State#state.tour_serv, {exit,State#state.tour_num}),
				    "{\"reply\":{\"type\":152,\"body\": {  \"turnir_queue\": { \"type\":3 ,  \"list\": [ ]}  } }}";
			    _ ->   ?INFO_MSG(" State#state.tour_serv : ~p ~p~n", [State#state.tour_serv , State#state.transaction_state]),
				    gen_server:call(plg_go_turnir, {del,jlib:string_to_jid(State#state.jid),State#state.tour_num}),
					    []
			end,
		    ?INFO_MSG(" Exit in tourn 1: ~p ~p~n", [State#state.tour_serv , State#state.jid]),
		   X0 = change_status(wait_tournament_exit,State), RR ;
	  2 when V == 5 -> %% выход из ожидания турнира 
		   %% ВНИМАНИЕ чтоб не было  рейс кондишн
	     Ress =  gen_server:call(plg_go_turnir, {del,jlib:string_to_jid(State#state.jid),State#state.tour_num}),

		   case Ress of
		       ok ->		    X0 = change_status(wait_tournament_exit,State), [] ;
		       _ -> %% похоже нас еще не передали в турнир а чел решил выйти раньше не даем ему это сделать
			   ?INFO_MSG(" Exit in tourn 2: ~p ~p~n", [State#state.tour_serv , State#state.jid])
			   ,X0=State
			   ,
			   "{\"reply\":{\"type\":152,\"body\": {  \"turnir_queue\": { \"type\":2 ,  \"list\": [ ]}  } }}"
			       end;
		    
	 _ -> X0=State,
		    []
			end,
{res,Reply,X0};
case_query(151, <<"query">>,State,Other) ->
case_query(150, <<"query">>,State,Other);

case_query(152, <<"query">>,State,Other) ->
case_query(150, <<"query">>,State,Other);

%% Дуэли
case_query(300,	 <<"query">> ,State= #state{duel_state = S,transaction_state=S },Other) when S==0 ->
%% хочу кого либо пригласить в дуэль 
%% никого в этот момент еще не приглашал

[{<<"jid">>,Enemy},{<<"bet">>,Bet}]=Other,
%%  проверить достаточность монет на ставку
    
Pid4pro = gproc:lookup_local_name(binary_to_list(Enemy)),
%%?INFO_MSG("Pid: ~p~n", [Pid4pro]),
{Premium, _Pdate} = get_premium_state(State),	 

 If_term =   case Premium of
	1 -> true;
	_ ->get_money(State,0) >= Bet
	     end,
    


 {Result,NewState} = case ({libs_functions:alive(Pid4pro), If_term}) of
	{true,true } ->
	%% отправить запрос туда
	    MeJsonUser = get_user_rec(State), %% структура с реальным jid
	    Query = {duel, MeJsonUser, Bet, get_user_perk(State, Bet)},
	    %% Запустить таймер через 23 секунды прислать отказ от дуэли
	    {ok,Ref} = libs_functions:wrap_cron_once(22, {gen_server, cast, [self(), {duel_timeout,make_json_duel_deny(2) }]}),
	    gen_server:cast(Pid4pro, Query),
	    test_change_status(State,1,1829),
	    {[], State#state{duel_bet=Bet,duel_state=1,duel_ref=Ref,transaction_state=1} };

	{_,false}-> %% бабла нет :(  f|t,f
	   {make_json_duel_deny(4),State};	      
	  
	{_false,_true}-> %% пользователь оффлайн f,t
	  {make_json_duel_deny(3),State}
    end,
{res,Result,NewState};

case_query(300,	 <<"query">> ,State,_Other)  ->
Result = make_json_duel_deny(8), %% ошибочная попытка пригласить тем кто в битве или в дуэли
{res,Result,State};

case_query(301,	 <<"query">> ,State = #state{duel_state = S,transaction_state=1 },Other) when S == 2 ->
%%  Согласен на дуэль
%% провреить что согласен с тем

[{<<"jid">>,Enemy0},{<<"bet">>,Bet}]=Other,
Enemy = binary_to_list(Enemy0),
%% Enemy
    {Jid, EnemyPerk} = State#state.duel_enemy,
    RealBet=State#state.duel_bet,
    RealJid = jlib:jid_to_string(Jid),
X1=  case ({Enemy, Bet}) of
	{RealJid, RealBet} -> %% валидное согласие
		  %% отменяем чужой таймер
		   Pid4pro = gproc:lookup_local_name(jlib:jid_to_string(Jid)),
		   Query = {duel_timer_stop},
		   case	 libs_functions:alive(Pid4pro) of
		       true ->	gen_server:cast(Pid4pro, Query) ;
		       _ ->    ok	end, 
	  %% остановить таймер поменять состояние 
	  libs_functions:wrap_cron_cancel(State#state.duel_ref), %% отменяем таймер 
	  go_duel_now({EnemyPerk,Jid},State),
	   State#state{duel_ref=0,duel_bet=0}; %%duel state обнулится автоматом после начала битвы 
      {_,_} -> State  %% согласие не тому
		end,
{res,[],X1};
case_query(301,	 <<"query">> ,State,_Other)  ->
Result = make_json_duel_deny(9), %% ошибочная попытка согласится
{res,Result,State};

case_query(302,	 <<"query">> ,S = #state{ duel_enemy=A,transaction_state=User_stat },_Other) when  A == undefined; A==0 ->
X2 =	case User_stat of
	1 ->test_change_status(S,0,1875),  S#state{ duel_ref=0 , duel_state=0,transaction_state=0 }; %% выход из косячной дуэли
	_ -> S
		end,
{res,[],X2 };

case_query(302,	 <<"query">> ,State= #state{ duel_enemy= {Jid,_EnemyPerk},transaction_state=1  },Other) ->
%%   отказ от дуэли
[{<<"jid">>,Enemy0},{<<"bet">>,Bet}]=Other,
Enemy = binary_to_list(Enemy0),


    RealBet=State#state.duel_bet,
    RealJid = jlib:jid_to_string(Jid),

   X1 =	   case {Enemy,Bet} of
	       {RealJid, RealBet}-> %%	отказываем противнику
		   User = Jid, 
		   Text1 =  make_json_duel_deny(2),
		   Pid4pro = gproc:lookup_local_name(jlib:jid_to_string(User)),
		   Query = {duel_timeout,Text1},
		   case	 libs_functions:alive(Pid4pro) of
		       true ->	gen_server:cast(Pid4pro, Query) ;
		       _ ->    ok	end, 

		   libs_functions:wrap_cron_cancel(State#state.duel_ref), %% отменяем свой таймер 

		   %%  поменять состояние
		   test_change_status(State,0,1902),
		   State#state{ duel_ref=0 , duel_state=0,transaction_state=0 } ; 
      _ -> State %% игрок отказал кому то другому либо не успел
		end,
{res,[],X1};
case_query(400,	 <<"query">> ,State= #state{route_to=R, stat_tr_count= Tr},_Other) when R == 0; R == 4 ->
%% начало тренировочной битвы
Result = [],
NTr =  case R of
      0 ->   Tr;
      4 ->   msg_self_change_stat([{1,1,add},{94,Tr,max},{89,0,set},{80,0,set},{90,0,set}]) %% прошлое поражение не защитано
	    ,get_achiv_after(1), 0
	     end,
%% make_json_duel_deny(9), %% ошибочная попытка согласится

{res,Result,State#state{route_to=4, stat_tr_count= NTr}};

case_query(401,	 <<"query">> ,State= #state{jid=Jid, route_to=R, stat_tr_count=Tr},[{<<"done">>,N}|_]) when R == 4 ->
%% завершение тренировки   {2,?ADD_SCORE_TR,add}

X2 =  case N of
      1 ->  msg_self_change_stat([{0,1,add},{89,5,set},{80,0,set},{90,0,set}]), 
      S = State#state{route_to=0,stat_tr_count= Tr+1},	   
      {Pstat, _Pdate} = get_premium_state(S),
      Fio1 = lists:flatten(["бот", " ","Иван" ," ","Кузьмич"]),
      libs_functions:send_log_end_battle(Jid, 3,0,1,{10,0},Fio1,Pstat),
	    case Pstat of
		0 ->	  add_money_0(S, 10, notify);
		 _ ->S
			end
       ;	  %% победа и очки ПВЕ
     
      F ->  
	msg_self_change_stat([{1,1,add},{94,Tr,max},{89,0,set}]), %% поражение и максимум продолжительности тренировок
	 {Pstat, _Pdate} = get_premium_state(State),
	 Fio1 = lists:flatten(["бот", " ","Иван" ," ","Кузьмич"]),
	 % F  1 победа	2 Капитуляция  3  неправильные ходы
	 libs_functions:send_log_end_battle(Jid, 3,F,0,{0,0},Fio1,Pstat),
	 State#state{route_to=0,stat_tr_count= 0}
		end,
User=get_jid_from_state(State),
sovets:get_sovet(User),
Result = [],
get_achiv_after(1),
%% make_json_duel_deny(9), %% ошибочная попытка согласится
{res,Result,X2};

case_query(450,	 <<"query">> ,State=  #state{statistics=DB},_) ->
%% На удаление!
%% полная статистика

   plg_save_2db:save_profile_2db(State),
   P8 = fun(I, A, AccIn) -> 
		 %% выдаем список 
	       [io_lib:format("[~p] ~p ", [I,A]) |AccIn]
	 end, %% fun

    M1 = array:foldl(P8, [], DB#statistics.stat_always),
    
Result = list_to_binary(M1),
{res,Result,State};
case_query(451,	 <<"query">> ,State,_) ->
%%На удаление! список начавшихся турниров с состоянием если турниры через чур долгие
    Name = now_tour,
All_tourn =  pg2:get_members(Name),

    P8 = fun(Pid,  AccIn) when is_pid(Pid) -> 
		 %% запрашиваем время турнира
		 {D,{A,B,_}}= gen_server:call(Pid,get_time),
		 T = D*24*60+A*60+B,
		 [ case T>30 of
		     true -> {T, gen_server:call(Pid,dump_state)};
		     _ -> T
			     end
		      |AccIn]; 
	    (_,AccIn) -> AccIn
	 end, %% fun
    


   R =	lists:foldl(P8,[], All_tourn),
    
Result = {length(All_tourn),length(R)},
{res,Result,State};

case_query(Type, Query ,State,Other) ->
%% матчинг для ошибок
?INFO_MSG("Error QUERY: ~p~n", [Query]),
?INFO_MSG("Error QUERY Type: ~p~n", [Type]),
?INFO_MSG("Error QUERY Other: ~p~n", [Other]),

%Bin1 = <<"{\"reply\":{\"type\":">>,
%Bin3 = <<",\"body\":{\"error\":{\"type\":\"unknown request\",\"val\":4}}}}">>,
%{res,list_to_binary([Bin1, integer_to_list(Type),Bin3]),State}.
{res,[],State}.

%%--------------------------------------------------------------------
%% Func: add_money_0(State, Value,Send_f)
%% Description: добавить указанное число монет
%%--------------------------------------------------------------------
add_money_0(State, Value,no_send)->
Tcp=(State#state.thing_change_param),

    New_money = case Tcp#thing_change_param_rec.money_0+Value of
	    M when M < 0 -> 0;
	    S ->S    end,

    Ntcp = Tcp#thing_change_param_rec{money_0=New_money},
    State#state{thing_change_param=Ntcp};

add_money_0(State, Value, _ )->
%% известить игрока об изменении
  Ask1 = <<"0LrQsNC60L7QuSDQvdC40YLRjCDRgtC10LrRgdGCINC/0YDQviDRh9GC0L4g0LLRiyDQstGB0YLQsNC70Lgg0LIg0L7Rh9C10YDQtdC00Ywg0Lgg0LHQu9CwINCx0LvQsCA=">>,
  User=jlib:string_to_jid(State#state.jid),
Tcp=(State#state.thing_change_param),
    New_money = Tcp#thing_change_param_rec.money_0+Value,
Result=mochijson2:encode(
{struct,[{<<"reply">>,
	  {struct,[{<<"type">>,1},
		   {<<"body">>,
{struct,[{<<"game_window">>,{struct,[{<<"player">>,{
			   struct,[{<<"money_0">>, New_money},
				     {<<"msg">>,{struct,[{<<"id">>,8},{<<"comment">>,Ask1}]}}
				   ]}}]	  }}]}}]}}]}),
send_msg(User,	Result),     
add_money_0(State, Value,no_send).

add_money_1(State, Value,no_send)->
Tcp=(State#state.thing_change_param),

    New_money = case Tcp#thing_change_param_rec.money_1+Value of
	    M when M < 0 -> 0;
	S ->S	 end,

    Ntcp = Tcp#thing_change_param_rec{money_1=New_money},
    
%пишем в лог
buy_log:send_log(State#state.jid, Tcp#thing_change_param_rec.money_1, Value),
State#state{thing_change_param=Ntcp};

add_money_1(State, Value, _ )->
%% известить игрока об изменении
  Ask1 = <<"0LrQsNC60L7QuSDQvdC40YLRjCDRgtC10LrRgdGCINC/0YDQviDRh9GC0L4g0LLRiyDQstGB0YLQsNC70Lgg0LIg0L7Rh9C10YDQtdC00Ywg0Lgg0LHQu9CwINCx0LvQsCA=">>,
  User=jlib:string_to_jid(State#state.jid),
Tcp=(State#state.thing_change_param),
    New_money = Tcp#thing_change_param_rec.money_1+Value,
Result=mochijson2:encode(
{struct,[{<<"reply">>,
	  {struct,[{<<"type">>,1},
		   {<<"body">>,
{struct,[{<<"game_window">>,{struct,[{<<"player">>,{
	       struct,[{<<"money_1">>, New_money},
		     {<<"msg">>,{struct,[{<<"id">>,8},{<<"comment">>,Ask1}]}}
		   ]}}]	  }}]}}]}}]}),
send_msg(User,	Result),     
add_money_1(State, Value,no_send).

%%--------------------------------------------------------------------
%% @doc Добавляет ПВП очки рейтинга
%% @spec
%% @end
%%--------------------------------------------------------------------

add_score(State, Value)->
%% известить об изменении
msg_self_change_stat([{7,Value,add}]),
State.



%%--------------------------------------------------------------------
%% @doc	 Доступ к ПВП рейтингу 
%% @spec get_score(State)-> integer()
%% @end
%%--------------------------------------------------------------------
get_score(_State = #state{statistics = DB}) ->
array:get(7, DB#statistics.stat_always).

%%--------------------------------------------------------------------
%% @doc Меняет указанные состояние игрока
%% @spec  changeState(State,N,L) -> #state{}
%% @end
%%--------------------------------------------------------------------

changeState(State,[],_L) ->
State;
changeState(State,[Now_param|N],L) ->
X1= case Now_param of
    {<<"firstName">> ,Value} -> State#state{firstName	= Value};
    {<<"lastName">>  ,Value} -> State#state{lastName	= Value};
    {<<"middleName">>,Value} -> State#state{middleName	= Value};
    {<<"frends">>    ,Value} ->	 gen_server:cast(self(), {make_friends_offline}), State#state{frends	  = Value };
    {<<"allow_duel">>	 ,Value}  ->  State#state{duel_allow	  = Value};
    {<<"profile_link">>	 ,Value}  ->  State#state{profile_link	  = Value};
    {<<"user_data">>  ,Value}	  ->  State#state{user_data    = Value};
    _ -> State
end,
changeState(X1,N,[Now_param|L]).


%%--------------------------------------------------------------------
%% Func: send_error(From, To, Type,Val)
%% Description: send error msg to user
%%--------------------------------------------------------------------
send_error(From, To, Type,Val,RType) ->
Text=mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,RType}, {<<"body">>,{struct,[{<<"error">>,
	  {struct,[{<<"type">>,Type},{<<"val">>,Val}]}}]} }]}}]}),
echo(From, To, Text).

%%--------------------------------------------------------------------
%% Func: echo(From, To, Taxt)
%% Description: send Text to user
%%--------------------------------------------------------------------
echo(_From, _To_me, []) ->
ok;
echo(_From_me, To,  Text) ->
send_msg(To,  Text).


%%--------------------------------------------------------------------
%% @doc функция отправки сообщения клиенту 
%% @spec send_msg( To,	Text) -> ok.
%% @end
%%--------------------------------------------------------------------
send_msg( To,  Text) ->
p_sender:send_msg(To,  Text).




%%--------------------------------------------------------------------
%% @doc функция выдает в нужном формате описание пользователя для оповещения челов в бою
%% @spec  get_info_for_fast_init_battle(State) -> typle()
%% @end
%%--------------------------------------------------------------------
get_info_for_fast_init_battle(State) -> 
Score = get_score(State),
Sn_id_bin = State#state.sn_id,
{struct,[
    {"firstName",  State#state.firstName},
    {"lastName",   State#state.lastName},
    {"rang",	   get_rank(State)},
%%    {"middleName", State#state.middleName},
    {"img",	   get_ava(State)  },
    {"jid",	   State#state.jid_bin},
    {"reit",	   Score },
%% слоты
    get_slot(0,0,State),
    get_slot(1,1,State),
    get_slot(2,2,State),
    get_slot(3,3,State),
    get_slot(4,4,State),
    get_slot(5,5,State),
%%    get_slot(6,6,State),
%%    get_slot(7,7,State),
%%    get_slot(8,8,State),
    {"sn_id",	   Sn_id_bin}
]}.



%%--------------------------------------------------------------------
%% @doc Функция возращает информацию по пользователю
%% @spec get_info(M,State) -> #state{}.
%% @end
%%--------------------------------------------------------------------
get_info(State) ->

Score = get_score(State),
_Jid = State#state.jid,
Sort = State#state.top_status,
Sn_id_bin = State#state.sn_id,

{Premium, Pdate} = get_premium_state(State),

%%    ?INFO_MSG("State : ~p~n", [State]),
    Money_0 = get_money(State,0),
%%    ?INFO_MSG(" get_money(State,0) : ~p~n", [Money_0]),
Result = mochijson2:encode({struct,[
    {"firstName",    State#state.firstName},
    {"lastName",     State#state.lastName},
    {"middleName",   State#state.middleName},
    {"jid",	     State#state.jid_bin},
    {"rang",	     get_rank(State)},
    {"money_0",	     Money_0},
    {"lrank",	     get_litleRank(State)},
    {"game_img",     get_ava(State)},
    {"reit",	     Score },
    {"sort",	     Sort  },
    {"duel_st",	     State#state.duel_allow},
    {"sn_id",	     Sn_id_bin},
    {"premium",	     Premium},
    {"premium_date", Pdate}
]}),
Result.

%%--------------------------------------------------------------------
%% @doc Функция должна менять состояния согласно первому атому
%% @spec change_status(M,State) -> #state{}.
%% @end
%%--------------------------------------------------------------------
change_status(rbattle,State) ->
test_change_status(State,2,2204),
State#state{  
 transaction_state = 2
  };
change_status(rbattle_exit,State) ->
test_change_status(State,0,2209),
State#state{  
 transaction_state = 0
  };
change_status(duel,State) ->
test_change_status(State,1,2214),
State#state{  
 transaction_state = 1
  };
change_status(duel_exit,State) ->
test_change_status(State,0,2219),
State#state{  
 transaction_state = 0
  };

change_status(wait_tournament,{State,Num}) ->
%% турнир
?INFO_MSG(" Num tour : ~p~n", [Num]),
?INFO_MSG(" Jid : ~p~n", [State#state.jid]),
%%  Состояние профиля 0 запрос разрешен
%%  Состояние профиля 1 игрок в дуэли
%%  Состояние профиля 2 игрок в поединке
%%  Состояние профиля 3 игрок в турнире
%% 4 бой
test_change_status(State,5,2233),
State#state{
  tour_num = Num %% позиция в турнире 
, tour_state =1
, transaction_state = 5
, tour_serv =0
   };
change_status(wait_tournament_exit,State) ->
test_change_status(State,0,2241),
State#state{  tour_num = 0 %% позиция в турнире 
, tour_state =0
, transaction_state = 0
, tour_serv =0
  };
change_status(enter_tournament,{State,Num,Bet,Pid}) ->
?INFO_MSG(" Num tour : ~p~n", [Num]),
?INFO_MSG(" Jid : ~p~n", [State#state.jid]),
test_change_status(State,3,2250),
State#state{  tour_num = Num %% позиция в турнире 
, tour_serv =Pid    %% сервер турнира
, bet= Bet
, tour_state =1
, transaction_state = 3
  };
change_status(quit_tournament,State) ->
%% для того чтоб не было багов с дуэлью сразу после вылета с турнира хатолкаем как будто его недавно пригласили
{Mega, Seconds, _} = erlang:now(),
  Timestamp = Mega * 1000000 + Seconds+5, %% Через 4 секунды игрок получит вылет с турнира плю 1 сек на учет времени TTL
%% то есть как будто его пригласили в будущем
test_change_status(State,0,2262),
State#state{  
tour_num = 0 %% позиция в турнире 
, tour_serv = 0	   %% сервер турнира
, bet =0
, tour_ref_bet_timer = 0
, tour_state =0
, transaction_state = 0
, duel_time=Timestamp
  }.
%%change_status(_M,State) -> State.

%%--------------------------------------------------------------------
%% @doc выдает все необходимые параметры для боя (перки)
%% @spec get_user_perk(X0,Bet) -> {user_perk,X0#state.score,Param#perk.avia,Param#perk.brone,Param#perk.soldier,Bet}
%% @end
%%--------------------------------------------------------------------
get_user_perk(State=#state{user_param = S}, Bet) ->
{user_perk,get_score(State),S#perk.avia,S#perk.brone,S#perk.soldier,Bet,
 get_info_for_fast_init_battle(State)
 }.


%%--------------------------------------------------------------------
%% @doc Результаты турнира
%% @spec win_or_lose_tournir(lose|win,Btype,X1) -> #state{}
%% @end
%%--------------------------------------------------------------------
win_or_lose_tournir(win, 3, X1 = #state{tour_serv = S,tour_num = N}, _ ) when is_pid(S) =:= true ->
    gen_server:cast(S, {win, N,get_jid_from_state(X1)}), 

    case N of
      E when E >= 29 -> change_status(quit_tournament,X1); %% турнир завершился
	_ -> test_change_status(X1, 3,2295),
	    X1#state{ transaction_state = 3} %% Турнир продолжается
    end;

win_or_lose_tournir(lose, 3, State,User) ->
%% выйти из турнира
    Text_lose = <<"{\"reply\":{\"type\":159,\"body\":{\"turnir_game_over\":{\"type\":\"game_over\",\"val\":159}}}}">>,
    libs_functions:wrap_cron_once(?END_BATTLE_SLEEP_INTERVAL+4, {p_sender, send_msg, [User, Text_lose]}),
 change_status(quit_tournament,State);

win_or_lose_tournir( _, _, State, _) ->
State.

%%--------------------------------------------------------------------
%% @doc Получает из статуса jid игрока для отправки сообщения в jabber
%% @spec get_jid_from_state(State) -> #jid{}
%% @end
%%--------------------------------------------------------------------
get_jid_from_state(State) ->
    jlib:string_to_jid(State#state.jid).
%%--------------------------------------------------------------------
%% @doc Возвращает описание игрока в формате PLG_TOURNAMET_USER_REC
%% @spec get_json_user_rec(State) -> #user{}
%% @end
%%--------------------------------------------------------------------

get_user_rec(State) ->
#user{ num=0,
      firstName=State#state.firstName,
      lastName=State#state.lastName,
      middleName=State#state.middleName,
      jid=get_jid_from_state(State),
      rank=get_rank(State),
      litleRank=get_litleRank(State),
      money_0=get_money(State,0),
      score = get_score(State),
      ava=get_ava(State)
      , slot_state=0
      , user_status = State#state.send_status
      , sn_id=State#state.sn_id
     }.


%%--------------------------------------------------------------------
%% @doc Функция собирает json для отказа от дуэли
%% @spec make_json_duel_deny(Type) -> list()
%% @end
%%--------------------------------------------------------------------

make_json_duel_deny(Type) ->
%%Типы 
%% 1 установлен автоотказ
%% 2 игрок отказался
%% 3 чел оффлайн
%% 4 у тебя нет денег нет на ставку
%% 5 неведомая ебаная хуйня
%% 6 в битве
%% 7 в турнире
%% 8 в дуэли (он не согласился и не отказался) 
%% 9 время ожидания истекло 
    mochijson2:encode( {struct,[{<<"reply">>,
			{struct,[{<<"type">>,302},
			{<<"body">>,
			{struct,[{<<"duel">>,
			{struct,[{<<"type">>,Type}
			]}}]}
			}]}}]}).
%%--------------------------------------------------------------------
%% @doc выдвет кол-во денег у игрока
%% @spec get_money(State,Type) -> integer()
%% @end
%%--------------------------------------------------------------------
get_money(State,1) ->
Tcp=(State#state.thing_change_param),
Tcp#thing_change_param_rec.money_1;
get_money(State,0) ->
Tcp=(State#state.thing_change_param),
Tcp#thing_change_param_rec.money_0.
%%--------------------------------------------------------------------
%% @doc	 выдает аву из профиля
%% @spec  get_ava (State) -> binary()
%% @end
%%--------------------------------------------------------------------
get_ava(State)->
 Tcp=(State#state.thing_change_param),
Tcp#thing_change_param_rec.ava.

%%--------------------------------------------------------------------
%% @doc	 выдает звание из профиля
%% @spec  get_rank (State) -> binary()
%% @end
%%--------------------------------------------------------------------
get_rank(_State = #state{achiv = Achiv}) ->
    {Rank_id, _Date_get, _Num} = Achiv#getted.rank,
    Achiv_info = achiv_info:show_info(Rank_id, 1),
    base64_encode(Achiv_info#achiv.name)
.

%%--------------------------------------------------------------------
%% @doc	 выдает краткое звание из профиля
%% @spec  get_litleRank (State) -> binary()
%% @end
%%--------------------------------------------------------------------
get_litleRank(_State = #state{achiv = Achiv}) ->
    {Rank_id, _Date_get, _Num} = Achiv#getted.rank,
    Achiv_info = achiv_info:show_info(Rank_id, 1),
    base64_encode(Achiv_info#achiv.sname)
.

%%--------------------------------------------------------------------
%% @doc Функция отправяет чела с противником в дуэль
%% @spec go_duel_now({EnemyPerk,Jid},State) ->ok.
%% @end
%%--------------------------------------------------------------------

go_duel_now({User1,Jid1},State) ->
{user_perk,Score1,_Avia1,_Brone1,_Soldier1,Bet,_Profile} = User1,

  User2 = get_user_perk(State,Bet),
 Jid2  = get_jid_from_state(State),
    {user_perk,Score2,_Avia2,_Brone2,_Soldier2,_Bet,_Profile2}=User2,
    

Name = <<"battle_serv">>,
Ali=gproc:lookup_local_name(Name),
%%   1 - поединок  2 - дуэль 3 - турнир
Battle_type = 2,
    First_turn =case abs(Score1-Score2) of
	   Delta when Delta>499 -> 
	       %% разница в 500 или больше рейтинга ходит тот у кого меньше рейтинга
	       Score1<Score2;
	   _ ->random:uniform(2)==1
       end,
    

case {First_turn,is_pid(Ali)} of
	{true, true}  ->
	%% первым ходит user1
	gen_server:cast(Ali, {new,  Jid1, User1,Jid2,User2,?DUEL_SCORE_ADD,Battle_type,0});
	{_,true} ->
	%% первым ходит user2

	gen_server:cast(Ali, {new,  Jid2, User2,Jid1,User1,?DUEL_SCORE_ADD,Battle_type,0});
	{_,false} ->
	%% нужный нас сервер не запущен :(
	   Text= make_json_duel_deny(5),
		send_msg(Jid1, Text),
		send_msg(Jid2, Text),
	  (catch match_battle:start_link())
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec get_last_duel_time(State)  -> integer() = timestamp 
%% @end
%%--------------------------------------------------------------------
get_last_duel_time(State)  ->  
    case State#state.duel_time of
       undefined -> 0;
	_ -> State#state.duel_time
	    
		end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
debug_send_me_state(Turn,Jid)->
    
%%Name = "scas@im.xlab.su/tester",
%%    NN = binary_to_list(Name),
%%    NN = jlib:string_to_jid(Name),
%%    Bin1 = list_to_binary([Jid, io_lib:format(" : ~p", [Turn])])
%%,send_msg(NN,Bin1).
{Turn,Jid}.


%%--------------------------------------------------------------------
%% @doc функция иницирования тймера конца хода 
%% @spec init_cron_timer_battle(State,_Value,_FlagNB) -> #state{}
%% @end
%%--------------------------------------------------------------------
init_cron_timer_battle(State,Value,FlagNB) when is_integer(Value) andalso  Value==1 ->
Pid4pro = self(),
    Interval =case FlagNB  of
		  0 -> stop_turn_timer(State),29 ; %% не первый ход в битве
		  _ -> 34  end,
%% запомнить время когда запустили таймер
{Mega, Seconds, _} = erlang:now(),
 {ok,Ref} =  libs_functions:wrap_cron_once(Interval, {gen_server, cast, [Pid4pro, timeout_turn]}),
%% ?INFO_MSG("libs_functions:wrap_cron_once   ~p  : ~p	~n", [State#state.jid,Ref]),	 
 State#state{ref_turn_timer=Ref,timer_start_time={FlagNB, Mega * 1000000 + Seconds}};
init_cron_timer_battle(State,_Value,_FlagNB) ->
State.

%%--------------------------------------------------------------------
%% @doc Функция когда сработал таймер и отсылается на сервер боя неправильный ход
%% @spec send_battle_serv_skip(State) ->ok.
%% @end
%%--------------------------------------------------------------------
send_battle_serv_skip(State) ->

First_user=jlib:string_to_jid(State#state.jid),
%%N = {struct, [{<<"go">>,{struct,[{<<"x0">>,0},{<<"y0">>,0},{<<"x1">>,8},{<<"y1">>,8}]}}]},
gen_server:cast(State#state.battle_serv, {skip, State#state.battle_id,First_user,
	State#state.player0,State#state.player1}),ok.

%%--------------------------------------------------------------------
%% @doc Функция сброса таймера на ход в бою
%% @spec  stop_turn_timer(State) -> #state{}.
%% @end
%%--------------------------------------------------------------------
stop_turn_timer(State = #state{ref_turn_timer={_,R}}) when is_reference(R)->
    libs_functions:wrap_cron_cancel(State#state.ref_turn_timer),
    State;
stop_turn_timer(State) ->
State.

%%--------------------------------------------------------------------
%% @doc откат хода у клиента при ходе с таймаутом
%% @spec	 send_timeout_to_turn(First_user,N) -> ok.
%% @end
%%--------------------------------------------------------------------

send_timeout_to_turn(User,{struct, [{<<"go">>,M}]})->
%% требуется откатить ход клиенту

{struct,[{<<"x0">>,X0},{<<"y0">>,Y0},{<<"x1">>,X1},{<<"y1">>,Y1}]} = M,
Text= mochijson2:encode({struct,[{<<"reply">>,
			{struct,[{<<"type">>,100},
			{<<"body">>,
			[{struct,[{<<"turn">>,
			   {struct,[
				{<<"player">>,3},
				{<<"go">>, {struct,[{<<"x0">>,X0},{<<"y0">>,Y0},{<<"x1">>,X1},{<<"y1">>,Y1}]}}
				] }}
			]}
			]
			}]}}]}),
    send_msg(User, Text );
send_timeout_to_turn(_User,_N)->
%% требуется откатить ход клиенту
ok.
    
%%--------------------------------------------------------------------
%% @doc	 инициализация слотов профиля при становлении онлайн игрока
%% @spec get_base_slots(Thing_id) -> array().
%% @end
%%--------------------------------------------------------------------
get_base_slots(Things_id) when is_list(Things_id) ->  
libs_functions:get_base_slots(Things_id).

%%--------------------------------------------------------------------
%% @doc выдает описание слота в формате совместимым с  mochijson2:encode для получения json элемента
%% @spec  get_slot(Num,SlotNum,State) -> tuple().
%% @end
%%--------------------------------------------------------------------
get_slot(Num,SlotNum,State) ->
    Array = (State#state.thing_change_param)#thing_change_param_rec.slots,
%%    ?INFO_MSG("alot : ~p~n", [1]),
get_slot_in_array(Num,SlotNum,Array).

%%--------------------------------------------------------------------
%% @doc вспомогательная функция дляописания слотов
%% @spec
%% @end
%%--------------------------------------------------------------------
get_slot_in_array(Num,SlotNum,Array) ->
    Slot = array:get(Num, Array),
    Bonus = Slot#slot.thing,
%%    ?INFO_MSG("SlotNum : ~p~n", [SlotNum]),
    Kd =case Num of
	     A when A < 3 -> ?KD_GROUP_0;
	     B when B > 2,B < 6 -> ?KD_GROUP_1;
	     _ -> ?KD_GROUP_2
	end,
    {list_to_binary(io_lib:format("slot~p", [SlotNum])),
	    {struct,[
		{  "num",    Slot#slot.count },
		{ "name",    libs_functions:base64_encode(Bonus#thing.name) },
		{"descr",    libs_functions:base64_encode(Bonus#thing.descr) },
		{  "img",    Bonus#thing.img },
		{ "type",    Bonus#thing.type },
		{   "id",    Bonus#thing.id },
		{   "kd",    Kd },
		{ "slot",    Slot#slot.num }
	    ]}
}.    

%%--------------------------------------------------------------------
%% @doc	 отдает активен ли статус премиума
%% @spec get_premium_state(State) -> tuple().
%% @end
%%--------------------------------------------------------------------
get_premium_state(State) ->
Thin_con= State#state.thing_change_param,
get_premium_status_date(Thin_con#thing_change_param_rec.premium).

%%--------------------------------------------------------------------
%% @doc Функция извленения даты и времени в нужном формате из статуса премиума
%% @spec get_premium_status_date({Pstat, Pdate}) -> {Pstat, Pdate}.
%% @end
%%--------------------------------------------------------------------
get_premium_status_date({Pstat, PTimestamp}) ->
    case Pstat of
	1 ->	Datetime_bin = libs_functions:date_to_print(calendar:gregorian_seconds_to_datetime(PTimestamp)),
	     {Pstat, Datetime_bin};
	_ ->
	    Datetime = lists:flatten([" "]),
	    {Pstat, list_to_binary(Datetime)}
end.


%%--------------------------------------------------------------------
%% @doc активирует соотвествующий слот пользователя 
%% @spec activate_user_slot(SlotNum,State,Slot_param_arr) ->	 {[list to send],#state{}}.
%% @end
%%--------------------------------------------------------------------
activate_user_slot(SlotNum,State = #state{ stat_bonus_count = OldCount, premium_in_battle=Pstat },Slot_param_arr) ->
%% определить сколько вещей в слоте
    Array = (State#state.thing_change_param)#thing_change_param_rec.slots,
    Slot = array:get(SlotNum, Array),
%%    ?INFO_MSG("alot : ~p~n", [2]),


    case {Slot#slot.count,Pstat} of
	{M,N} when M + N > 0 ->	 %% уменьшить число вещей в слоте
		   StateZ = stop_turn_timer(State), %% остановили таймер таймаута хода
		   New_state=dec_thing_count(StateZ,SlotNum),
		   Bonus = Slot#slot.thing,
		   First_user=jlib:string_to_jid(New_state#state.jid),
		   %% TODO по хороему стоит упоковать уменьшение числа вещей в слоте в функцию активации слота
		  Fun=Bonus#thing.apply_thing,
%%		     ?INFO_MSG(" fun : ~p~n", [erlang:fun_info(Fun)]),
%% выяснись что анонифункоторые созданы лишь как fun ... end являются локальными при их вызове пропроверка

		  Fun(Slot_param_arr,  New_state#state.battle_serv,New_state#state.battle_id,
				    First_user, New_state#state.player0, New_state#state.player1),
	     %% чел использовал слот
%% 97- количество использованных бонусов слот0
%% 98- количество использованных бонусов слот1
%% ... 
%% 106- количество использованных бонусов слот8


	    Msg_list1 =	 lists:append([],[{97+SlotNum,1,add}]),
	    msg_self_change_stat(Msg_list1),
	      {[],New_state#state{stat_bonus_count = (OldCount+1) }};
	{_M1,_N1} ->
	    %%	error  + профиль
	      ?INFO_MSG(" error profile : ~p~n", [1]), %% NewState
	    Me = self(),
	    ToM =  jlib:string_to_jid( ?SEND_FROM),
	    From =   get_jid_from_state(State),
	    Body = <<"{\"query\":1}">>,
	   gen_server:cast(Me, {query_type, ToM, From, Body}), %% в майлбокс запрос как бы от игрока профиля
	  %%   Result = mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,122}, {<<"body">>,{struct,[{<<"error">>,
	  %%	      {struct,[{<<"type">>,<<"invalid profile">>},{<<"val">>,1596}]}}]} }]}}]}),
	    Result = [],
	       {Result,State}
		end.


%%--------------------------------------------------------------------
%% @doc Функция уменьшения кол-ва вещей в слоте
%% @spec dec_thing_count(State,Num) -> #state{}
%% @end
%%--------------------------------------------------------------------
dec_thing_count(State=#state{premium_in_battle=Pstate},Num) -> 
%% Если не премиум статус то уменьшаем кол-во вещей в слоте
    case Pstate of
	M when M>0 ->State;
	_ ->	 
	    change_thing_count(State,Num,-1)
end.
%%--------------------------------------------------------------------
%% @doc Функция изменения кол-ва вещей в слоте
%% @spec change_thing_count(State,Num,Value) -> #state{}
%% @end
%%--------------------------------------------------------------------
change_thing_count(State=#state{ thing_change_param=Pstate},Num,Value) ->
   Array = (Pstate)#thing_change_param_rec.slots, %% Берем массив слотов
		Slot = array:get(Num, Array),					      %%   берем слот
		NewSlot = Slot#slot{count = Slot#slot.count+Value},		      %% меняем кол-во
		 ArrayNew = array:set(Num, NewSlot, Array),			     %% собираем новый массив слота
		NN = State#state.thing_change_param,
		MM = NN#thing_change_param_rec{slots = ArrayNew},
		%% Сообщение о изменении числа вещей в слоте
		StateNew= State#state{thing_change_param=MM},
diff_state(State,StateNew).

%%--------------------------------------------------------------------
%% @doc функция анализа и оповещений о изменениях в данных игрока State#state.thing_change_param
%% @spec diff_state(StateOld,StateNew) -> StateNew#state{}
%% @end
%%--------------------------------------------------------------------
diff_state(State,StateNew) -> 

    User=jlib:string_to_jid(State#state.jid),
    diff_tuple( <<"money_0">> ,(State#state.thing_change_param)#thing_change_param_rec.money_0,
	       (StateNew#state.thing_change_param)#thing_change_param_rec.money_0,User),
  %%   ?INFO_MSG("AAA : ~p~n", [2]),
    diff_tuple(<<"money_1">>,(State#state.thing_change_param)#thing_change_param_rec.money_1,
	       (StateNew#state.thing_change_param)#thing_change_param_rec.money_1,User),
%%    ?INFO_MSG("AAA : ~p~n", [4]),
    diff_tuple(<<"ava">>,(State#state.thing_change_param)#thing_change_param_rec.ava,
	       (StateNew#state.thing_change_param)#thing_change_param_rec.ava,User),
    diff_tuple(<<"premium">>,(State#state.thing_change_param)#thing_change_param_rec.premium,
	       (StateNew#state.thing_change_param)#thing_change_param_rec.premium,User),
    %%	?INFO_MSG("AAA : ~p~n", [5]),
    diff_tuple(<<"slot">>,(State#state.thing_change_param)#thing_change_param_rec.slots,
	       (StateNew#state.thing_change_param)#thing_change_param_rec.slots,User),

 StateNew 
.

%%--------------------------------------------------------------------
%% @doc сравнивает характеристики и если они изменились оповещает клиента об изменении
%% @spec diff_tuple(<<"query">>,A,B,User) ->ok
%% @end
%%--------------------------------------------------------------------
 diff_tuple(_, A,A,_User) ->
    ok;
diff_tuple(<<"money_0">>,_A,B,User) ->

   Result=get_profile_change_msg(<<"money_0">>, B),
   send_msg(User,  Result);
diff_tuple(<<"money_1">>,_A,B,User) ->

    Result=get_profile_change_msg(<<"money_1">>, B),
    send_msg(User,  Result);
diff_tuple(<<"premium">>,_A,B,User) ->
%% попало значит в результате паттерн матчинга данные в слоте разные 
%% надо изменить статистику 
%% 86- Общее количество купленных режимов премиум.
    msg_self_change_stat([{{86,1,add}}]),
    Result=get_profile_change_msg(<<"premium">>, B),
    send_msg(User,  Result);
diff_tuple(<<"ava">>,_A,B,User) ->
    Result=get_profile_change_msg(<<"img">>, B),
    send_msg(User,  Result);

diff_tuple(<<"slot">>,A,B,User) ->

%% раз попало в данную точку то массивы слотов явно отличаются
Slot0 =	   get_slots_change_msg(0, A, B),
Slot1 =	   get_slots_change_msg(1, A, B),
Slot2 =	   get_slots_change_msg(2, A, B), 
    All_slot =lists:append([Slot0,Slot1,Slot2]),
    

%%?INFO_MSG("get_slots_change_msg_1 : ~p~n", [All_slot]),

Result=mochijson2:encode({struct,[{<<"reply">>,
     {struct,[{<<"type">>,1},
      {<<"body">>,
{struct,[{<<"game_window">>,{struct,[{<<"player">>,{   struct,[ 

{<<"razdel2">>,{struct,[
{<<"name">>,  things:get_razdel_name (2)},
{<<"descr">>, things:get_razdel_descr(2)},
{<<"list">>,{struct,
		     All_slot
	    }}
]}}
   ]}}]	  }}]}}]}}]}),
    send_msg(User,  Result);
diff_tuple(_, _QA,_DA,_User) ->
ok.

%%--------------------------------------------------------------------
%% @doc Сообщения об изменении
%% @spec  get_profile_change_msg(<<"money_1">>, B) -> binary().
%% @end
%%--------------------------------------------------------------------
get_profile_change_msg(<<"premium">>, B) -> 
    {Pstat, Pdate}=get_premium_status_date(B),
mochijson2:encode({struct,[{<<"reply">>,
     {struct,[{<<"type">>,1},
      {<<"body">>,
{struct,[{<<"game_window">>,{struct,[{<<"player">>,{   struct,[ 
{<<"premium">>,	       Pstat},
{<<"premium_date">>,   Pdate}
   ]}}]	  }}]}}]}}]});
get_profile_change_msg(Bin_str, B) when is_binary(Bin_str) -> 

mochijson2:encode({struct,[{<<"reply">>,
     {struct,[{<<"type">>,1},
      {<<"body">>,
{struct,[{<<"game_window">>,{struct,[{<<"player">>,{   struct,[ {Bin_str, B}   ]}}]   }}]}}]}}]});
get_profile_change_msg(_, _B) ->
[].

%%--------------------------------------------------------------------
%% @doc вспомогательная функция сбора данных об изменении слота в профиле
%% @spec     get_slots_change_msg(2,A, B) ->tuple()
%% @end
%%--------------------------------------------------------------------
get_slots_change_msg(T,Array1,Array2) when is_integer(T) -> 
%% ?INFO_MSG("get_slots_change_msg_1 : ~p~n", [T]),
    A = T*3, %% 0 0   1	 3   2	6

%%    Array1 = (State#state.thing_change_param)#thing_change_param_rec.slots,
%%    Array2 = (NewState#state.thing_change_param)#thing_change_param_rec.slots,
    S1 = (array:get(A, Array1)),
    S2 = (array:get(A+1, Array1)),
    S3 = (array:get(A+2, Array1)),

    S4 = (array:get(A,	 Array2)),
    S5 = (array:get(A+1, Array2)),
    S6 = (array:get(A+2, Array2)),
   Slot20 = case S1 of S4 -> [];
		 _ -> [get_slot_in_array(A,0,Array2)] end,
   Slot21 = case S2 of S5 -> [];
					  _ -> [get_slot_in_array(A+1,1,Array2)] end,
   Slot22 = case S3 of S6 -> [];
					  _ -> [get_slot_in_array(A+2,2,Array2)] end,

    All = lists:append([Slot20,Slot21,Slot22]),
    Kd =case T of
	     0 -> ?KD_GROUP_0;
	     1 -> ?KD_GROUP_1;
	    _ -> ?KD_GROUP_2
	end,
%% тут идет описание конкретной вещи или слота	  
    
case All of
	[] -> [];
	M ->	 All_slot = M,
[{list_to_binary(io_lib:format("group~p", [T])) ,{struct,[
{<<"name">>,  things:get_group_name (2, T)},
{<<"descr">>, things:get_group_descr(2, T)},
{<<"kd">>,  Kd},
{<<"list">>,{struct,
All_slot
 }}
 ]}}]
end;
get_slots_change_msg(_A,_State,_NewState) -> 
[]
.

%%--------------------------------------------------------------------
%% @doc Отправить себе запрос для запроса статистки через какое то время
%% @spec  get_achiv_after(Type, State) -> ok.
%% @end
%%--------------------------------------------------------------------
get_achiv_after(Type) ->
%%?INFO_MSG("get_achiv_after : ~p~n", [2]),
  Time = 3000,
get_achiv_after(Type,Time).

get_achiv_after(Type,Time) ->
  Msg = {statistic ,  Type},
  Pid = self(),
  erlang:send_after(Time, Pid, Msg)
.

%%--------------------------------------------------------------------
%% @doc Запрос на проверку получения достижения. Type - тип события
%% @spec  get_achiv(Type, State) -> ok.
%% @end
%%--------------------------------------------------------------------
get_achiv(Type, _State=#state{jid_bin=Ajid,statistics=DB}) ->
%%?INFO_MSG("get_achiv : ~p~n", [Ajid]),
 get_achiv0(Ajid, DB, Type).

get_achiv0(A,B,Type) ->
    Name = <<"achiv">>,
    Pid4pro=gproc:lookup_local_name(Name),
    UserParams = #userparams{	
		    jid		 = A
		  , statistics	 = B
		},

    case (is_pid(Pid4pro)) of
	true  ->
       %% отправить запрос туда
	libs_functions:wrap_cron_once(3, {gen_server, cast, [Pid4pro , {if_achiv, Type, UserParams}]});

	_ ->
	%%запустить нужный процесс
	{ok,Pid} = achiv:start_link(),
	libs_functions:wrap_cron_once(3, {gen_server, cast, [Pid ,{if_achiv, Type, UserParams}]})

    end,
ok
.


dell_achive_event(State, Num) ->
if ((is_integer(Num)) and (Num>0) and (Num=<10)) ->
    Dell_list = achiv_info:get_dell_list(Num),
    Getted = State#state.achiv,
    NewGetted = achiv:del_achiv(Getted, Dell_list),
    State#state{ achiv = NewGetted};
true ->
    State
end.

%%--------------------------------------------------------------------
%% @doc	 функция получения достижений
%% @spec set_achiv(State, UserParams) -> state{}
%% @end
%%--------------------------------------------------------------------
set_achiv(State = #state{ achiv = Getted}, UserParams=#userparams{new_statistics = List 
					, money_0      = AddM
					, new_bonuses  = Slots
					, new_achive  = NA
					, dell_achive  = DA
				       }) ->
%% функция применения достижений к профилю #userparams
%%	jid		    %% идентификатор пользователя     , statistics	    %% статистика      
%%    , money_0 = 0	    %% монеты 
%%    , new_statistics = [] %% список изменения статистики [{id, +-add_val} ]
%%    , new_bonuses = []    %% список полученых бонусов	     [{id, add_val}]
%%    , new_achive  = []    %% список полученых достижений о которых следует известить пользователя  [{id,_date,val}]
%%    , dell_achive  = []   %% список удаляемых достижений [{id,val}]
X1 = add_money_0(State, AddM,notify),
%%?INFO_MSG("list add [~p] == ~p ~n ", [State#state.jid,List]),
msg_self_change_stat(List),

P8 = fun(A, AccIn) -> 

	     case A of
	       {Num,Val} ->  change_thing_count(AccIn,Num,Val) ;
		 _ ->AccIn		     
	       end %% case
	 end, %% fun

X2 = lists:foldl(P8, X1 , Slots), %% изменяем кол-во вещей в слотах

%%  Getted
NewGetted0 = achiv:add_achiv(Getted,NA),
NewGetted =  achiv:del_achiv(NewGetted0,DA),

%% проверка того что дали новое звание	
    {Rank_id_OLD, _, _} = Getted#getted.rank,
    {Rank_id, _, _}  = NewGetted#getted.rank,


case Rank_id_OLD==Rank_id of
    true -> ok;
    _ -> %% звание сменилось
   Achiv_info = achiv_info:show_info(Rank_id, 1),
   User_new_rang = libs_functions:base64_encode(Achiv_info#achiv.name),
   User_new_lrang = libs_functions:base64_encode(Achiv_info#achiv.sname),
   UserS=jlib:string_to_jid(State#state.jid),
 Text_new_rang=mochijson2:encode(
				  {struct,[{<<"reply">>,
					    {struct,[{<<"type">>,1},
						     {<<"body">>,
						      {struct,[{<<"game_window">>,{struct,[{<<"player">>,{
									struct,[
										{<<"rang">>,	   User_new_rang},
										{<<"lrank">>,	   User_new_lrang} ]}}]	  }}]}}]}}]}), 
	send_msg( UserS, Text_new_rang),
	libs_functions:wrap_cron_once(3, {libs_functions, send_log_to_user, [UserS, 3, {NewGetted#getted.rank}, "#000080"]})
	    end,
%% проверка того что дали новое звание	done

New_Achivs_list = UserParams#userparams.new_achive,

%% известить игрока о новых ачивках

State_out =    case New_Achivs_list of

	[] -> X2;
	_ ->
	   % TCP = State#state.thing_change_param,
	    User=jlib:string_to_jid(State#state.jid),
	    LLR1 = <<"{ \"reply\": { \"type\": 45, \"body\": { \"achivs\": [">>,
	    LLR2 = <<"]} } }">>,
	case (New_Achivs_list) of
	    [] -> Achivs_info0 = [];
	     _ -> Achivs_info0 = [ achiv_info:show_info(Achiv_id, 3)  || {Achiv_id, _ADate, _AQnnty} <- New_Achivs_list],
		  set_log(User, 1, {New_Achivs_list})
	end,
	    Achivs_info	 = libs_functions:p8_fun(Achivs_info0),
	    Achivs_send = list_to_binary([LLR1, Achivs_info, LLR2]),
	    send_msg(User,  Achivs_send),
		       X2

    end,

State_out#state{ achiv = NewGetted }
.



%%--------------------------------------------------------------------
%% @doc Функция проводящая иницирование статистики из хранилища
%% @spec init_user_statistics(Jid)-> #statistics{}
%% @end
%%--------------------------------------------------------------------
init_user_statistics([]) ->
%%у чела новый профиль значит и статистика по 0
Stat_day    = array:new([{size, 20}, {default, 0}, {fixed, true}]),
Stat_week   = array:new([{size, 20}, {default, 0}, {fixed, true}]),
Stat_moon   = array:new([{size, 20}, {default, 0}, {fixed, true}]),
Stat_always = array:new([{size, ?STATISTICS_PARAM_COUNT - 60}, {default, 0}, {fixed, true}]),
#statistics{stat_day=Stat_day
  , stat_week = Stat_week
  , stat_moon = Stat_moon
  , stat_always = Stat_always
}
.
    
%%--------------------------------------------------------------------
%% @doc функция изменения статистики игрока
%% @spec change_user_stat(State,Num, Value) -> state{}
%% @end
%%--------------------------------------------------------------------

change_user_stat(State = #state{statistics = DB}, Num, Value,Type) ->
 Counter = DB#statistics.counter + 1,
%%?INFO_MSG("change reit: ~p~n", [Counter]),
    Get_change_value = fun(Slotl,A,Typel) -> 
		 %% анонимная функция получения на сколько сменится значение в зависимости от типа
		    case Typel of
			  add -> Slotl+A; %% добавление
			  min  -> erlang:min(Slotl,A); %% 
			  min_no_zero when A > 0,Slotl>0 -> erlang:min(Slotl,A); %% отрезаем нули ибо по умолчанию у нас как раз нули
			  min_no_zero when Slotl==0 -> A;
			  min_no_zero  -> Slotl;
			  max -> erlang:max(Slotl,A);
			   _ -> A  end
	 end, %% fun
%%?INFO_MSG("change reit: [~p] ~p ~p~n ", [Num,Type,Value]),
    NewDB  =case Num of
		  A when A < 20 ->		   
		      Array = DB#statistics.stat_day, 
		      Slot = array:get(Num, Array),   
		      ChangeVal = Get_change_value(Slot,Value,Type),	    
		      DB#statistics{stat_day=array:set(Num, ChangeVal, Array)	    , counter=Counter};
		  B when B > 19 , B <40 -> 
		      Array = DB#statistics.stat_week,
		      Slot = array:get(Num-20, Array),		 
		    ChangeVal =Get_change_value(Slot,Value,Type),	 
		      DB#statistics{stat_week=array:set(Num-20, ChangeVal, Array)   , counter=Counter};
		  C when C > 39 , C < 60 -> 
		      Array = DB#statistics.stat_moon,
		      Slot = array:get(Num-40, Array),	   
		      ChangeVal =Get_change_value(Slot,Value,Type),		 
		      DB#statistics{stat_moon=array:set(Num-40, ChangeVal, Array)   , counter=Counter}
		    ;
		  _D  -> 
		      Array = DB#statistics.stat_always,
		      Slot = array:get(Num-60, Array),		 
		      ChangeVal =Get_change_value(Slot,Value,Type),
		      DB#statistics{stat_always=array:set(Num-60, ChangeVal, Array) , counter=Counter}
		end,
    New_state = State#state{statistics=NewDB},
    %if (Num == 7) -> vkapi:setUserLevel(get_jid_from_state(New_state), get_score(New_state));
%	 true -> ok
    %end,
New_state.
%%--------------------------------------------------------------------
%% @doc функция которая вычисляет часть нужных параметров статистики при проигрыше или выйгрыше 
%% @spec stat_local_param_stat()-> [{},..]
%% @end
%%--------------------------------------------------------------------
stat_local_param_add_list( AccIn,Btype,Result,If_win_add) when is_list(AccIn) , is_list(If_win_add)->
%% Поединок {1,Num}
%% Дуэль    {2,Num}
%% Турнир   {3,Num}
%%
%%
    {Aw,Anw} =case Btype of %% в зависимости от типа боя у нас разные параметры добавляются
		  {1,_} ->    {[{5,1,add}, {92,1,add}],[{6,1,add},{92,0,set}]};
		  {2,_} ->   {[{3,1,add}],[{4,1,add}]};
		  {3,Num} ->
		      {Stat_win,Stat_lose} =case Num of
			N  when N  =< 16 -> {8,9} ;	     %% 1/8
			N1 when N1 >= 17, N1 =< 24 -> {-1,10}; %% 1/4
			N2 when N2 >= 25, N2 =< 28 -> {-1,11}; %% 1/2 
			_ -> {12,13}	end,			  %% финал
		      {[{Stat_win,1,add},{93,1,add}],[{Stat_lose,1,add},{93,0,set}]}
				 end,

    case Result of
	1 -> Add =  lists:append(Aw,If_win_add),
	     lists:append(AccIn,Add) ;
	_ -> %% проигрыш
	    lists:append(AccIn,Anw)
    end.


%%--------------------------------------------------------------------
%% @doc Функция расчета параметров статистики после конца битвы
%% @spec stat_end_battle(State,Result,Battle_mode) -> ok
%% @end
%%--------------------------------------------------------------------
stat_end_battle(_State = #state{jid=Jid, battle_type=1,stat_bonus_count =Count, stat_turn_count = Turns, premium_in_battle=Pstat, 
		enemyFio =Enemyfio  },Result,
		Battle_mode,Add_p) -> 
%% Завершился ПОЕДИНОК
%% Battle_mode
%% 0 нормальная победа
%% 1 победа изза ошибки 
%% 2 победа в результате выхода
%% 3   время боя вышло должно приходить как 0
    AA =case Result  of
	    1 -> lists:append([],[{89,1,set}]);	 %% Победа в поединке
	    _ ->[{89,0,set}]
		    end,

   Msg_list00 = lists:append(AA,[{14,Count,set} %% Количество бонусов, использованных в игре за крайнюю битву
    , {90,Count,max}  %% максимум бонусов за битву
				]), 
    Msg_list0 =case Battle_mode of
		   0 when Result==1 -> lists:append(Msg_list00,[{15,Turns,min_no_zero}	
								     %% Количество ходов // Наименьшее кол-во ходов при победе
					, {17,1,add} %% Кол-во нормальных побед в Поединке
					, {80,Turns,set}  %% кол-во ходов за последнию битву
					, {83,Turns,max}  %% max кол-во ходов 
				       ]);
		   _S  -> lists:append(Msg_list00,[						
					 {80,Turns,set}	 %% кол-во ходов за последнию битву
					, {83,Turns,max}  %% max кол-во ходов 
				       ])
	       end,

			

%%92- Количество игр выигранных подряд в поединке,
%%  (3 счетчика при проигрыше сбрасывается)

%%88- Общее количество побед в поединках, находясь под премиумом.

    Add_list0 =case Pstat of
		  Am when Am >0 ->lists:append([],[{88,1,add}]) ;
		  _ ->	[]  end,

    IFWINAdd_list =case Turns of
		  An when An ==0 -> lists:append([{91,1,add}],Add_list0) ; %% 91- Победы в сухую (без передачи хода противнику).
		  _ ->	Add_list0  end,
 %% по результатам битвы и типа битвы (1) запрашиваем список параметров в статистике которае надо изменить
    Msg_list1 =	 stat_local_param_add_list(Msg_list0,{1,0},Result,IFWINAdd_list),
    
   msg_self_change_stat(Msg_list1),
   libs_functions:wrap_cron_once(4, { libs_functions, send_log_end_battle, [Jid, 1,Battle_mode,Result,Add_p,Enemyfio,Pstat]});
%%   libs_functions:send_log_end_battle(Jid, 1,Battle_mode,Result,Add_p,Enemyfio,Pstat);


stat_end_battle(_State = #state{jid=Jid,battle_type=2,	premium_in_battle=Pstat, stat_bonus_count =Count, stat_turn_count = Turns, enemyFio =Enemyfio },
		Result, Battle_mode,Add_p) -> 
%% Battle_mode
  %% 0 нормальная победа
  %% 1 победа изза ошибки 
  %% 2 победа в результате выхода
  %% 3	 время боя вышло
    AA =case Result  of
	    1 -> lists:append([],[{89,2,set}]);	 %% Победа в Дуэль
	    _ ->[{89,0,set}]
		    end,

   Msg_list00 = lists:append(AA,[{14,Count,set} %% Количество бонусов, использованных в игре за крайнюю битву
, {90,Count,max}  %% максимум бонусов за битву
]), 

    Msg_list0 =case Battle_mode of
		   0 when  Result==1 -> lists:append(Msg_list00,[
					%% {15,Turns,min_no_zero},   %% Количество ходов // Наименьшее кол-во ходов при победе
					 {16,1,add} %% Кол-во нормальных побед в Поединке
					,  {80,Turns,set}  %% кол-во ходов за последнию битву
					, {83,Turns,max}  %% max кол-во ходов 
				       ]);
		   _  -> lists:append(Msg_list00,[						
					 {80,Turns,set}	 %% кол-во ходов за последнию битву
					, {83,Turns,max}  %% max кол-во ходов 
				       ])
	       end,
    



%%?INFO_MSG(" Дуэль add stat : ~p~n", [Turns]),
    Add_list0 =[],
    IFWINAdd_list =case Turns of
		  A when A ==0 ->lists:append([{91,1,add}],Add_list0) ; %% 91- Победы в сухую (без передачи хода противнику).
		  _ ->	Add_list0  end,
 Msg_list1 =  stat_local_param_add_list(Msg_list0,{2,0},Result,IFWINAdd_list),
%% Дуэль

?INFO_MSG(" Дуэль add stat : ~p~n", [Msg_list1]),
   msg_self_change_stat(Msg_list1),
   libs_functions:wrap_cron_once(4, { libs_functions, send_log_end_battle, [Jid, 2,Battle_mode,Result,Add_p,Enemyfio,Pstat]});
%%   libs_functions:send_log_end_battle(Jid, 2,Battle_mode,Result,Add_p,Enemyfio,Pstat);


stat_end_battle(State = #state{jid=Jid, battle_type=3, tour_num = Num, stat_bonus_count =Count, stat_turn_count = Turns, 
			       premium_in_battle=Pstat, enemyFio =Enemyfio },
		Result, Battle_mode,Add_p) -> 
%%  8- Количество побед 1/8 турнира
%%  9- количество проигрышей 1/8 в турнире
%% 10- количество проигрышей 1/4 в турнире
%% 11- количество проигрышей 1/2 в турнире
%% 12- кол-во побед 1/1 в турнире(финале)
%% 13- количество проигрышей 1/1 в турнире(финале)
    AA =case Result  of
	    1 when Num>28 -> lists:append([],[{89,4,set}]);  %% Победа в финале
	    1 -> lists:append([],[{89,3,set}]);	 %% Победа 
	    0 when Num>28, Battle_mode==0 -> lists:append([],[{89,0,set},{18,1,add}]);	%% нормальный проигрыш в финале 
	    _ ->[{89,0,set}]
		    end,

 Msg_list00 = lists:append(AA,[{14,Count,set}  %% Количество бонусов, использованных в игре за крайнюю битву
, {90,Count,max}  %% максимум бонусов за битву
]),

    Msg_list0 =case Battle_mode of
		   0 when Result==1 -> lists:append(Msg_list00,[
				%%{15,Turns,min_no_zero},
								     %% Количество ходов // Наименьшее кол-во ходов при победе
					 {80,Turns,set}	 %% кол-во ходов за последнию битву
					, {83,Turns,max}  %% max кол-во ходов 
				       ]);
		   _  -> lists:append(Msg_list00,[						
					 {80,Turns,set}	 %% кол-во ходов за последнию битву
					, {83,Turns,max}  %% max кол-во ходов 
				       ])
	       end,
%%87- Общее количество побед в турнирах, находясь под премиумом.

    Add_list0 =case Pstat of
		  At when At >0 -> [{87,1,add}] ;
		  _ ->	[]  end,
    IFWINAdd_list =case Turns of
		  Ay when Ay ==0 ->lists:append([{91,1,add}],Add_list0) ; %% 91- Победы в сухую (без передачи хода противнику).
		  _ ->	Add_list0  end,	   

%% турнир
Msg_list1 =  stat_local_param_add_list(Msg_list0,{3,Num},Result,IFWINAdd_list),
?INFO_MSG(" ~p турнир add stat : ~p ~n", [State#state.jid,Msg_list1]),
msg_self_change_stat(Msg_list1),
 libs_functions:wrap_cron_once(4, { libs_functions, send_log_end_battle, [Jid, 100+Num,Battle_mode,Result,Add_p,Enemyfio,Pstat]});
%%   libs_functions:send_log_end_battle(Jid, 100+Num,Battle_mode,Result,Add_p,Enemyfio,Pstat);



stat_end_battle(_State,Result, _Battle_mode,_Add_p) -> 
?INFO_MSG("  : Alarm! ~p~n", [Result]),
   ok.
%%--------------------------------------------------------------------
%% @doc	 сообщение себе для изменения парметров статистики
%% @spec msg_self_change_stat([]) ->	ok;
%% @end
%%--------------------------------------------------------------------
msg_self_change_stat([]) ->
    ok;
msg_self_change_stat(List) ->
	libs_functions:wrap_cron_once(1, {gen_server, cast, [self(), {add_stat,List}]}).
%%--------------------------------------------------------------------
%% @doc функция по корректировке стейта при завершении битвы
%% @spec set_state_at_endbattle(State) -> #state{}
%% @end
%%--------------------------------------------------------------------
set_state_at_endbattle(State,Log) -> 
?INFO_MSG(" set_state_at_endbattle  :  ~p ~p~n", [State#state.jid,Log]),
test_change_status(State, 0,Log),
State#state{route_to = 0
       , battle_id = 0
       , battle_serv=0
       , stat_change_in_turn=0
       , premium_in_battle = 0
       , transaction_state = 0
       , battle_invite_msg = 0
       , tour_ref_bet_timer = 0
       , bet=0
       , enemyFio =  "звание Имя Фамилия"
       , battle_type=0}.


get_stat_val(Num, DB) ->

case Num of
	  A when A < 20 ->		   
			Array = DB#statistics.stat_day, 
			array:get(Num, Array);

	  B when B > 19 , B <40 -> 
			Array = DB#statistics.stat_week,
			array:get(Num-20, Array);

	  C when C > 39 , C < 60 -> 
			Array = DB#statistics.stat_moon,
			array:get(Num-40, Array);

	  _D  -> 
			Array = DB#statistics.stat_always,
			array:get(Num-60, Array)
	end
.

get_stat_summ(DB, List_stst_param) ->
    List_stst_val = [get_stat_val(Num, DB) || Num <- List_stst_param], 
    lists:sum(List_stst_val)
.
%%--------------------------------------------------------------------
%% @doc Функция сохранения профиля в Redis
%% @spec save_user_state(SaveState) -> ok
%% @end
%%--------------------------------------------------------------------
save_user_state(State = #state{link_to_redis=C
			       , jid	    = P00
			       , jid_bin    = P01
			       , firstName  = P02
			       , lastName   = P03
			       , middleName = P04
			       , sn_id	    = P05
			       , reg_time   = P06
			       , achiv	    = P07
			       , msg_send   = P08
		%	       , user_param = P09
			       , is_admin   = P10

			       , last_init_time = P11
			       , last_enter_day = P12
			       , everyday_count = P13
			       , profile_link	= P14
			       , user_data	= P140

			       , tran_number	= P15

			       , statistics	= P16
			       , buy_kredits = P17
			       
			       , thing_change_param = C01 
			      }) ->
    P09 = #perk{}, %% для синхронизации всем идентичных параметров игрока
    
%% новый механизм сохранения 
%% параметры и всякие числа // фактически ничего не зависит от других модулей
SaveState0 =  erlang:term_to_binary({P00,P01,P02,P03,P04,P05,P06,P07, P08,P09,P10,P11,P12
	     ,P13,P14	,P140,0,0,0,0,0,0,0,0,0}, [compressed, {minor_version,1}]), %% запасные записи в тупле для расширения параметров сохранения
SaveState1 =  P15, %% явно сохраняем как число	НОМЕР ТРАНЗАКЦИИ
SaveState2 = erlang:term_to_binary({P16,P17,0,0,0,0,0,0,0,0,0}, [compressed, {minor_version,1}]),
Keygen = fun(A) -> list_to_binary([P00,"_P_",integer_to_list(A) ]) end,

KeyValuePairs = [P00, SaveState0, Keygen(1), SaveState1, Keygen(2), SaveState2],
{ok, <<"OK">>} = plg_redis_wrapper:q(C, ["MSET" | KeyValuePairs]),
save_things(C,C01,P00),

MyJid = P00,
%% wtf? зачем это тут? я сохраняю профиль и при этом я так понимаю меняю кол-во рейтинг
Prefix_sn = get_sn_prefix(MyJid),
plg_redis_wrapper:q(C, ["zadd", list_to_binary([Prefix_sn, <<"allusers">>]),get_score(State), MyJid]),

{Premium, Pdate} = get_premium_state(State),

MyJSONinfo =  mochijson2:encode({struct,[
		{"firstName",	   State#state.firstName},
		{"lastName",	   State#state.lastName},
		{"middleName",	   State#state.middleName},
		{"reit",	   get_score(State) },
		{"rang",	   get_rank(State) },
		{"img",		   get_ava(State) },
		{"premium",	   Premium },
		{"premium_date",   Pdate },
		{"jid",		   State#state.jid_bin},
		{"sn_link",	   State#state.profile_link }
	    ]}),

plg_redis_wrapper:q(C, ["SET", list_to_binary([MyJid, <<"info">>]), {get_score(State), MyJSONinfo}]),

%%?INFO_MSG("save_user_state : ~p~n", [B]),    
ok.

%%--------------------------------------------------------------------
%% @doc Создание стандартного state для новичка
%% @spec get_noob_state(Jid) -> #state{}
%% @end
%%--------------------------------------------------------------------
get_noob_state(Jid) ->
 FirstName = unicode:characters_to_binary("0J3QvtCy0L7QsdGA0LDQvdC10YY="),
 LastName = unicode:characters_to_binary("0J3QvtCy0L7QsdGA0LDQvdC10YY="),
 MiddleName = unicode:characters_to_binary("0J3QvtCy0L7QsdGA0LDQvdC10YY="),


RankId = {1, calendar:datetime_to_gregorian_seconds(erlang:localtime()), 1},
Medals = [],
Achivs = [],

 Achiv = #getted{rank=RankId,
	    medals = Medals,
	    achivs = Achivs
    },

 Jid_bin=unicode:characters_to_binary(Jid),
 Param =#perk{},
%%   ссылка на стандартный аватар пустая

 Ava = unicode:characters_to_binary(""),

 All_user_thing=[],%% формат [{Num, Id, Count}]

 Slots = get_base_slots(All_user_thing),
 {_J, Login, _H, _R, _L2, _H2, _R2} = jlib:string_to_jid(Jid),
 Sn_id = lists:nthtail(3, Login),
 Sn_id_bin = list_to_binary(Sn_id),
 Admin =  libs_functions:sn_id_is_admin(Login),
 %% иницируем статистику 
 UserStat = init_user_statistics([]),
 Now = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
 Interval_of_day =7,
 PrmDate =  Now+Interval_of_day*86400, %% премиум на первые 7 дней после регистрации
  %% первый вход в игру
%%?INFO_MSG("get_achiv_after : ~p~n", [1   ]),
 get_achiv_after(5,0008),


#state{
		jid=Jid
	      , jid_bin=Jid_bin
	      , firstName = FirstName
	      , lastName = LastName
	      , middleName = MiddleName
	      , achiv = Achiv
	      , user_param=Param
	      , sn_id = Sn_id_bin
	      , thing_change_param = #thing_change_param_rec{ava =Ava, money_0=0,money_1=0, slots=Slots, premium = {1,PrmDate}}
	      , is_admin = Admin
	      , statistics =UserStat
	      , last_init_time=Now
	      , last_enter_day=Now
	      , reg_time = Now
}.

%%--------------------------------------------------------------------
%% @doc функция анализа даты крайнего входа и подсчета статистки посещений
%% @spec last_init_time(ResultState0) -> #state{}
%% @end
%%--------------------------------------------------------------------
last_init_time_check(State0 = #state{	     last_init_time=Last_init_Time, everyday_count=Count, 
					     last_enter_day=Day_of_enter, thing_change_param = Thin_con	  }
		    ) ->

%%?INFO_MSG("last_init_time_check begin : ~p~n", [1]),

%% определить когда завершается премиум и запустить крон 
    {Pstat, Pdate} = Thin_con#thing_change_param_rec.premium,
     Now = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
%%?INFO_MSG("last_init_time_check begin : ~p~n", [2]),
State = case Pstat of
	M when M >0, Pdate > Now -> %% премиум действует и еще есть время до конца
		{ok,Ref} = libs_functions:wrap_cron_once(Pdate - Now, {gen_server, cast, [self(), {premium_timeout}]}),
%%?INFO_MSG("last_init_time_check begin : ~p~n", [3]),
		State0#state{premium_ref=Ref};
	F when F >0 -> %% премиум действует  время вышло
%%?INFO_MSG("last_init_time_check begin : ~p~n", [4]),
	       gen_server:cast(self(), {premium_timeout})		
	       , State0;
	    _  -> State0
	end,



%?INFO_MSG("last_init_time_check begin : ~p~n", [Day_of_enter]),
%%{{Current_Year, Current_Month, Current_Day}, _ } =  
Current	      = calendar:local_time(), %% {{2012,4,16},{9,44,33}}
Now_timestamp = calendar:datetime_to_gregorian_seconds(Current),
%?INFO_MSG("Current : ~p~n", [Current]),
    Day_of_enter0   = calendar:gregorian_seconds_to_datetime(Day_of_enter),
    Last_init_Time0 = calendar:gregorian_seconds_to_datetime(Last_init_Time),
    {{Current_Year, Current_Month, Current_Day}, {_A0,_A1,_A2} } =  Current,
    ZeroTime0 =	    {{Current_Year, Current_Month, Current_Day}, {0,22,0} }, %% 00:22:00
    ZeroTime = calendar:datetime_to_gregorian_seconds(ZeroTime0),

    Now_timestamp= calendar:datetime_to_gregorian_seconds(Current),
% ?INFO_MSG("last_init_time_check begin : Last_init_Time ~p~n", [Last_init_Time0]),
% ?INFO_MSG("last_init_time_check begin : Day_of_enter0	 ~p~n", [Day_of_enter0]),
{Days , _Time0}	  = calendar:time_difference(Day_of_enter0  , Current),	 %% прошли ли сутки после крайнего ++ к классности
{Days2, _Tim1 }	  = calendar:time_difference(Last_init_Time0, Current),	 %% время когда игрок заходил крайний раз

%% раздаем монетки
%%?INFO_MSG("step : ~p~n", [Now_timestamp]),
%%?INFO_MSG("step : ~p~n", [Last_init_Time < ZeroTime]),
StateS = case Last_init_Time of
	    TAa when TAa < ZeroTime , Now_timestamp >= ZeroTime -> %% время пришло надо обнулить суточную статистику
%%	       ?INFO_MSG("step : ~p~n", [TAa]),
		 get_achiv(5, State), %% накинуть монетки
		 State#state{ last_enter_day = Now_timestamp };
	    _ -> State
		end,
%%?INFO_MSG("step : ~p~n", [2]),

NewState = case Days2 of
	       Ti2 when Ti2 == 0, Days == 1; Ti2==1 ->	
	       %% прошли ровно сутки с момента начисления довольствия или прошли сутки с момента инита
	       msg_self_change_stat([{95,Count,max}]),
	       StateS#state{ everyday_count=Count+1 };
	       Timeout when Timeout >1 ->   %% прошло два или более  суток с его последнего входа (init-а) а значит и с последнего входа в игру
		   msg_self_change_stat([{95,Count,max}]), 
		StateS#state{everyday_count=0  };
		_ -> %% чел входил в игру менее суток назад
		      StateS
			   end,
%% так же требуется привязатся ко времени например 00 20 и если это первый вход после 00 20 этих суток то запустить функцию эффекта входа

 NewStateM = day_effect_apply (NewState),


NewStateM2 =	case Last_init_Time of
	    A when A < ZeroTime , Now_timestamp >= ZeroTime -> %% время пришло надо обнулить суточную статистику
		set_zero_stat(NewStateM);
	    _ -> NewStateM	
		end,
    
NewStateM2#state{last_init_time = Now_timestamp}.


%%--------------------------------------------------------------------
%% @doc Функция анализирующая переменные из редиса и применяющая эффекты на профиль
%% @spec day_effect_apply (State) -> #state{} 
%% @end
%%--------------------------------------------------------------------
day_effect_apply (State0 =  #state{tran_number = Number
		  ,link_to_redis = Link}) ->
%% общая транзакция
Key = "tran_enter_eff",	 %%  string Номер транзакции
Key_user = lists:append(["effects_",binary:bin_to_list(State0#state.jid_bin)]),

{ok, AA0} = plg_redis_wrapper:q(Link, ["GET", Key]),
AA = bin_to_num(AA0),
 P1 = [["MULTI"],
      ["SMEMBERS",  Key_user],
      ["del",  Key_user],
      ["EXEC"]
      ],
    [{ok, <<"OK">>}, {ok, <<"QUEUED">>},  {ok, <<"QUEUED">>},  {ok, [MyListMSG, _deldone]} ] = plg_redis_wrapper:qp(Link, P1),
    

    
State = msg_list_decode_send(MyListMSG,State0),

NewState =  case AA of
	M when M > Number ->  %% надо синхронизовать транзакцию
	      Key_query = "effects_", %% <set> SMEMBERS 
	     {ok, ListMSG} = plg_redis_wrapper:q(Link, ["SMEMBERS", Key_query]), %% {ok,[<<"1">>,<<"12">>,<<"14">>]}
	      msg_list_decode_send(ListMSG,State#state{tran_number = M});
	_ -> State 
	    end,

    diff_state(State,NewState),
    
NewState
.


%%--------------------------------------------------------------------
%% @doc функция парсинага Jid и определения префикса
%% @spec   get_sn_prefix(Jiv) -> Pref.
%% @end
%%--------------------------------------------------------------------
get_sn_prefix(Jid) ->
    [Pref1, Pref2|_T]= Jid,
    Pref = list_to_binary([Pref1, Pref2]),
    Pref.

%%--------------------------------------------------------------------
%% @doc функция формирования вывода личного дела
%% @spec   personal_data(State) -> JSON.
%% @end
%%--------------------------------------------------------------------
personal_data(State) ->
LL1 = <<"{ \"reply\": { \"type\": 51, \"body\": { \"dossier\": { \"pvp\": ">>,
LL2 = <<"} } } }">>,


Statistics = State#state.statistics,

All_bonuses = get_stat_summ(Statistics, [97, 98, 99, 100, 101, 102, 103, 104, 105, 106]),
All_loss =    get_stat_summ(Statistics, [61, 64, 66, 69, 70, 71, 73]),
%All_battles = get_stat_summ(Statistics, [60, 61, 63, 64, 65, 66, 68, 69, 70, 71, 72, 73]),
All_battles = get_stat_summ(Statistics, [60, 61, 76, 64, 77, 66, 68, 69, 70, 71, 72, 73]),


Medals1 = (State#state.achiv)#getted.medals,
Medals2 = user_medals_list_flip(Medals1),
Medals = user_medals_list_sort(Medals2),

GET_TRE_BEST_MEDAL = fun(A, AccIn) ->
    {Medal_id_A, _Date_get, _Qntty} = A,
    Medal_level = achiv_info:show_info(Medal_id_A, level),
    if (Medal_level>5) -> lists:flatten([AccIn, [A]]);
	true -> AccIn
    end
end,

%TreMedals = lists:sublist(Medals, 3),
TreMedals1 = lists:foldl(GET_TRE_BEST_MEDAL, [], Medals),

TreMedals  = lists:sublist(TreMedals1, 3),
%{"win_contest",  get_stat_val(65, Statistics)},
%{"win_duel",	  get_stat_val(63, Statistics)},
Params_out = mochijson2:encode({struct,[
	  {"all_battles",  All_battles},
	  {"win_training", get_stat_val(60, Statistics)},
	  {"win_contest",  get_stat_val(77, Statistics)},
	  {"win_duel",	   get_stat_val(76, Statistics)},
	  {"win_tourney",  get_stat_val(72, Statistics)},
	  {"all_loss",	   All_loss},
	  {"los_contest",  get_stat_val(66, Statistics)},
	  {"los_duel",	   get_stat_val(64, Statistics)},
	  {"all_out",	   get_stat_val(81, Statistics)},
	  {"all_bonus",	   All_bonuses},
	  {"medals", [ achiv_info:show_info(Medal_id, {4, Qntty}) ||  {Medal_id, _Date_get, Qntty} <- TreMedals]}
    ]}),


list_to_binary([LL1, Params_out, LL2])
.


% функция отображения списка достижений

user_achivs(State) ->

LL1 = <<"{ \"reply\": { \"type\": 53, \"body\":	 ">>,
LL2 = <<"} }  ">>,

Achivs = (State#state.achiv)#getted.achivs,

Params_out = mochijson2:encode({struct,[

	  {"merits", [ achiv_info:show_info(Achiv_id, {5, Date}) ||  {Achiv_id, Date, _Num} <- Achivs]}
    ]}),


list_to_binary([LL1, Params_out, LL2])
.

%% функция отображения списка медалей

user_medals(State) ->
LL1 = <<"{ \"reply\": { \"type\": 52, \"body\":	 ">>,
LL2 = <<"} }  ">>,

Medals1 = (State#state.achiv)#getted.medals,
Medals2 = user_medals_list_flip(Medals1),
Medals = user_medals_list_sort(Medals2),

Params_out = mochijson2:encode({struct,[

	  {"medals", [ achiv_info:show_info(Medal_id, {4, Qntty}) ||  {Medal_id, _Date_get, Qntty} <- Medals]}
    ]}),
list_to_binary([LL1, Params_out, LL2])
.

%% функция сортировки массива достижений по уровню

user_medals_list_sort(Medals_list) ->
SORT_MEDAL = fun (A, B) ->
    {Medal_id_A, _ADate_get, _AQntty} = A,
    {Medal_id_B, _BDate_get, _BQntty} = B,
    Medal_level_A = achiv_info:show_info(Medal_id_A, level),
    Medal_level_B = achiv_info:show_info(Medal_id_B, level),
    if (Medal_level_A>Medal_level_B) -> true;
	true -> false
    end
end,

lists:sort(SORT_MEDAL, Medals_list)
.


%% функция склейки нескольких достижений по группам в 1

user_medals_list_flip(Medals_list) ->
FLIP_MEDAL = fun (A, AccIn) ->
    {Medal_id_A, Date_get, _Qntty} = A,
    Medal_group_A = achiv_info:show_info(Medal_id_A, group),
    if (Medal_group_A == Medal_id_A) -> lists:flatten([AccIn, [A]]);
	true -> 
		In_list = lists:member(Medal_group_A, AccIn),
		if (In_list == false)  -> lists:flatten([AccIn, [{Medal_group_A, Date_get, achiv:num_getted_achiv(Medal_id_A, A)}]]);
			true -> AccIn
		end
    end
end,

lists:foldl(FLIP_MEDAL, [], Medals_list)
.

%%--------------------------------------------------------------------
%% @doc функция парсинага списка сообщений и отправки сообщение себе 
%% @spec   msg_list_decode_send(ListMSG) -> #state{}.
%% @end
%%--------------------------------------------------------------------
msg_list_decode_send([],State) -> 
    State;
msg_list_decode_send([Head | Tail],State=#state{thing_change_param=DB}) -> 
  {rmsg, Type, Fun, List}= erlang:binary_to_term(Head),
   NewDB =    case Type of
	 A when is_function(Fun,1)  andalso  A==1 -> Fun(DB,List);
	_ -> State    
		 end,	 
msg_list_decode_send(Tail,State#state{thing_change_param=NewDB}).
 

%%--------------------------------------------------------------------
%% @doc надо вяснить и обнулить суточную статистику а недельюную или месячную если конец недели или месяца
%% @spec   set_zero_stat(NewStateM) ->	  NewStateM.
%% @end
%%--------------------------------------------------------------------
set_zero_stat(NewStateM = #state{ statistics=Stat, jid_bin=Ajid }) ->
%% суточная 
Empty_arr    = array:new([{size, 20}, {default, 0}, {fixed, true}]),
NStat0 = Stat#statistics{stat_day=Empty_arr },

NewStateM1 = dell_achive_event(NewStateM, 1),

{Year, Month, Day} = erlang:date(),
    Calc_day_achiv_type = 2,
    Calc_week_achiv_type = 3,
    Calc_moon_achiv_type = 4,
get_achiv0(Ajid,Stat,Calc_day_achiv_type),

%% недельная
NStat1 =  case calendar:day_of_the_week({Year, Month, Day}) of
	1 -> get_achiv0(Ajid,Stat,Calc_week_achiv_type), 
	     NewStateM2=dell_achive_event(NewStateM1, 2),
	     NStat0#statistics{stat_week=Empty_arr };
	_ -> NewStateM2=NewStateM1, NStat0
	  end,
    


%% месячная

    NStat =case Day of
	       1 ->get_achiv0(Ajid,Stat,Calc_moon_achiv_type), 
		   NewStateM3=dell_achive_event(NewStateM2, 3),
		   NStat1#statistics{stat_moon=Empty_arr } ;  %% первый день месяца
	       _ -> NewStateM3=NewStateM2,NStat1       end,

NewStateM3#state{  statistics=NStat}.


%%--------------------------------------------------------------------
%% @doc Сохраняет в редис инвентарь игрока
%% @spec save_things(C,C01) -> ok
%% @end
%%--------------------------------------------------------------------
save_things(C,_C01 = #thing_change_param_rec{
		money_1 = T0	   %% кредиты
		, money_0 = T1	     %% монеток
		, ava =	    T2 %% аватара
		, premium = T3		%% флаг премиум режима {Режим, Timestamp}  дата в unix timestamp
		, slots	  = T4		  %% array слотов
	       },P00) ->

 Keygen = fun(A) -> list_to_binary([P00,"_T_",integer_to_list(A) ]) end,

ST0 = T0,
ST1 = T1,
ST2 = T2,
ST3 = erlang:term_to_binary(T3, [compressed, {minor_version,1}]),
 Get_all_things = fun( _, _Value= #slot{num = I, count=Count }, AccIn) -> 
		 lists:append(AccIn,[{I,I,Count}]) %% ID совпадает с номером слота
	 end,

ST4 = erlang:term_to_binary(array:foldr(Get_all_things,[],T4), [compressed, {minor_version,1}]),

KeyValuePairs = [Keygen(0), ST0, Keygen(1), ST1,Keygen(2), ST2,	 Keygen(3), ST3,Keygen(4), ST4],
 {ok, <<"OK">>} = plg_redis_wrapper:q(C, ["MSET" | KeyValuePairs]).

%%--------------------------------------------------------------------
%% @doc бинарную строку <<"100">> в число 100
%% @spec bin_to_num(Bin) -> integer()
%% @end
%%--------------------------------------------------------------------
bin_to_num(A) -> plg_tools:bin_to_num(A).



get_user_state(Type, Link, Jid, MyJid) ->

    Pid4pro=gproc:lookup_local_name(Jid),
    
    case (libs_functions:alive(Pid4pro)) of
        true  ->
        %% отправить запрос туда
        gen_server:cast(Pid4pro, {send_achiv, MyJid, Type}),
        Result = cast;
        _ ->
       % если такого процесса нет, то берем из редиса
        {ok, AA} = plg_redis_wrapper:q(Link, ["GET", Jid]),

        case AA of
            undefined ->            Result = undefined;
	    <<"saved_in_db">> ->          Result = plg_save_2db:restore_profile_from_db(Jid);
            B when is_binary(B) ->  Result = recovery_user_state(Link, AA);	    
            _ ->                    Result = undefined
                 end
    end,
Result.



%%--------------------------------------------------------------------
%% @doc восстановление профиля из редиса
%% @spec recovery_user_state(Link,B) -> #state{}
%% @end
%%--------------------------------------------------------------------
recovery_user_state(Link,B)  ->

    {P00,P01,P02,P03,P04,P05,P06,P07,P08,_P09,_P10,P11,P12
     ,P13,P14   ,P140,0,0,0,0,0,0,0,0,0} =   erlang:binary_to_term(B),

    Keygen = fun(A) -> list_to_binary([P00,"_P_",integer_to_list(A) ]) end,
    %% номер транзакции
%%  ?INFO_MSG(" State#state.tour_serv : ~p ~p~n", [plg_redis_wrapper:q(Link, ["GET",  Keygen(1)])]),
    P15 =    case  plg_redis_wrapper:q(Link, ["GET",  Keygen(1)]) of
                 {ok, <<"undefined">>} ->0;
		 {ok, AA} when is_binary(AA) ->bin_to_num(AA);       
		 _ -> 0		end,
    %% статистика
    {P16,P17,0,0,0,0,0,0,0,0,0} =  
	case  plg_redis_wrapper:q(Link, ["GET",  Keygen(2)]) of
	    {ok, GP15} when is_binary(GP15) ->       erlang:binary_to_term(GP15);
		_ -> {init_user_statistics([]),{0,0},0,0,0,0,0,0,0,0,0}	end, 

    %% вещи и инвентарь

    Keyg = fun(A) -> list_to_binary([P00,"_T_",integer_to_list(A) ]) end,
    T0 = case  plg_redis_wrapper:q(Link, ["GET",  Keyg(0)]) of %% кредиты
             {ok, <<"undefined">>} ->0;
	     {ok, GT0} when is_binary(GT0) ->bin_to_num(GT0);       
	     _ -> 0		end,
    T1 = case  plg_redis_wrapper:q(Link, ["GET",  Keyg(1)]) of %% монеты
             {ok, <<"undefined">>} ->0;
	     {ok, GT1} when is_binary(GT1) ->bin_to_num(GT1);       
	     _ -> 0		end,
    T2 = case  plg_redis_wrapper:q(Link, ["GET",  Keyg(2)]) of %% ава
	     {ok, GT2} ->GT2;       
	     _ -> 0		end,
%% премиум
   T3 = case  plg_redis_wrapper:q(Link, ["GET",  Keyg(3)]) of
	    {ok, GT3} when is_binary(GT3) ->       erlang:binary_to_term(GT3);
		_ ->  {0,0} 	end, 
%% вещи
Thr = case  plg_redis_wrapper:q(Link, ["GET",  Keyg(4)]) of
	    {ok, GT4} when is_binary(GT4) ->       erlang:binary_to_term(GT4);
		_ ->  []	end,

   T4 = get_base_slots(Thr), 



C01 = #thing_change_param_rec{
         	money_1 = T0       %% кредиты
		, money_0 = T1       %% монеток
		, ava =     T2 %% аватара
		, premium = T3          %% флаг премиум режима {Режим, Timestamp}  дата в unix timestamp
		, slots   = T4            %% array слотов
	       },
 {_J, Login, _H, _R, _L2, _H2, _R2} = jlib:string_to_jid(P00),
 Admin =  libs_functions:sn_id_is_admin(Login),

#state{         	       jid          = P00
			       , jid_bin    = P01
			       , firstName  = P02
			       , lastName   = P03
			       , middleName = P04
			       , sn_id      = P05
			       , reg_time   = P06
			       , achiv      = P07
			       , msg_send   = P08
%%			       , user_param = P09
			       , user_param = #perk{}			     
			       , last_init_time = P11
			       , last_enter_day = P12
			       , everyday_count = P13
			       , profile_link   = P14
			       , user_data      = P140

			       , tran_number    = P15

			       , statistics     = P16
			       , buy_kredits = P17
                               , is_admin = Admin
                               , thing_change_param = C01 
			      }.
%%--------------------------------------------------------------------
%% @doc Функция заново заталкивает игрока в битву после того как чел обновился
%% @spec reinit_user_battle(State) -> []
%% @end
%%--------------------------------------------------------------------
reinit_user_battle(_State = #state{battle_serv =BattPid, battle_id =Id, 
				  jid= First_user , player0 = P0, player1 = P1
                                 , battle_invite_msg = Battle_invite
				 , you_turn = Turn
				 , timer_start_time = { FlagNB,Time_St}
                                 }
                    ) -> %% задержка если чел в турнире чтоб успело отрисоватся окно турнира
    %% сообщение должно прийти позже чем профиль то есть на все ответы должна быть задержка скажем в 1 сек
       
       User = jlib:string_to_jid(First_user),
      {Mega, Seconds, _} = erlang:now(),
      Stamp_now = Mega * 1000000 + Seconds,
    Interval =case FlagNB  of
                  0 -> 0; %% не первый ход в битве
		  _ -> 10  end,
  
    Timer = case (Stamp_now-Time_St)< 30 + Interval  of
               true when (Stamp_now-Time_St)>=0 -> (Stamp_now-Time_St);
	       _ ->29 		   
		       end,
    TurnA =case Turn of
               -1 ->0; %% если пробелаи таймер то по сути это уже не наш ход
	       _  ->Turn
		       end,
%% чей ход
   Msg0= lists:append([{ send_user,User, match_battle:any_move_json(2+TurnA,1,Timer)}],[]),
   Msg1= lists:append([{cast,BattPid,{map, Id, User,P0,P1,no_send}}],Msg0),
   lists:append([{send_user,User, Battle_invite}],Msg1).

%%--------------------------------------------------------------------
%% @doc вспомогательная функция валидизации state
%% @spec valid_state(State) -> state{}
%% @end
%%--------------------------------------------------------------------
valid_state(State = #state{test_valid_state =A}) -> 
State#state{
test_valid_state = A+1
}.

%%--------------------------------------------------------------------
%% @doc Функция составления и отправки логов игроку
%% @spec set_log(User, Type, Params) 
%% Params = {???}
%% Type = 1 - достижения (звания, награды, достижения ...)
%% @end
%%--------------------------------------------------------------------
set_log(User, Type, Params) ->
libs_functions:send_log_to_user(User, Type, Params,[])
.

set_log(User, Type, Params, Color) ->
 libs_functions:send_log_to_user(User, Type, Params,Color)
.


add_auto_buy(Tid, Id, Count, State) ->

    Wotid = lists:keydelete(Tid, 1, State#state.auto_buy),

    case (lists:keysearch(Tid, 1, State#state.auto_buy)) of
        {value, Vall} -> {_T, Th_list} = Vall,
                         State#state{ auto_buy = lists:flatten([Wotid, [{Tid, lists:flatten([Th_list, [{Id, Count}]])}]]) };
                    _ -> State#state{ auto_buy = lists:flatten([Wotid, [{Tid, [{Id, Count}]}]]) }
    end
.

dell_auto_buy(Tid, State) ->
State#state{ auto_buy = lists:keydelete(Tid, 1, State#state.auto_buy) }
.

%%--------------------------------------------------------------------
%% @doc  логи для поиска баги с завершением профиля
%% @spec change_status(Pid,NumOld,Num)-> ok.
%% @end
%%--------------------------------------------------------------------
test_change_status(State=#state{transaction_state=NumOld}, Num,Flag)->
{{_Year,_Month,_Day}, {Hour,Minutes,Seconds}} = erlang:localtime(),         %%{{1970,1,1},{0,0,0}}
 Datetime = lists:flatten([libs_functions:add_zero(Hour), ":", libs_functions:add_zero(Minutes),":", libs_functions:add_zero(Seconds)]),
 Jid = State#state.jid,
?INFO_MSG("~p  ~p change [~p]   ~p -> ~p ~n", [Datetime,Flag,Jid,NumOld,Num]).
