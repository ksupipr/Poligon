%%%-------------------------------------------------------------------
%%% File    : match3_battle.erl
%%% Author  : Marat Yusupov <marat@yusupov.me> and Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : обвязка вокруг обязки модуля уровня ядра для match3 Полигон
%%% Created :
%%%-------------------------------------------------------------------
-module(match_battle).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("poligon.hrl").
-include("shared.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% any fun library
-export([any_move_json/3]).


-record(state, {port, %% на будущее
                battle_count=0 %% число битв
		, id_list = []
               }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%% Наша обвязка к модуля ядра зарегана в gen_server  поэтому
%% запуск идет через вызов gen_server
%% после этого gen_server запускает callback init
%%
%% Первый аргумент – {local,?SERVER} – определяет имя сервера. В нашем случае сервер будет локально
%% зарегистрирован под именем ch3. Если имя будет опущено, то gen_server не будет зарегистрирован.
%% Вместо имени будет использоваться идентификатор процесса. В качестве имени также может быть
%% передан кортеж {global, Name}, при этом сервер будет зарегистрирован с помощью функции global:register_name/2
%%
%% Второй аргумент – ?MODULE  – имя callback-модуля, в котором расположены callback-функции, реализованные нами.
%% В нашем случае интерфейсные функции (start_link, alloc и free) и callback- функции (init, handle_call и handle_cast)
%% расположены в одном модуле. Это достаточно обычная ситуация, поскольку удобно размещать код, связанный с процессом,
%% в одном модуле.
%%
%% Третий аргумент – [] – терм, который передаётся в качестве параметра callback-функции init.
%% В нашем случае init  "match_erl_drv" имя библиотеки
%%
%% Четвёртый аргумент – [] – список опций. Список доступных опций приводится в описании модуля gen_server
%%
%% Для синхронизации процедуры запуска сервера, функция start_link/3,4 не возвращает управление до тех пор,
%% пока не завершится выполнение функции Module:init/1.
%%--------------------------------------------------------------------
start_link() ->
     case catch gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
          {ok, Server} ->
              {ok, Server};
          {'EXIT', Reason} ->
              exit(Reason)
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
%%
%% То есть нас запустили задача инициализировать все то что надо в том числе линк к порту модуля ядра
%% и это выдать в качестве state ибо я думаю что переменные созданные в рамках модуля
%% бессмысленные раз мы получаем управленияе сквозь gen_server то есть нельзя определить Port и
%% оставить его как есть не добавив в state
%% ПОТВЕРЖДЕНИЕ: если вы пришли из ООП, вы можете принять состояние сервера(state) за переменные его экземпляра.
%% В каждом callback методе вам будут доступны эти переменные, и также вы сможете их изменять.
%%
%% Примеры возвращаемых значений:
%%
%% {ok,<0.37.0>}
%%--------------------------------------------------------------------
init(_) ->
        snappy:free(0), %% инициализируем ядерный модель приложения match3
        Name = <<"battle_serv">>,
        gproc:add_local_name(Name),
        {ok, #state{}}.

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
?INFO_MSG(" packet received handle_call  : ~p~n", [State]),
{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({new,  User1,Num1,User2,Num2,Reit,Type,Num_of_tourn}, State = #state {id_list = Old_list}) ->
%% узнать какой id Битвы назначен
InTime = time_for_function(),


{user_perk,_Score1,Soldier1,Brone1,Avia1,Bet,Profile1} = Num1,
{user_perk,_Score2,Soldier2,Brone2,Avia2,Bet,Profile2} = Num2,
%% {struct,[ {"firstName",  B},  {"lastName",   C},  {"rang", A}, |_
   {struct,[ {_,  EB1},  {_,   EC1},  {_, EA1} |_]} = Profile1 ,
   {struct,[ {_,  EB2},  {_,   EC2},  {_, EA2} |_]} = Profile2 ,  
    Re_decode =   fun(A) -> 
	  libs_functions:base64_decode(A)
	 end, %% fun
     A1 = Re_decode(EA1),
     B1 = Re_decode(EB1),
     C1 = Re_decode(EC1),
     A2 = Re_decode(EA2),
     B2 = Re_decode(EB2),
     C2 = Re_decode(EC2),

   Fio1 = lists:flatten([A1, " ", B1," ",C1]),
   Fio2 = lists:flatten([A2, " ", B2," ",C2]),

{match_res,Id_map,Next_turn,_Win_status,Panel1,Panel2, map,Map}=snappy:new(Avia1,Brone1,Soldier1,Avia2,Brone2,Soldier2),
%% TODO проверить ошибку вдруг сервер не смог выделить карту для битвы
%?INFO_MSG(" Next_turn done:[ ~p] vs [~p] ~p~n", [1,2,Next_turn]),
%% отдать прифилю игроков что челы в указанной битве
Jid1=jlib:jid_to_string(User1),
Pid1=gproc:lookup_local_name(Jid1),
Jid2=jlib:jid_to_string(User2),
Pid2=gproc:lookup_local_name(Jid2),

        case ({ libs_functions:alive(Pid1), libs_functions:alive(Pid2)}) of
                {true,true}  ->
                        %% пиды корректны
		%%   1 - поединок  2 - дуэль 3 - турнир

		   Timer_4End_Battle = case Type of
			3 when Num_of_tourn<29 ->  ?BATTLE_TIMEOUT_TURNIR;
			_ ->               ?BATTLE_TIMEOUT_DEF end,
%			_ ->               33 end,

		{_,T0x0,T0x1,T0x2, T0x3,T0x4,T0x5 }= Panel1,
		{_,T1x0,T1x1,T1x2, T1x3,T1x4,T1x5 }= Panel2,
		        BattleList=[{<<"type">>,Type},
                        {<<"bet">>,Bet},
                        {<<"coin_war">>,Bet + (Bet div 2)},
                        {<<"rating">>, Reit},
                        {<<"timer">>, Timer_4End_Battle+180}, %% длительность боя
                        {<<"player0">>,unicode:characters_to_binary(Jid1)},
                        {<<"player1">>,unicode:characters_to_binary(Jid2)},
			{<<"set_profile0">>, Profile1},
			{<<"set_profile1">>, Profile2},
			{<<"panel0">>,{struct, [{<<"tx0">>,T0x0},{<<"tx1">>,T0x1},{<<"tx2">>,T0x2},
					       {<<"tx3">>,T0x3},{<<"tx4">>,T0x4},{<<"tx5">>,T0x5}
					       ] }},
			{<<"panel1">>,{struct, [{<<"tx0">>,T1x0},{<<"tx1">>,T1x1},{<<"tx2">>,T1x2},
					       {<<"tx3">>,T1x3},{<<"tx4">>,T1x4},{<<"tx5">>,T1x5}
					       ] }}
                         ],

                        Text=mochijson2:encode({struct,[{<<"reply">>,
                        {struct,[{<<"type">>,31},
                        {<<"body">>,
                        {struct,[{<<"battle">>,
                         {struct, BattleList }}
                        ]}  }]}}]}),

                        Battle_count = State#state.battle_count+1,

                        {ok,RefB} =  libs_functions:wrap_cron_once(Timer_4End_Battle, 
								   {gen_server, cast, [self(), {timeout_step1, Id_map,User1,User2}]}),
                        gen_server:call(Pid1, {begin_battle,Id_map,User1,User2,1, %% его ход следующий
                        Bet,Reit,self(),RefB,Text,Fio2}),
                        gen_server:call(Pid2, {begin_battle,Id_map,User1,User2,0, %% ждет хода соперника
                        Bet,Reit,self(),RefB,Text,Fio1}),

                        %% состояние игроков на сервере сменили
                        %% челы зашли в битву им об этом сообщить

                        send_msg( User1, Text),
                        send_msg( User2, Text),


                        %% челы зашли в битву им надо отправить карту
                        Bin1 = <<"{\"reply\":{\"type\":100,\"body\": [ ">>,
                        Bin3 = <<" ] }}">>,
                        Text1=list_to_binary([Bin1,Map,Bin3]),
  Interval = 1, %% задержка перед отправкой карты 1 секунда  дабы сперва отрисовать окно 
 libs_functions:wrap_cron_once(Interval, {p_sender, send_msg, [User1, Text1]}),
 libs_functions:wrap_cron_once(Interval, {p_sender, send_msg, [User2, Text1]}),
		Id_list = [Id_map|Old_list],
                        you_move_send(User1,User2,{Next_turn,1});
                        %% отправить чей ход (не надо ибо оно определяется по "Player0")
                _ ->
		        snappy:free(Id_map), Id_list = Old_list,
                        Battle_count = State#state.battle_count

        end,

NewState = State#state{battle_count  = Battle_count,id_list=Id_list},
InTime(),

{noreply, NewState};

handle_cast({map,  Id,User,User0,User1,Flag_send_turn}, State= #state {id_list = Old_list}) ->
%% отдать карту
%%
InTime = time_for_function(),
    case lists:member(Id, Old_list) of
     true ->
	    {match_res,_Id_map,Next_turn,Win_status,_Panel1,_Panel2, map,Map}=snappy:get_map(Id),

	    case (Win_status) of
                3 ->    Bin1 = <<"{\"reply\":{\"type\":100,\"body\": [ ">>,
                        Bin3 = <<"]}}">>,
                        Text1=list_to_binary([Bin1,Map,Bin3]),
                        send_msg( User, Text1);
		4 -> ?INFO_MSG(" WTF map? : ~p~n", [Map]), end_battle(User0,User1,1);
                M ->  %% победа одного из игроков
                     end_battle(User0,User1,M)
	    end,
	    case Flag_send_turn of
		send -> you_move_send(User0,User1,{Next_turn,0});
		_ -> []
			 end;
	_ ->   ok
    end,
    

InTime(),
{noreply, State};

handle_cast({skip, Id,_First_user,User0,User1}, State= #state {id_list = Old_list}) ->
%% пропуск хода
%% ведет к пометке об ошибке о переходе хода
  InTime = time_for_function(),
    case lists:member(Id, Old_list) of
     true ->

	    {match_res,_Id_map,Next_turn, Win_status,_Panel1,_Panel2, map,_Map}=snappy:next_user(Id),
	    %%?INFO_MSG(" Next user : ~p~n", [Next_turn]),
	    %% you_move_send(User0,User1,{Next_turn,0}),
	    ?INFO_MSG(" Next_turn done: ~p~n", [Next_turn]),
	    %% если вдруг победа игрока
	    %% Win_status
	    %% 0 победил 0 игрок
	    %% 1 победил 1 игрок
	    %% 3 битва продолжается
	    %% 4 ошибка - переход хода(в результате ошибки)
	    %% 5 победил 5 игрок
	    %% 6 победил 6 игрок
	   { Battle_count,New_list} = case (Win_status) of
			       %% отдать прифилю игроков что челы в указанной битве
			       A when A == 5; A == 6 ->   %% победа одного из игроков изза ошибки второго
				   snappy:free(Id),
				   end_battle(User0,User1,Win_status),
				   {(State#state.battle_count-1),Old_list--[Id]};

			       B when B==0; B==1 ->  %% победа одного из игроков выроженно скорее всего это условие никогда не сработает
				   %%  ?INFO_MSG(" skip normal win : ~p~n", [Win_status]),
				   snappy:free(Id),
				   %% +5 ибо ядро расчета боя выдает неправильный статус и проще тут поменять чем в си лазить
				   end_battle(User0,User1,Win_status+5),
				   {(State#state.battle_count-1),Old_list--[Id]};
			       _ -> you_move_send(User0,User1,{Next_turn,0}), {(State#state.battle_count),Old_list}

			   end,
	    NewState = State#state{battle_count  = Battle_count, id_list =New_list};
	_ ->  NewState = State, ok
    end,
InTime(),
{noreply, NewState};


handle_cast({change,  Id,User,User0,User1,N}, State= #state {id_list = Old_list}) ->
%% отдать результаты хода

InTime = time_for_function(),
    case lists:member(Id, Old_list) of
     true ->
	    {struct, [{<<"go">>,M}]}=N,
	    {struct,[{<<"x0">>,X0},{<<"y0">>,Y0},{<<"x1">>,X1},{<<"y1">>,Y1}]} = M,
	    {match_res,_Id_map,Next_turn,Win_status,Panel1,Panel2, map,Map}=snappy:change(Id,X0,Y0,X1,Y1),
	    %%  проверить ошибки при ходе(переход хода проверяется при ошибочном ходе)
	    %% Если ход успешный то про ход надо сообщить профилю игрока
	    %% ... {"turn":{"player":1,"go":{"x0":2,"y0":5, "x1":2,"y1":6}}}
	    %%?INFO_MSG(" Next_turn done: ~p~n", [Next_turn]),

	    %%?INFO_MSG(" ~p~n / ~p~n / ~p~n State : ~p~n", [Id_map,Next_turn,Win_status,Map]),
	    %%
	    %%
	    %%
	    %%
	    case (Win_status) of
		%% отдать прифилю игроков что челы в указанной битве
                3 -> 
		    %% битва продолжается
		    %% сообщение о ходе передаем игроку
		    Text_mod = case User  of
				   User0 ->
				       %% ход User0
				       Next_user=User1,
				       send_msg( User, get_json_turn_panel(1,X0,Y0,X1,Y1,Panel1,Panel2)),
				       get_json_turn_panel(2,X0,Y0,X1,Y1,Panel2,Panel1);

				   User1 ->
				       %% ход User1
				       Next_user=User0,
				       send_msg( User, get_json_turn_panel(1,X0,Y0,X1,Y1,Panel2,Panel1)),
				       get_json_turn_panel(2,X0,Y0,X1,Y1,Panel1,Panel2)
			       end,
		    send_msg( Next_user, Text_mod),
		    Bin1 = <<"{\"reply\":{\"type\":100,\"body\": [ ">>,
		    Bin3 = <<"]}}">>,
		    Text1=list_to_binary([Bin1,Map,Bin3]),
		    send_msg( User0, Text1),
		    send_msg( User1, Text1),
		    New_list=Old_list,
		    Battle_count = State#state.battle_count,
		    you_move_send( User0,User1,{Next_turn,0});
                4 ->  %% ошибка - переход хода
		    %% !!! DOCS !!!
		    %% на 28 марта если ход неправильный клиент сам откатит ход
		    %% если ход правильный а сервер считает что ход косячный начинается хуйня
		    %% ход на клиенте остается верным а на сервере ход откатывается
		    Battle_count = State#state.battle_count,
		    New_list=Old_list,
		    you_move_send( User0,User1,{Next_turn,0});

                A when A == 5; A == 6 ->   %% победа одного из игроков изза ошибки второго
		    %%   you_move_send( User0,User1,{Next_turn,0}),
		    {_,Move_type,_Interval} = Next_turn,
		    snappy:free(Id),
		    Move_type, %% TODO  тут требуется добавить параметр для завершения
		    end_battle(User0,User1,Win_status), %% TODO  тут требуется добавить параметр для завершения
		    New_list=Old_list--[Id],
		    Battle_count = State#state.battle_count-1;

                _ ->  %% победа одного из игроков

		    Text_mod = case User  of
				   User0 ->
				       %% ход User0
				       Next_user=User1,
				       send_msg( User, get_json_turn_panel(1,X0,Y0,X1,Y1,Panel1,Panel2)),
				       get_json_turn_panel(2,X0,Y0,X1,Y1,Panel2,Panel1);

				   User1 ->
				       %% ход User1
				       Next_user=User0,
				       send_msg( User, get_json_turn_panel(1,X0,Y0,X1,Y1,Panel2,Panel1)),
				       get_json_turn_panel(2,X0,Y0,X1,Y1,Panel1,Panel2)
			       end,

		    send_msg( Next_user, Text_mod),
		    Bin1 = <<"{\"reply\":{\"type\":100,\"body\": [ ">>,
		    Bin3 = <<"]}}">>,
		    Text1=list_to_binary([Bin1,Map,Bin3]),
		    send_msg( User0, Text1), 
		    send_msg( User1, Text1),
                    %% you_move_send( User0,User1,{Next_turn,0}),
                    %% ходы в результате победы клиенту не слать  
		    snappy:free(Id),
		    end_battle(User0,User1,Win_status),
		    New_list=Old_list--[Id],
		    Battle_count = State#state.battle_count-1

	    end;
	_ ->  New_list=Old_list,
	    Battle_count = State#state.battle_count
		  
    end,
NewState = State#state{battle_count  = Battle_count,id_list =New_list},
InTime(),
{noreply, NewState};

handle_cast({line, Id,User_Turned,User0,User1,Slot1,Slot2,Line_num}, State= #state {id_list = Old_list}) ->
InTime = time_for_function(),
NewState =  case lists:member(Id, Old_list) of
     true ->
	    %% отдать результаты использовани бонуса
	    %% перехода хода быть не должно но я могу победить
	    Bonus_result = snappy:line(Id,Line_num),
	    bonus_apply_analize(State,Bonus_result,Slot1,Slot2,User_Turned,User0,User1)
	    ;
	_ -> State
    end,
InTime(),
{noreply, NewState};

handle_cast({boom, Id,User_Turned,User0,User1,Slot1,Slot2,Coord}, State= #state {id_list = Old_list}) ->
%% отдать результаты использовани бонуса
%% перехода хода быть не должно но я могу победить
InTime = time_for_function(),
NewState =  case lists:member(Id, Old_list) of
     true ->
		    {X,Y} =Coord,
		    Bonus_result = snappy:boom(Id,X,Y),
		    bonus_apply_analize(State,Bonus_result,Slot1,Slot2,User_Turned,User0,User1);
	_ -> State
    end,
InTime(),
{noreply, NewState};

handle_cast({random_cell, Id,User_Turned,User0,User1,Slot1,Slot2,_Coord}, State= #state {id_list = Old_list}) ->
InTime = time_for_function(),
%% отдать результаты использовани бонуса
NewState =  case lists:member(Id, Old_list) of
     true ->

Bonus_result = snappy:random_cell(Id),
%%?INFO_MSG(" random_cel ok: ~p~n", [Bonus_result]),
bonus_apply_analize(State,Bonus_result,Slot1,Slot2,User_Turned,User0,User1);
%%?INFO_MSG(" random_cel done: ~p~n", [1]),
	_ -> State
    end,
InTime(),
{noreply, NewState};

handle_cast({change_health, Id,User_Turned,User0,User1,Slot1,Slot2,Coord}, State= #state {id_list = Old_list}) ->
%% изменить число здоровья 
%%
InTime = time_for_function(),
NewState =  case lists:member(Id, Old_list) of
     true ->
		    {Type,Dp} =Coord,
		    %% Type =0 наносим урон
		    %%?INFO_MSG("change_health ok: ~p~n", [Dp]),
		    Bonus_result = snappy:change_health(Id,Type,Dp),
		    bonus_apply_analize(State,Bonus_result,Slot1,Slot2,User_Turned,User0,User1);
	_ -> State
    end,
InTime(),
{noreply, NewState};

handle_cast({no_change, Id,User_Turned,User0,User1,Slot1,Slot2,_Coord}, State= #state {id_list = Old_list}) ->
%% отдать результаты использовани бонуса
%% перехода хода быть не должно но я могу победить
InTime = time_for_function(),   
  case lists:member(Id, Old_list) of
     true ->
		    snappy:no_change(Id),

		    User_Next_Turn = case User_Turned of
					 User0 ->  User1;                           
					 User1 ->  User0
				     end,
		    Bin1 = <<"{\"reply\":{\"type\":100,\"body\": [ ">>,
		    Bin3 = <<"]}}">>,
		    Text1=list_to_binary([Bin1,Slot1,Bin3]),
		    Text2=list_to_binary([Bin1,Slot2,Bin3]),
                        send_msg(User_Turned , Text1),
                        send_msg(User_Next_Turn , Text2),
		    %%  переход хода результат использования бонуса 
		    %% так как тут костыль с результатами хода руками задаем
		    %% 6 сохранение хода в результате применения бонуса
		    %% то есть фактичски тот ктоприменяет бонус у того ход и продолжается
		    you_move_send(User_Turned,User_Next_Turn,{{0,6,0},0});
	_ -> ok
    end,

InTime(),
{noreply, State};

handle_cast({timeout_step1, Id,User0,User1}, State) ->
%% осталось 3 минуты до завершения битвы
Jid1=jlib:jid_to_string(User0),
Pid1=gproc:lookup_local_name(Jid1),
Jid2=jlib:jid_to_string(User1),
Pid2=gproc:lookup_local_name(Jid2),
  Text = <<"{\"reply\":{\"type\":100,\"body\": [{\"timeout\":{\"sec\":180}}  ] }}">>,

                        send_msg( User0, Text),
                        send_msg( User1, Text),
    libs_functions:send_log_to_user(User0, 0, {lists:flatten(["До конца боя осталось <b>3 минуты</b>. "])}, "#000000"),
    libs_functions:send_log_to_user(User1, 0, {lists:flatten(["До конца боя осталось <b>3 минуты</b>. "])}, "#000000"),
                        {ok,RefB} =  libs_functions:wrap_cron_once(180,  {gen_server, cast, [self(), {timeout_battle, Id,User0,User1}]}),
    Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
    case Profile_is_live(Pid1) of
	true ->         gen_server:call(Pid1, {change_ref,RefB});
	_ ->    ok
		end,
    case Profile_is_live(Pid2) of
	true ->         gen_server:call(Pid2, {change_ref,RefB});
	_ ->    ok
		end,

{noreply, State};

handle_cast({timeout_battle, Id,User0,User1}, State= #state {id_list = Old_list}) ->
%%
%% {timeout_battle, Id_map,User1,User2}
    ?INFO_MSG("Interval_sec : ~p~n", [Id]),
    
InTime = time_for_function(),
    case lists:member(Id, Old_list) of
	true ->
	    {match_res,_Id_map,_Next_turn,Win_status,_Panel1,_Panel2, map,Map}=snappy:end_timeout(Id),
	    %%Итого по победам  Win_status==
	    %% 0 1 нормальная победа/проигрыш
	    %% 3 4 ошибка какая то
	    %% 5 6 победа/проигрыш изза ошибки
	    %% 7 8 победа/проигрыш по времени

	    case (Win_status) of
                3 ->    
                        ?INFO_MSG(" WTF? timeout_battle Win_status==3: ~p~n", [Map]);
                4 ->    
                        ?INFO_MSG(" WTF? timeout_battle Win_status==4: ~p~n", [Map]),
			end_battle(User0,User1,1);
                M ->  %% победа одного из игроков
		    end_battle(User0,User1,M)
	    end,
	    snappy:free(Id), %% освободили карту
	    %%    Battle_mode =case M  of
	    %% 0 нормальная победа
	    %% 1 победа изза ошибки 
	    %% 2 победа в результате выхода
	    %% 3   Время битвы вышло


	    Battle_count = State#state.battle_count-1, %% уменьшили кол-во битв

	    New_list=Old_list--[Id];
	    _ -> 	    	    Battle_count = State#state.battle_count, 
				    New_list=Old_list
	     end,
NewState = State#state{battle_count  = Battle_count,id_list = New_list},
InTime(),
{noreply, NewState};


handle_cast({exit, Id,User,User0,User1}, State= #state {id_list = Old_list}) ->
%% выход из боя
%% приходит строго во время хода игрока = организационное ограниечение
InTime = time_for_function(),
    case lists:member(Id, Old_list) of
	true ->
	    snappy:free(Id), %% освободили карту
	    Win_user =  case User  of
			    User0  ->  User1;
			    User1 -> User0
			end,  %% противник победил
	    Jid1=jlib:jid_to_string(Win_user),
	    Pid=gproc:lookup_local_name(Jid1),
Profile_call = fun(A) when is_pid(A) -> 
			  case is_process_alive(A) of
			      true ->  gen_server:call(A, {end_battle_nt, 1,2 }) ;
				 _ -> false 
				      end; 
		     (_) -> false 
		  end,
	    Profile_call(Pid), %% отправили сообщение победителю о победе в результате дизертирства
	    Battle_count = State#state.battle_count-1, %% уменьшили кол-во битв
	    New_list=Old_list--[Id];
	    _ -> 	    	    Battle_count = State#state.battle_count, 
				    New_list=Old_list
	     end,
NewState = State#state{battle_count  = Battle_count,id_list = New_list},
InTime(),
{noreply, NewState};

handle_cast(Msg, State) ->
%%I(<0.293.0>:match_profile:324) :  go?? : {struct,  [{<<"id">>,1},{<<"param0">>,1},{<<"param1">>,2}]}
%%en_server:cast(Pid4pro, {query_type, To, From,  Body});  gen_server:cast(Pid, {jid, Jid}),
?INFO_MSG("cast MSG: ~p~n", [Msg]),
{noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% это все остальные запросы
%%--------------------------------------------------------------------
handle_info(Info, State) ->
?INFO_MSG(" packet received handle_info  : ~p~n", [Info]),
{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
?INFO_MSG(" packet received handle_info  : ~p~n", [Reason]),
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
%% Func: any_move_json(N,Type,Time)
%% Description: вспомогательная функция для подготовки json чей ход для отправки
%%--------------------------------------------------------------------

any_move_json(Mturn,Type,Time) ->
    RType =case Type  of
               M when is_integer(M) -> list_to_binary(integer_to_list(M));
	       _ -> <<"1">>    end,
    N =case Mturn  of
               O when is_integer(O) -> list_to_binary(integer_to_list(O));
	       _ -> <<"1">>    end,    

Bin1 = <<"{\"reply\":{\"type\":100,\"body\": [ {\"next_move\":{\"next_move\":">>,
 Bin2 = <<", \"move_type\":">>,
 Bin21 = <<", \"set_timer\":">>,
 Bin3 = <<"}}]}}">>,

%% move_type определяет тип перехода хода или тип сохранения хода
%% 0 уничтожение >3 фишек ход сохранился 
%% 1 ход сменился в результате стандартных правил игры (начало боя, уничтожено 3 фишки)
%% 2 неправильные координаты
%% 3 нет совпадений после хода 
%% 4 кончилось время на ход
%% 5 сработал бонус доп ход
%% 6 сохранение хода в результате применения бонуса

list_to_binary([Bin1,N,Bin2,RType, Bin21,integer_to_list(Time),Bin3]).

%%--------------------------------------------------------------------
%% Func: you_move_send( User1,User2,M)
%% Description: сообщение о ходе одного из игроков
%%--------------------------------------------------------------------
you_move_send(User1,User2,{{0,Move_type,Interval},Flag}) ->
%% Flag определяет время сколько отведено одному ходу
%% выслать что ход 1 игрока {"reply":{"type":100,"body": [ {"next_move":1 }] }}
%%?INFO_MSG("profile MSG: ~p~n", [User1]),
    Timer =0,
%%case Flag  of
%%                  0 -> 30; %% не первый ход в битве
%%		  _ -> 40  end,
%%  сообщение для ожидания окончания хода для игрока при переходе хода 
%% Его ход 3 
%% не его ход 2
 For_time_move_yes = any_move_json(3,Move_type,Timer), %% готовся скоро твой ход
 For_time_move_no = any_move_json(2,Move_type,Timer),

libs_functions:wrap_cron_once(Interval, {p_sender, send_msg, [User1, For_time_move_yes]}),
event_to_profile(User1,1,Flag),
libs_functions:wrap_cron_once(Interval, {p_sender, send_msg, [User2, For_time_move_no]}),
event_to_profile(User2,0,Flag),
ok;

you_move_send( User1,User2,{{1,Move_type,Interval},Flag}) ->
%% выслать что ход 2 игрока {"reply":{"type":100,"body": [ {"next_move":1 }] }}
    Timer =0,
 For_time_move_yes = any_move_json(3,Move_type,Timer), %% готовся скоро твой ход
 For_time_move_no = any_move_json(2,Move_type,Timer),


 libs_functions:wrap_cron_once(Interval, {p_sender, send_msg, [User2, For_time_move_yes]}),
 event_to_profile(User2,1,Flag),
%%                        send_msg(User1, For_time_move_no),
 libs_functions:wrap_cron_once(Interval, {p_sender, send_msg, [User1, For_time_move_no]}),
 event_to_profile(User1,0,Flag),
ok;

you_move_send( _User1,_User2,BB) ->
?INFO_MSG("fail you_move_send: ~p~n", [BB]),
ok.

%%--------------------------------------------------------------------
%% @doc функция извещает профиль игрока о том что ход его
%% @spec event_to_profile (User) -> ok
%% @end
%%--------------------------------------------------------------------
event_to_profile(User,Turn,Flag) ->
Jid1=jlib:jid_to_string(User),
Pid1=gproc:lookup_local_name(Jid1),
Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
    case Profile_is_live(Pid1) of
       true -> gen_server:call(Pid1, {change_turn_on_battle, Turn,Flag});
	_ ->   ?INFO_MSG("change_turn_on_battle: ~p~n", [Jid1])
end.



%%--------------------------------------------------------------------
%% Func: end_battle(User0,User1,M)
%% Description: победа одного из игроков в схватке
%%--------------------------------------------------------------------

end_battle(User0,User1,M) ->
%% выслать обоим что битва закончилась
%% выслать jid победившего игрока что у него победа чтоб ему денег накинули
%%
Jid1=jlib:jid_to_string(User0),
Pid1=gproc:lookup_local_name(Jid1),
Jid2=jlib:jid_to_string(User1),
Pid2=gproc:lookup_local_name(Jid2),

Profile_call = fun(A,B,C) when is_pid(A) -> 
			  case is_process_alive(A) of
			      true ->  gen_server:call(A, {end_battle, B,C }) ;
				 _ -> false 
				      end; 
		     (_,_,_) -> false 
		  end,


%%{"reply":{"type":100,"body":[{"end":{"win_player":1,"result":{"param0":0,"param1":6,"param2":0,"param3":7}}}]}}

%%"result":{"param0":0,"param1":6,"param2":0,"param3":7}

%%Итого по победам  M==
%% 0 1 нормальная победа/проигрыш
%% 3 4 ошибка какая то
%% 5 6 победа/проигрыш изза ошибки
%% 7 8 победа/проигрыш по времени


    Battle_mode =case M  of
		     D when D>6 -> 3; %% по таймеру
		     Q when Q >1 -> 1; %% по ошибочным ходам
		     _ ->0    end,    %% нормальная
 

%% определяем кто победил User1,User2
case (M) of
        U1 when U1 == 0; U1 == 5; U1 == 7 ->
        %% первый игрок победил
%%	?INFO_MSG(" end b : ~p~n", [U1]),
	Profile_call(Pid1,1,Battle_mode),
	Profile_call(Pid2,-1,Battle_mode);
        U2 when U2 == 1; U2 == 6; U2 == 8 ->
%%	?INFO_MSG(" end b : ~p~n", [U2]),
        %% игрок 2 победил
	Profile_call(Pid1,-1,Battle_mode),
	Profile_call(Pid2,1,Battle_mode);
        FF ->
         ?INFO_MSG("Case error: ~p ~p ~p  ~n", [FF,Pid1,Pid2]),
	Profile_call(Pid1,1,1),
	Profile_call(Pid2,1,1)

end,
ok.
%%--------------------------------------------------------------------
%% @doc Функция сбора панели в json
%% @spec get_json_turn_panel(This_user,X0,Y0,X1,Y1,Panel1,Panel2),
%% @end
%%--------------------------------------------------------------------
get_json_turn_panel(This_user,X0,Y0,X1,Y1,{0,_T0x0,_T0x1,_T0x2, _T0x3,_T0x4,_T0x5},{0,_T1x0,_T1x1,_T1x2, _T1x3,_T1x4,_T1x5 })->
      mochijson2:encode({struct,[{<<"reply">>,
                        {struct,[{<<"type">>,100},
                        {<<"body">>,
                        [{struct,[{<<"turn">>,
                           {struct,[
                                {<<"player">>,This_user},
                                {<<"go">>, {struct,[{<<"x0">>,X0},{<<"y0">>,Y0},{<<"x1">>,X1},{<<"y1">>,Y1}]}}
                                ] }}
                        ]}]
                        }]}}]});

get_json_turn_panel(This_user,X0,Y0,X1,Y1,{M0,T0x0,T0x1,T0x2, T0x3,T0x4,T0x5},{M1,T1x0,T1x1,T1x2, T1x3,T1x4,T1x5 })->
    Panels = case {M0,M1}  of
		{1,1} -> 
			{<<"panels">>,{struct, 
				       [
			{<<"panel0">>,{struct, [{<<"tx0">>,T0x0},{<<"tx1">>,T0x1},{<<"tx2">>,T0x2},
					       {<<"tx3">>,T0x3},{<<"tx4">>,T0x4},{<<"tx5">>,T0x5}
					       ] }},
			{<<"panel1">>,{struct, [{<<"tx0">>,T1x0},{<<"tx1">>,T1x1},{<<"tx2">>,T1x2},
					       {<<"tx3">>,T1x3},{<<"tx4">>,T1x4},{<<"tx5">>,T1x5}
					       ] }}] }};
		{1,0} ->
		    	{<<"panels">>,{struct, 
				       [
			{<<"panel0">>,{struct, [{<<"tx0">>,T0x0},{<<"tx1">>,T0x1},{<<"tx2">>,T0x2},
					       {<<"tx3">>,T0x3},{<<"tx4">>,T0x4},{<<"tx5">>,T0x5}
					       ] }}  ] }};
		{0,1} ->  {<<"panels">>,{struct, 
				       [
			{<<"panel1">>,{struct, [{<<"tx0">>,T1x0},{<<"tx1">>,T1x1},{<<"tx2">>,T1x2},
					       {<<"tx3">>,T1x3},{<<"tx4">>,T1x4},{<<"tx5">>,T1x5}
					       ] }}] }};
		_ ->	{<<"panels">>,{struct, [ ] }}
			end,
      mochijson2:encode({struct,[{<<"reply">>,
                        {struct,[{<<"type">>,100},
                        {<<"body">>,
                        [{struct,[{<<"turn">>,
                           {struct,[
                                {<<"player">>,This_user},
                                {<<"go">>, {struct,[{<<"x0">>,X0},{<<"y0">>,Y0},{<<"x1">>,X1},{<<"y1">>,Y1}]}}
                                ] }}
                        ]},
			 {struct,[Panels]}
			]
                        }]}}]}).






%%--------------------------------------------------------------------
%% @doc функция отправки сообщения пользователю
%% @spec send_msg(To,  Text) -> ok
%% @end
%%--------------------------------------------------------------------

send_msg(To,  Text) ->
p_sender:send_msg(To,  Text).


%%--------------------------------------------------------------------
%% @doc функция общая для анализа после применения бонусов
%% @spec bonus_apply_analize(State,Bonus_result,Slot1,Slot2,User_Turned,User0,User1) ->state{}.
%% @end
%%--------------------------------------------------------------------
bonus_apply_analize(State= #state {id_list = Old_list},Bonus_result,Slot1,Slot2,User_Turned,User0,User1) ->
%% специально в ответ клиенту добавляемая строка для определения кто сходил бонусом
%% Slot1 = <<"{"slot":{"id":1, "player":1 "param0":1, "param1":2}}">>
%% Slot2 = <<"{\"slot\":{\"id\":1, \"player\":2 \"param0\":1, \"param1\":2}}">>
%% Bonus_type =  {"slot":{"id":1, "player":1 "param0":1, "param1":2}}

%% парсим результаты
    {match_res,Id,Next_turn,Win_status,_Panel1,_Panel2, map,Map} = Bonus_result,
%?INFO_MSG(" Next_turn done: ~p~n", [Next_turn]),    
%% Определяем чей ход
User_Next_Turn = case User_Turned of
        User0 ->  User1;                           
        User1 ->  User0
  end,

case (Win_status) of

                3 -> 
                        Bin1 = <<"{\"reply\":{\"type\":100,\"body\": [ ">>,
	                Bin2 = <<",">>,
                        Bin3 = <<"]}}">>,
                        Text1=list_to_binary([Bin1,Slot1,Bin2,Map,Bin3]),
                        Text2=list_to_binary([Bin1,Slot2,Bin2,Map,Bin3]),
                        send_msg(User_Turned , Text1),
                        send_msg(User_Next_Turn , Text2),
	                New_list=Old_list,
                        Battle_count = State#state.battle_count,
                        you_move_send( User0,User1,{Next_turn,0});
                4 ->  %% ошибка - переход хода        по идее  такого быть не должно но оставим
                        Battle_count = State#state.battle_count,
	                New_list=Old_list,
                        you_move_send( User0,User1,{Next_turn,0});

                A when A == 5; A == 6 ->   %% победа одного из игроков изза ошибки второго
                     you_move_send( User0,User1,{Next_turn,0}),
                     snappy:free(Id),
                     end_battle(User0,User1,Win_status),
                     New_list=Old_list--[Id],
                     Battle_count = State#state.battle_count-1;

                _ ->  %% победа одного из игроков
                        Bin1 = <<"{\"reply\":{\"type\":100,\"body\": [ ">>,
	                Bin2 = <<",">>,
                        Bin3 = <<"]}}">>,
                        Text1=list_to_binary([Bin1,Slot1,Bin2,Map,Bin3]),
                        Text2=list_to_binary([Bin1,Slot2,Bin2,Map,Bin3]),
                        send_msg(User_Turned , Text1),
                        send_msg(User_Next_Turn , Text2),

                     snappy:free(Id),
                     end_battle(User0,User1,Win_status),
                     New_list=Old_list--[Id],
                     Battle_count = State#state.battle_count-1

end,
State#state{battle_count  = Battle_count, id_list =New_list}.
    


%%encode({free,  I, D}) -> [1,  I, D]; %% Освободить карту
%encode({get_map, I, D}) -> [2, I, D]; %% Дай свободную карту/дай карту такую то
%encode({change, I, D,X1,Y1,X2,Y2}) -> [3, I, D,X1,Y1,X2,Y2]; %%  сходит на такой то карте так то
%encode({destroy_point, I, D,X1,Y1}) -> [4, I, D,X1,Y1]; %%  Уничтожить квадрат с центром по координтатам
%%encode({destroy_line, I, D,Y1}) -> [5, I, D,Y1]. %%  Уничтожить линию

%%decode([Int]) -> Int.

%%--------------------------------------------------------------------
%% @doc логи на длительность хода при условии что прошло достаточно времени
%% @spec
%% @end
%%--------------------------------------------------------------------
time_for_function() ->
    
InTime = erlang:statistics(wall_clock),
fun()->
	Nowtime = erlang:statistics(wall_clock),
%%	Stat_diff = fun({_,I}, {_,A}) ->  I-A end, %% fun Stat_diff(InTime,Nowtime)
        {_A,S1}= Nowtime,
	case S1 of
	S when S>500 ->?INFO_MSG(" wall_clock new : ~p -> ~p ~n", [InTime,Nowtime]);
	_ -> []
	    end
end.
