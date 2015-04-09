%%%-------------------------------------------------------------------
%%% @author Marat Yusupov <marat@yusupov.me> and Михаил Богатырев <ksupipr@yandex.ru>
%%% @doc
%%% модуль проведения турнира 
%%% @end
%%% Created : 19 Feb 2012 by Marat Yusupov
%%%-------------------------------------------------------------------
-module(plg_tournament).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-export([get_query_bet/4,get_query_bet/3]).

%%
%% события:
%% begin начало турнира
%% распределить входящих чуваков по два и отправить их в битву

%% чел оплатил ставку
%% чел не оплатил ставку

%% чел покинул турнир
%% битва между двумя игроками закончилась с таким то результатом

%% турнир завершен
%%%%%%%%%%%%%%%%%
%%Состояния 
%%0 Ждем ставку
%%1 ждем начало боя
%%2 бой
%%3 Победил
%%4 Проиграл
%%5 Вышел



-include("shared.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("poligon.hrl").
-record(user, ?PLG_TOURNAMET_USER_REC ).
%% структура турнира после первой ставки и перед тем как все отыграют первый бой
%% 1 чел вылетел проигра например 
%% 0 ждем ставку
%% _ чел поставил


-record(state, {
	   activ_users=[]  %% список акнивных jid (тех кто не вылетел с турнира) для рассылки сообщений по ним

	  
	  , num_rec_user %% полное состояние турнира от начала до конца num -> #user{} 
	  %% num_rec_user используется строго для отрисовки состояния турнира для игрока
	  %% то есть тут игроки должны быть на той позиции в которой находятся и на которой они свалили например не осилив ставку
	  
	  , turn_status %% полное состояние турнира  #Jid{} | 0 ставки еще не было | 1 противник вышел из схватки не сделал схватку
	  
	  , ident_num  = [] %% соответсвие jid к позиции в турнире [{jid,num}|_]  под jid в принципе можно понимать любой идентификатор
	  , ident_perk = [] %% соответсвие jid к описанию перков игрока [{jid, UserPerk}|_]
	  , ident_user = [] %% jid -> #user{}пользователь | 0 

	  , standartBetx16=50  %%% так же надо сменить в профиле о ставках
	  , standartBetx8=100
	  , standartBetx4=200
	  , standartBetx2=300
          , standartAddScore = 14

	  , ss %% self pid
	  , tour_start_time
%%	  , log_pro_pid
	  , status=0

}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
%%Первый аргумент - {local, ch3} – определяет имя сервера. В нашем случае сервер
%%будет локально зарегистрирован без имени и к нему надо будет обращатся по пиду
    gen_server:start_link( ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Name = now_tour,
    pg2:create(Name),
    pg2:join(Name, self()), 
Current       = calendar:local_time(), %% {{2012,4,16},{9,44,33}}
%%Now_timestamp = calendar:datetime_to_gregorian_seconds(Current),
    {ok, #state{tour_start_time = Current }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_time, _From, State = #state{tour_start_time = Btime}) ->
    Current       = calendar:local_time(), %% {{2012,4,16},{9,44,33}}
    Reply = calendar:time_difference(Current,  Btime ),
 {reply, Reply, State};
handle_call(dump_state, _From, State=#state{tour_start_time = _Btime}) ->
?INFO_MSG("dump_state: ~p~n", [State]),    
 {reply, State, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
 {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({begin_tourn, Users}, State = #state{ standartBetx16 =StartBet }) ->

%%{ok,Log_me}=libs_log_2file:start_link(self(),"/tmp/t"),


    case length(Users) of
	16 ->
	    %% начать турнир
	    Ss=self(),
	    X1=State#state{
		 status=1 
	       , num_rec_user = array:new([{size, 32}, {default, 0}, {fixed, true}])
               , turn_status  = array:new([{size, 32}, {default, 0}, {fixed, true}])
%%	       , log_pro_pid  = Log_me
	       ,ss=Ss 
	       },



	    P8 = fun(A = #user{num = Num,jid= Jid}, AccIn ) -> 
		 %% отправки сообщения и сбор Jid в один результирующий list в State
                 %% данным запросом мы привели профиль в состояние в турнире из состояния в ожидании турнира
		 Status = 1,
                 AccIn1=add_user(A,AccIn,Status,Num),
                 Res = add_jid(Jid,AccIn1), %% добавляет в число активных игроков
			 get_query_bet(Jid, StartBet,Num),
			  Res
		 end,
	    X2 = lists:foldl(P8, X1 , Users),
	    {noreply, X2};
        _ ->{stop, more_users, State#state{activ_users = Users}}
		end;


handle_cast({bet,_LNum,UserPerk,Jid}, State=#state{ ident_perk = List}) ->
%% {user_perk,Score1,_Avia1,_Brone1,_Soldier1,_Bet} = UserPerk,
%% Успешная ставка 
   Num = get_num_jid(Jid,State),
%%?INFO_MSG("Num from Serrv  ~p~n", [Num]),
%%?INFO_MSG("Num from Serrv  ~p~n", [LNum]),
%%   gen_server:call(Log_me,{write,lists:concat(["add bet num:", Num])}),

   User4Battle = {UserPerk,Jid},
   Num2=  get_enemy( Num ),
 %% установить перк 
  LOut = lists:keystore(Jid, 1, List, {Jid,UserPerk}),

 %% подвинуть себя в массиве состояния турнира num_rec_user
    User = get_user(Jid,State),
    X1=State#state{ident_perk = LOut , num_rec_user=array_set(State#state.num_rec_user, Num, User)
		  , turn_status=array_set(State#state.turn_status, Num, Jid) },

    case array_get(X1#state.turn_status, Num2) of
	0 -> {noreply, X1}; %% ставки противника еще нет
	1 ->  %% противник вышел из схватки Мы победили в данной схватке
             win_result_analize(Num, User4Battle,X1);
	User4Battle_jid -> %% Отправляем их в битву
	    User4Battle2 = {get_perk(User4Battle_jid, X1),User4Battle_jid},
	    go_battle(User4Battle,User4Battle2,X1,Num2),
	    {noreply, X1}
end;

handle_cast({bet_no, _, _UserPerk,Jid}, State) ->
%%  ставка провалена =#state{log_pro_pid=Log_me }
%% указать что чел проиграл 

   Num = get_num_jid(Jid,State),
%%   gen_server:call(Log_me,{write,lists:concat(["bet_no num:", Num])}),
%% взять профиль игрока из старого состояния турнира
    User = get_user(Jid,State),
    State_N= set_user_status(User,State,1,Num), %% пометить что он вылетел в истории и состояние #user в хранилище
    X0=del_jid(Jid,State_N), %% из списка активных jid  убрали
    all_send_change(Num, -1,X0),
    Num2=  get_enemy( Num ),     %% текущее состояние
    X1=X0#state{turn_status=array_set(X0#state.turn_status, Num, 1)},%% пометили что мы вне игры

    case array_get(X1#state.turn_status, Num2) of
	0 -> {noreply, X1}; %% ставки противника еще нет уходим в ожидание результатов ставки
	1 ->  %% противник тож слился надо проанализировать состояние турнира и расставить либо завершить турнир
	    find_any_warrior(Num,X1);
	Jid2 -> %% Он нас победил
	     User2 = {get_perk(Jid2, X1),Jid2},
             win_result_analize(Num2, User2,X1)
    end;

handle_cast({exit, LNum}, State) ->

%%   Игрок вышел из турнира
%% указать что чел проиграл 
%% взять профиль игрока из старого состояния турнира
    User = (array_get(State#state.num_rec_user, LNum)),
    Jid = User#user.jid,
    Num = get_num_jid(Jid,State),
%%   gen_server:call(Log_me,{write,lists:concat(["exit num:", Num])}),
%% пометить что он вылетел
    X00 = del_jid(User#user.jid,State), %% из списка активных jid  убрали
    all_send_change(Num, -1,X00),
  %% пометить в истории что вышли
    %%1 ожидание результатов битвы
    %%	2 победа
    %%	3 проигрыш
    X0 = set_user_status(User,X00,3,Num), 
    Num2 = get_enemy( Num ),
    X1=X0#state{turn_status=array_set(X0#state.turn_status, Num, 1)}, %% пометили в реале что вылетели


    case array_get(X1#state.turn_status, Num2) of
	0 -> {noreply, X1}; %% ставки противника еще нет уходим в ожидание результатов ставки
	1 ->  %% противник тож слился надо проанализировать состояние турнира и расставить либо завершить турнир
	    find_any_warrior(Num,X1);
	Jid2 -> %% Он нас победил
	     User2 = {get_perk(Jid2, X1),Jid2},
             win_result_analize(Num2, User2,X1)
    end;

handle_cast({show, User },  State) ->
%% отобразить список народа 

  %%  fun((Index :: array_indx(), Value :: term(), Acc :: A) -> B)
    P8 = fun(_, V, AccIn) -> 
		 %% анонимная функция для расстановки запятых между обьектами
		 case V of
		    V when  is_tuple(V), element(1, V) == user ->
              		 A1 = unicode:characters_to_binary(plg_go_turnir:get_player_json(V)), 
	            	 B =  unicode:characters_to_binary(","), 
			  case AccIn of [] ->  [ A1 | AccIn ];
			      _ ->   M = [B | AccIn],[A1 | M ] 
			  end;
		     _ -> AccIn
                 end		 
	 end,

    NewUserJson_bin = array:foldl(P8,[] , State#state.num_rec_user),
    Bin1 = <<"{\"reply\":{\"type\":150,\"body\": { \"turnir_queue\": {      \"list\": [ ">>,
    Bin3 = <<"] } }}}">>,
    Text1=list_to_binary([Bin1,NewUserJson_bin,Bin3]), 
    send_msg(User,  Text1),    

{noreply, State};

handle_cast({win,NumU,Jid}, State = #state{standartBetx2 = Bet}) when NumU >=29 ->
%% чел победил =#state{log_pro_pid=Log_me}
%% Получаем свой профиль 
%% хз че делать - Завершаем турнир

%%    Num = get_num_jid(Jid,State), %% позиция в турнире
%%    gen_server:call(Log_me,{write,lists:concat(["normal win num:", Num])}),

    Prize = Bet + (Bet div 2),
    
Text=mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,158},
                        {<<"body">>,
			 {struct,[{<<"turnir_win">>,
			   {struct,[{<<"type">>,<<"champion of the tournament">>},
                            {<<"prize">>,Prize},
                            {<<"state">>,31},
                            {<<"val">>,158}
                          ]}}]}
                        }]}}]}),
 send_msg(Jid,  Text),
{stop, normal, State};

handle_cast({win,_,Jid}, State) ->
%% чел победил 
%% ПРотивник проиграл
    Num = get_num_jid(Jid,State),
    Num2=  get_enemy( Num ),
%%?INFO_MSG("win Turn, Num: ~p Jid:  ~p~n", [Num,JidE]),    
%% взять профиль игрока из старого состояния турнира


    JidE = array_get(State#state.turn_status, Num2), %% jid противника

case JidE of
    V when  is_tuple(V), element(1, V) == jid ->
	%% пометить что он вылетел
	User2 = get_user(JidE,State), %% получили противника #user{}
	State_N= set_user_status(User2,State,3,Num2), %%пометили в истории что он вылетел
	all_send_change(Num2,-1,State_N),
	X0=del_jid(JidE,State_N),
	%% Получаем свой профиль

        User1 = {get_perk(Jid, X0),Jid},
	%% Мы победили ищем другого противника
	?INFO_MSG("Win user: ~p~n", [Num]),    
	win_result_analize(Num, User1,X0);
    _ ->
        ?INFO_MSG("WTF: ~p~n", [JidE]), 
        User1 = {get_perk(Jid, State),Jid},
	win_result_analize(Num, User1,State)

end;

handle_cast(Msg, State) ->
%%O log it
%%?INFO_MSG("profile MSG: ~p~n", [Msg]),
?DEBUG("packet received handle_cast : ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(normal, _State) ->
%% турнир успешно завершился :) Todo Поздравить победителя
%%?INFO_MSG("Tournament done!!: ~p~n", [normal]),    
%%   gen_server:call(Log_me,{write,"normal termenate"}),
  Name = now_tour, 
  pg2:leave(Name, self()),
ok;
terminate(Reason, State) ->

?INFO_MSG("Tournament unnormal stop: ~p~n", [Reason]),   
?INFO_MSG("Tournament unnormal stop: ~p~n", [State]), 

%% разослать всем игрокам что турнир завершился с ошибкой
Text=mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,156},
                        {<<"body">>,
			 {struct,[{<<"turnir_out">>,
			   {struct,[{<<"type">>,<<"break tournament">>},{<<"val">>,156}]}}]}
                        
                        }]}}]}),

    P8 = fun(A) -> 
		 %% отправки сообщения
		 send_msg(A,  Text)
	 end,
lists:foreach(P8, State#state.activ_users),
%% TODO надо бы и профилям сообщить что челы выпали из турнира
  Name = now_tour, 
  pg2:leave(Name, self())
.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc функция анализа до отправки в битву 
%% @spec new_battle({new,  Jid1, User1,Jid2,User2,  AddScore,Battle_type, Num}) ->ok
%% @end
%%--------------------------------------------------------------------
new_battle({new,  User1,Num1,User2,Num2,Reit,Type,Num_of_tourn},State)  -> 
   Profile_call = fun(A,B,C) when is_pid(A) -> 
			  case is_process_alive(A) of
			      true ->  gen_server:call(A, {end_battle, B,C }) ;
				 _ -> false 
				      end; 
		     (_,_,_) -> false 
		  end,

    case {Num1,Num2}  of
	{{user_perk,_Score1,_Soldier1,_Brone1,_Avia1,_Bet1,_Profile1},
	{user_perk,_Score2,_Soldier2,_Brone2,_Avia2,_Bet2,_Profile2}} ->
	    Name = <<"battle_serv">>,
	    Ali=gproc:lookup_local_name(Name),
	    gen_server:cast(Ali, {new,  User1,Num1,User2,Num2,Reit,Type,Num_of_tourn});
	    
	{_,{user_perk,_Score2,_Soldier2,_Brone2,_Avia2,_Bet2,_Profile2}} -> 
	    %% победил второй ибо первый косячный профиль
	    Jid1=jlib:jid_to_string(User1),
	    Pid1=gproc:lookup_local_name(Jid1),
	    Jid2=jlib:jid_to_string(User2),
	    Pid2=gproc:lookup_local_name(Jid2),
    
	    Profile_call(Pid1,-1,1),
	    Profile_call(Pid2,1,1)
	    ;
	_ -> 
	    %% победил первый ибо второй косячный профиль
	    %% Или оба косячные и пофик кто победил
	    Jid1=jlib:jid_to_string(User1),
	    Pid1=gproc:lookup_local_name(Jid1),
	    Jid2=jlib:jid_to_string(User2),
	    Pid2=gproc:lookup_local_name(Jid2),
	    Profile_call(Pid2,-1,1),
	    case Profile_call(Pid1,1,1) of
		    false -> 
			%% профиль который должен победить оффлайн
			%% надо самому себе прислать что чел победил 
		        NumUU = get_num_jid(User1,State),
		        gen_server:cast({win,NumUU,User1});
		    _ -> ok
		   end
	    	
	end.





%%--------------------------------------------------------------------
%% @doc функция которая анализирует не завершилась ли битва и подыскивает новую позицию игроку
%% @spec win_result_analize(Num, User,State)->{noreply, State} |
%%                                     {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
win_result_analize(Num, _User,State) when Num >= 29 ->
%% битва завершилась просим gen_serv убить нас
%% известить победителя
%%       {_UserPerk,Jid}=User,
%% Jid1=jlib:jid_to_string(Jid),
%% Pid1=gproc:lookup_local_name(Jid1),
?INFO_MSG("end win Turn, Num: ~p Jid:  ~p~n", [Num,1]),    
% gen_server:call(Pid1, {user_win_tourn,Num}),
%%all_send_change(31,User,State),
{noreply, State};

win_result_analize(Num, {_UserPerk,Jid},State) ->
%% Num нахожусь в позиции Num Это доверенный NUM 
%% Победил 
NextNum = next_num(Num),
%% взять профиль игрока из старого состояния турнира
   Jid = get_num_jid(Num,State),
   User_rec = get_user(Jid,State),

   X1 =  set_user_status(User_rec,State,1,NextNum), %% состояние игрока в истории  и состоянии игрока #user{} поставил в ожидание
?INFO_MSG("Usr win: ~p [~p] |->  ~p~n", [Jid,Num,NextNum]),    
%% прописываем событие перехода в следующий слот


    Bet = get_bet(NextNum,X1),
    Ss=State#state.ss,

    X2= set_num_jid(NextNum,Jid,X1),
    libs_functions:wrap_cron_once(?END_BATTLE_SLEEP_INTERVAL+7, {plg_tournament, get_query_bet, [Jid, Bet,NextNum,Ss]}),

    all_send_change(Num,NextNum,State),
{noreply, X2}.
    
%%--------------------------------------------------------------------
%% @doc функция лавинообразного движения проигрышей 
%% @spec find_any_warrior(NumNext,State) ->{noreply, State} |
%%         {stop, done, State}
%% @end
%%--------------------------------------------------------------------
find_any_warrior(Num,State) when Num >= 29 ->
%% ибо соперник тож слился все слились турнир завершен
%% противник вышел при том что его противник то же вышел

?INFO_MSG("find_any_warrior Num >= 29 : ~p~n", [Num]),
{noreply, State};
%%{stop, done, State};
find_any_warrior(Num,State)->
 %% это состояние когда слился и Num и EnemyNum
    NextNum=next_num(Num),
    X2=State#state{turn_status=array_set(State#state.turn_status, NextNum, 1)}, %% прописали 

    Num2 =  get_enemy( NextNum ),
    
 case array_get(X2#state.turn_status, Num2) of
	0 -> {noreply, X2}; %% ставки противника еще нет уходим в ожидание результатов ставки
	1 ->  %% противник тож слился надо проанализировать состояние турнира и расставить либо завершить турнир
            %%  тут видимо должно быть следующий NUM2
	    find_any_warrior(NextNum,X2);
	Jid2 -> %% Он нас победил

	     User2 = {get_perk(Jid2, X2),Jid2},
             win_result_analize(Num2, User2,X2)
	end.




    
%%--------------------------------------------------------------------
%% @doc Отправляем двух игроков в битву турнира
%% @spec go_battle(User1,User2,State) -> ok.
%% @end
%%--------------------------------------------------------------------

go_battle({User1,Jid1},{User2,Jid2},State = #state{ standartAddScore =AddScore},Num) ->
%%  отправить двих в битву
%%{user_perk,_Score1,_Avia1,_Brone1,_Soldier1,_Bet,_Profile1} = User1,
%%{user_perk,_Score2,_Avia2,_Brone2,_Soldier2,_Bet,_Profile2} = User2,
Ident = random:uniform(1000),
?INFO_MSG("BAttle [~p]: ~p vs ~p~n", [Ident,Jid1,Jid2]), 
Name = <<"battle_serv">>,
Ali=gproc:lookup_local_name(Name),
%%   1 - поединок  2 - дуэль 3 - турнир
Battle_type = 3,

%%   gen_server:call(Log_me,{write,lists:concat(["goto Battle:", Ident])}),
%%

case {( random:uniform(2)==1),is_pid(Ali)} of
        {true, true}  ->
        %% первым ходит user1
        new_battle({new,  Jid1, User1,Jid2,User2,  AddScore,Battle_type, Num},State);
        {_,true} ->
        %% первым ходит user2
         new_battle({new,  Jid2, User2,Jid1,User1,AddScore,Battle_type, Num},State);
        {_,false} ->
        %% нужный нас сервер не запущен :(
        %%?INFO_MSG("match_rbattle me this line go nex : ~p~n", [123]),
        %% {"reply":{"type":150,"body": { "error":{"type":"break tournament","val":153} } }}
                        Text=mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,156},
                        {<<"body">>,
			 {struct,[{<<"turnir_out">>,
			   {struct,[{<<"type">>,<<"break tournament">>},{<<"val">>,156}]}}]}
                        
                        }]}}]}),
	        send_msg(Jid1, Text),
	        send_msg(Jid2, Text),
                        (catch match_battle:start_link())
    end.

%%--------------------------------------------------------------------
%% @doc Добавляет во все нужные списки
%% @spec add_user(User,State,Status,Num) -> #state{}.
%% @end
%%--------------------------------------------------------------------

add_user(User,State=#state{ident_user = List},Status,Num) when is_tuple(User) andalso  element(1, User) == user  ->
    UserN = User#user{num=Num,slot_state=Status}, 
    Jid = User#user.jid,
    %% добавить в актуальные  jid -> num ; добавить в историю num num_jid
    %% добавить jid -> perk для боя Перк пользователя я узнаю только после ставки
    %% добавить jid -> #user{} для описания игрка клиенту
  X1=  State#state{num_rec_user=array_set(State#state.num_rec_user, Num, UserN), 
	     ident_user = [{Jid,UserN}|List]
	    %% это не ндао ибо ставки от игрока еще нет ,turn_status=array_set(State#state.turn_status, Num, Jid)
	    },
set_num_jid(Num,Jid,X1);

add_user(_User,State,_Status,_Num) ->

 State.
%%--------------------------------------------------------------------
%% @doc устанавливает состояние игрока  в общий истории турнира  и в состоянии игрока в jid -> #user{}
%% @spec set_user_status(User,State,Status,Num) -> #state{}
%% @end
%%--------------------------------------------------------------------
set_user_status(User,State=#state{ident_user = List, num_rec_user=Array },Status,Num) when is_tuple(User) andalso  element(1, User) == user  ->

    UserN = User#user{num=Num,slot_state=Status},
    Jid = User#user.jid,
    LOut = lists:keystore(Jid, 1, List, {Jid,UserN}),  
    AOut= array_set(Array, Num, UserN), %% в истории
State#state{ident_user = LOut, num_rec_user=AOut}.



%%--------------------------------------------------------------------
%% @doc Добавляет в список активных jid игрока
%% @spec add_jid(Jid) ->#state{}.
%% @end
%%--------------------------------------------------------------------
add_jid(Jid,State=#state{activ_users=List}) ->
%% Добавить в общий массив Jid-ов
Last = lists:append(List,[Jid]),
State#state{ activ_users = Last }.
%%--------------------------------------------------------------------
%% @doc Удаляет выбышего чела из списка активных участников Турнира
%% @spec del_jid(Jid) ->ok.
%% @end
%%--------------------------------------------------------------------
del_jid(Jid,State) ->
%% Удалить Jid
 Even = fun(X) -> X =/= Jid end,
 X1_all = lists:filter(Even, State#state.activ_users),
%% так как в эту функцию попадает только тогда когда игрок в очереди то 
%% функция выше явно отработала и удалила одного игрока из очереди
 State#state{ activ_users = X1_all }.

%%--------------------------------------------------------------------
%% @doc возвращает следующий уровень в иерархии турнира
%% @spec next_num(Num) -> integer()
%% @end
%%--------------------------------------------------------------------
next_num(N) when N =< 2 ->
    17;
next_num(N) when N==3;N==4 ->
    18;
next_num(N) when N==5;N==6 ->
    19;
next_num(N) when N==7; N==8 ->
    20;
next_num(N) when N==9; N==10->
    21;
next_num(N) when N==11; N==12->
    22;
next_num(N) when N==13; N==14->
    23;
next_num(N) when N==15; N==16->
    24;
next_num(N) when N==17; N==18->
    25;
next_num(N) when N==19; N==20->
    26;
next_num(N) when N==21; N==22->
    27;
next_num(N) when N==23; N==24->
    28;
next_num(N) when N==25; N==26->
    29;
next_num(N) when N==27; N==28->
    30;
next_num(_N) -> 31.
%%--------------------------------------------------------------------
%% @doc Ставки при турнире
%% @spec get_bet(N) -> integer()
%% @end
%%--------------------------------------------------------------------

get_bet(N,State) when N =< 16 -> 
    State#state.standartBetx16;
get_bet(N,State) when N >= 17,N =< 24 -> 
    State#state.standartBetx8;
get_bet(N,State) when N >= 25,N =< 28 -> 
    State#state.standartBetx4;
get_bet(_N,State) ->     
 State#state.standartBetx2.
    

    
%%--------------------------------------------------------------------
%% @doc рассылает изменение положения  пользователя в списке турнира
%% @spec  all_send_change(OldNum, NewNum) -> ok.
%% @end
%%--------------------------------------------------------------------
all_send_change(OldNum, NewNum,State)->
%% 
                        Text=mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,155},
                        {<<"body">>,
			 {struct,[{<<"turnir_change">>,
			   {struct,[{<<"oldnum">>,OldNum},{<<"newnum">>,NewNum}]}}]}
                        
                        }]}}]}),
    P8 =case NewNum of
            -1 ->     
		  fun(A) -> 
		 %% отправки сообщения
               libs_functions:wrap_cron_once(?END_BATTLE_SLEEP_INTERVAL+4, {p_sender, send_msg, [A, Text]})
		  end;
	    _ ->
		fun(A) -> 
		 %% отправки сообщения
		 send_msg(A,  Text)
		end
	end,
    


lists:foreach(P8, State#state.activ_users).

%--------------------------------------------------------------------
%% Func: send_msg(To,  Text) 
%% Description: send Text to user small form
%%--------------------------------------------------------------------
send_msg([],  _) ->
    ok;
send_msg(_To,  []) ->
    ok;
send_msg(To,  Text) ->
p_sender:send_msg(To,  Text).



%%--------------------------------------------------------------------
%% @doc функция запроса у пользователя ставки
%% @spec  get_query_bet(Jid, Bet,Num) -> ok.
%% @end
%%--------------------------------------------------------------------
get_query_bet(Jid, Bet,Num) ->
     Ss=self(),   
 get_query_bet(Jid, Bet,Num,Ss).

get_query_bet(Jid, Bet,Num,Ss) ->
     Jid1=jlib:jid_to_string(Jid),
     Pid1=gproc:lookup_local_name(Jid1),
     Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
    case Profile_is_live(Pid1) of
	true ->     
	    gen_server:cast(Pid1, {get_bet4tourn,Bet,Num,Ss});
	_ ->        gen_server:cast(Ss, {bet_no, Num, Num,Jid})
		end.

%%--------------------------------------------------------------------
%% @doc  дай номер соперника
%% @spec get_enemy( Num ) -> integer().
%% @end
%%--------------------------------------------------------------------
get_enemy( Num ) ->
case Num rem 2 of
        0 -> %% четное число 
	    Num-1;
	_ ->
	    Num+1
end.

%%--------------------------------------------------------------------
%% @doc  функция установки элемента массива
%% @spec array_set(Array, I, Value) -> array()
%% @end
%%--------------------------------------------------------------------

array_set(Array, I, Value) ->
    %% array indexing starts at 0
   array:set(I, Value, Array).
%%--------------------------------------------------------------------
%% @doc функция получения элемента массива
%% @spec  array_get(Array, I) -> any()
%% @end
%%--------------------------------------------------------------------

array_get(Array, I) ->
    array:get(I, Array).


%%--------------------------------------------------------------------
%% @doc  устанавливает num на определенный jid и в массиве истории Турнира и изменияет num в ident_user
%% @spec set_num_jid(Num,Jid,State=#state{  }) ->#state{}
%% @end
%%--------------------------------------------------------------------
set_num_jid(Num,Jid,State=#state{ ident_num = List, num_rec_user = Array, ident_user = ListUser }) ->
%%?INFO_MSG("ident_num: ~p~n", [List]), 
    LOut = lists:keystore(Jid, 1, List, {Jid,Num}), %% сохранение или замена если такой ключ уже есть 

%% замена позиции в хранилище #user
    {value, {_,User}, LU2} =  lists:keytake(Jid, 1, ListUser), 
    UserN = User#user{num=Num}, 
    AOut = array:set(Num, UserN, Array),
    LUOut = [{Jid,UserN} | LU2 ],

    State#state{ ident_num = LOut, num_rec_user = AOut,ident_user = LUOut }.
%%--------------------------------------------------------------------
%% @doc выдает позицию jid-а или jid на этой позиции    num выдается только строго для невыбовших jid
%% @spec get_num_jid(Key,#state{})  ->integer() | #jid{}
%% @end
%%--------------------------------------------------------------------
get_num_jid(Key,_State=#state{ ident_num = List, num_rec_user = Array}) when is_integer(Key) ->
    case lists:keysearch(Key, 2, List)  of
	{value, {Tuple,_}} ->Tuple ;
	false ->     A = array:get(Key, Array),
		     A#user.jid
				      end;
get_num_jid(Key,_State=#state{ ident_num = List }) when is_tuple(Key) ->
    case lists:keysearch(Key, 1, List)  of
	{value,{_,Tuple}} ->Tuple ;
	false ->     -1
 end.

%%--------------------------------------------------------------------
%% @doc отадет перки по jid
%% @spec get_perk(Key,State=#state{ ident_perk = List}) -> #perk{}
%% @end
%%--------------------------------------------------------------------
get_perk(Key,_State=#state{ ident_perk = List}) when is_tuple(Key)->
    case lists:keysearch(Key, 1, List)  of
	{value, {_,Tuple}} -> Tuple ;
	false ->  none
 end.
%%--------------------------------------------------------------------
%% @doc отдает описание пользователя по jid 
%% @spec get_user(Key,State=#state{ident_user = List }) -> #user{}
%% @end
%%--------------------------------------------------------------------
get_user(Key,_State=#state{ ident_user = List }) ->
    case lists:keysearch(Key, 1, List)  of
	{value, {_,Tuple}} -> Tuple ;
	false ->  none
 end.

