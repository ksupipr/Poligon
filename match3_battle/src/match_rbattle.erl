%%%-------------------------------------------------------------------
%%% File    :
%%% Author  : Marat Yusupov
%%% Description : Подсистема отправки битву двух игроков.
%%% Copyright   : Marat Yusupov marat@yusupov.me
%%% Created :
%%%-------------------------------------------------------------------
-module(match_rbattle).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("ejabberd.hrl").
-include("jlib.hrl").


-define(SERVER, ?MODULE).

-record(state, {
standartBet=50,
standartAddScore=5,
user1 = 0,
num1 %%позиция игрока для определения чей ход
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
%% Если имя будет опущено, то gen_server не будет зарегистрирован.
%% поэтому мы запускаем без имени то есть имя будет pid
  gen_server:start_link(?MODULE, [], []).

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
Name="random_battle",
gproc:add_local_name(Name),
State = #state{},
{ok, State}.

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


handle_cast({adduser,First_user,User2_num,MeJid},  State) ->
{user_perk,_Score1,_Avia1,_Brone1,_Soldier1,Bet,_Profile} = User2_num,

        case  Bet==State#state.standartBet of
        true ->
                %% TODO Надо учесть что чел не тот же

                %% Сообщение что ты в очереди
                Ask1 = <<"0LrQsNC60L7QuSDQvdC40YLRjCDRgtC10LrRgdGCINC/0YDQviDRh9GC0L4g0LLRiyDQstGB0YLQsNC70Lgg0LIg0L7Rh9C10YDQtdC00Ywg0Lgg0LHQu9CwINCx0LvQsCA=">>,
                        Text=mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,20},
                        {<<"body">>,
                        {struct,[{<<"queue">>,
                        {struct,[{<<"type">>,1},
                        {<<"bet">>,State#state.standartBet},
                        {<<"coin_war">>,State#state.standartBet div 2},
                        {<<"rating">>,State#state.standartAddScore},
                        {<<"msg">>,{struct,[{<<"id">>,2},{<<"comment">>,Ask1}]}}
                        ]}}]}
                        }]}}]})
                        ,


                case  State#state.user1 of
                0 ->   
                        echo(MeJid, First_user, Text),
                        NewState = State#state{user1=First_user,num1=User2_num},
			%%  чел стал в очередь первым 

                        %% TODO таймер!
                        %%Ask1=<<"Для вас не нашлось соперника может хотите поиграть в тренировочный бой?">>,
                        %%Me={jid,"t90c","im.xlab.su",[],"t90c","im.xlab.su",[]},
                        %%{"off_queue":{"ask":{"id":1,"comment":"Для вас не нашлось соперника может хотите поиграть в тренировочный бой?"} }}
                        %%Text=mochijson2:encode({struct,[{<<"off_queue">>,[{struct,
                        %%[{<<"ask">>,{struct,[{<<"id">>,1},{<<"comment">>,Ask1}]}}
                        %%]}]}]}),
                        %%echo(Me, First_user, Text)
                        Jid1=jlib:jid_to_string(First_user),
                        Pid1=gproc:lookup_local_name(Jid1),
                        %% надемся что пид корректен и меняем состояние профиля он в битве
                        gen_server:call(Pid1, {local_state_on_battle, 2});

                User2 when User2 =/= First_user ->
                        echo(MeJid, First_user, Text),
                        Jid1=jlib:jid_to_string(First_user),
                        Pid1=gproc:lookup_local_name(Jid1),
                        %% надемся что пид корректен и маркируем что чел в очереди а битву
                        gen_server:call(Pid1, {local_state_on_battle, 2}),
                        go_battle(First_user,User2_num,State,MeJid),
                        NewState = State#state{user1=0};
		    _ -> NewState=State,   Text1=mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,20},
                        {<<"body">>,
                        {struct,[{<<"queue">>,
                        {struct,[{<<"type">>,2},
                        {<<"msg">>,{struct,[{<<"id">>,22},{<<"comment">>,Ask1}]}}
                        ]}}]}
                        }]}}]})
                        ,   send_msg(First_user,  Text1)

                end;
        _ -> %% ставка не равна страндартной ставке
        NewState=State
        end,
{noreply, NewState};

handle_cast({deluser,First_user,MeJid},  State) ->
%%Выход из очереди {deluser,First_user,MeJID4send}
case  State#state.user1 of
                First_user ->
                        NewState = State#state{user1=0},

                %% Теперь Вы вне очереди на случайный бой
                Ask1 = <<"0KLQtdC*0LXRgNGMINCS0Ysg0LLQvdC1INC+0YfQtdGA0LXQtNC4INC90LAg0YHQu9GD0YfQsNC50L3Ri9C5INCx0L7QuQ==">>,
                        Result = mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,21},
                        {<<"body">>,
                        {struct,[{<<"queue">>,
                        {struct,[{<<"type">>,3},
                        {<<"msg">>,{struct,[{<<"id">>,22},{<<"comment">>,Ask1}]}}
                        ]}}]}
                        }]}}]}),
                        %% обнуляем очередь
                        Jid1=jlib:jid_to_string(First_user),
                        Pid1=gproc:lookup_local_name(Jid1),
                        %% надемся что пид корректен и меняем состояние профиля он в битве
                        gen_server:call(Pid1, {local_state_on_battle, 0});

                _ ->
                        %% это не он в очереди
                        %% ?INFO_MSG(": e xit empty ~s", [First_user]),
                        NewState=State,
                        Ask1 = <<"0JLRiyDQvdC1INC80L7QttC10YLQtSDQv9C+0LrQuNC90YPRgtGMINC+0YfQtdGA0LXQtNGMINGB0LvRg9GH0LDQudC90YvRhSDQsdC+0LXQsg==">>,
                        Result = mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,21},
                        {<<"body">>,
                        {struct,[{<<"queue">>,
                        {struct,[{<<"type">>,3},
                        {<<"msg">>,{struct,[{<<"id">>,21},{<<"comment">>,Ask1}]}}
                        ]}}]}
                        }]}}]})
                end,
                echo(MeJid, First_user, Result),
{noreply, NewState};
handle_cast({timeout,First_user},  State) ->
%%Чел слишком долго в очереди
case  State#state.user1 of
                First_user ->
                        NewState = State#state{user1=0},

                %% Теперь Вы вне очереди на случайный бой
                        %%Ask1=<<"Для вас не нашлось соперника может хотите поиграть в тренировочный бой?">>,
                        %%Me={jid,"t90c","im.xlab.su",[],"t90c","im.xlab.su",[]},
                        %%{"off_queue":{"ask":{"id":1,"comment":"Для вас не нашлось соперника может хотите поиграть в тренировочный бой?"} }}
                        %%Text=mochijson2:encode({struct,[{<<"off_queue">>,[{struct,
                        %%[{<<"ask">>,{struct,[{<<"id">>,1},{<<"comment">>,Ask1}]}}
                        %%]}]}]}),
                        %%echo(Me, First_user, Text)
    %%            Ask1 = <<"0KLQtdC*0LXRgNGMINCS0Ysg0LLQvdC1INC+0YfQtdGA0LXQtNC4INC90LAg0YHQu9GD0YfQsNC50L3Ri9C5INCx0L7QuQ==">>,
    %%                    Result = mochijson2:encode(
    %%                    {struct,[{<<"reply">>,
    %%                    {struct,[{<<"type">>,21},
    %%                    {<<"body">>,
    %%                    {struct,[{<<"queue">>,
    %%                    {struct,[{<<"type">>,3},
    %%                   {<<"msg">>,{struct,[{<<"id">>,22},{<<"comment">>,Ask1}]}}
    %%                    ]}}]}
    %%                    }]}}]}),
                        %% обнуляем очередь
                        Jid1=jlib:jid_to_string(First_user),
                        Pid1=gproc:lookup_local_name(Jid1),
                        %% надемся что пид корректен и меняем состояние профиля он в битве
                        gen_server:call(Pid1, {local_state_on_battle, 0});

                _ -> NewState=State
                end,

{noreply, NewState};

handle_cast(Msg, State) ->
?INFO_MSG("profile MSG: ~p~n", [Msg]),
{noreply, State}.
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
?DEBUG("packet received handle_info : ~p~n", [Info]),
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


go_battle(User1,User1_num,State,MeJid) ->

{user_perk,Score1,_Avia1,_Brone1,_Soldier1,_Bet, _Pro1} = User1_num,
{user_perk,Score2,_Avia2,_Brone2,_Soldier2,_Bet, _Pro2} = State#state.num1,
Name = <<"battle_serv">>,
Ali=gproc:lookup_local_name(Name),
Battle_type = 1,
    First_turn =case abs(Score1-Score2) of
	   Delta when Delta>499 -> 
	       %% разница в 500 или больше рейтинга ходит тот у кого меньше рейтинга
	       Score1<Score2;
	   _ ->random:uniform(2)==1
       end,

case {First_turn,is_pid(Ali)} of
        {true, true}  ->
        %% первым ходит user1
        gen_server:cast(Ali, {new,  User1, User1_num,State#state.user1,State#state.num1,State#state.standartAddScore,Battle_type,0});
        {_,true} ->
        %% первым ходит user2

        gen_server:cast(Ali, {new,  State#state.user1,State#state.num1,User1,User1_num,State#state.standartAddScore,Battle_type,0});
        {_,false} ->
        %% нужный нас сервер не запущен :(
        %%?INFO_MSG("match_rbattle me this line go nex : ~p~n", [123]),

        Ask1 = <<"0JjQs9GA0L7QstC+0Lkg0YHQtdGA0LLQtdGAINC/0LXRgNC10LPRgNGD0LbQtdC9INC30LDQv9GA0L7RgdCw0LzQuCAtICDQv9C+0L/RgNC+0LHRg9C50YLQtSDRh9GD0YLRjCDQv9C+0LfQttC1">>,
                        Text=mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,20},
                        {<<"body">>,
                        {struct,[{<<"queue">>,
                        {struct,[{<<"type">>,2},
                        {<<"msg">>,{struct,[{<<"id">>,3},{<<"comment">>,Ask1}]}}
                        ]}}]}
                        }]}}]}),
                        echo(MeJid, User1, Text),echo(MeJid, State#state.user1, Text),
                        (catch match_battle:start_link()),
                        ?INFO_MSG(" done: ~p~n", [State#state.user1])
    end.


%%--------------------------------------------------------------------
%% Func: echo(From, To, Taxt)
%% Description: send Text to user
%%--------------------------------------------------------------------
echo(_From, _To, []) ->
ok;
echo(_From, To, Text) ->
   send_msg(To,  Text).
%%echo(To, Text) ->
%%   send_msg(To,  Text).






%%--------------------------------------------------------------------
%% @doc функция отправки сообщения пользователю
%% @spec send_msg(To,  Text) -> ok
%% @end
%%--------------------------------------------------------------------

send_msg(To,  Text) ->
p_sender:send_msg(To,  Text).
