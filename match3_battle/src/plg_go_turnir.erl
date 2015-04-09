%%%-------------------------------------------------------------------
%%% @author Marat Yusupov
%%% @copyright (C) 2012, Marat Yusupov
%%% @doc
%%%  Процесс реализующий набор нужного числа народа на турнир
%%% задача: набирать нужное число народу и отправить их в битву
%%% при этом следует отправлять состав при наборе каждого чела всем
%%% @end
%%% Created : 14 Feb 2012 by Marat Yusupov
%%%-------------------------------------------------------------------
-module(plg_go_turnir).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% shared fun
-export([get_player_json/1]).

-define(SERVER, ?MODULE). 

-include("shared.hrl").
-record(user, ?PLG_TOURNAMET_USER_REC ).


-record(state, {
	  all_users = []  %% :: [#user]
	  , count=0 %% числи игроков в очереди
	  , prize=50 %%
	  , free_num=[]
}).
-include("ejabberd.hrl").
-include("jlib.hrl").

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server Инит происходит стандартным рекордом с 
%%  нулевой длинной списка всех пользователей
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
Free_num=lists:seq(1, 16),
    {ok, #state{free_num=Free_num}}.

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
%%handle_call({add,UserData}, From, #state{ all_users = []} = State) ->
%%    X1 = add_user(UserData,State,From),
%%    {reply, added, X1};

handle_call({add,{_Fname,_Lname,_Mname,Jid,_Rang,_Lrank,_Money,_Score,_Ava,_UserStatus,_Sn_id} = UserData}, From, State) ->

%% функция которая ставит чела в ожидание начала турнира
%% -1 проверить что указанный Jid еще не в очереди 
%%[H|_]=State#state.all_users,
%%?INFO_MSG("profile MSG1: ~p~n", [H#user.jid]),
%%?INFO_MSG("profile MSG2: ~p~n", [Jid]),
%%?INFO_MSG("profile MSG3: ~p~n", [(H#user.jid) =:= Jid]),

 Even = fun(X) -> (X#user.jid) =:= Jid end,
%% ?INFO_MSG("profile MSG3: ~p~n", [ lists:filter(Even, State#state.all_users)]),

    case lists:filter(Even, State#state.all_users)  of
	[] -> %% игрока нет в списках на турнир
	    {X1,Res} = add_user(UserData,State,From);
	_ -> %% игрок уже в очереди  ругнутся
                        Ask1 = <<"0JLRiyDQvdC1INC80L7QttC10YLQtSDQv9C+0LrQuNC90YPRgtGMINC+0YfQtdGA0LXQtNGMINGB0LvRg9GH0LDQudC90YvRhSDQsdC+0LXQsg==">>,
                        Result = mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,151},
                        {<<"body">>,
                        {struct,[{<<"turnir_queue">>,
                        {struct,[{<<"type">>,2},
                        {<<"msg">>,{struct,[{<<"id">>,21},{<<"comment">>,Ask1}]}}
                        ]}}]}
                        }]}}]})
		, send_msg(Jid,  Result),Res=0,     
	     X1 = State
	    
end,
{reply, Res, X1};

handle_call({del,Jid,Num}, _From, State) ->
 Even = fun(X) -> (X#user.jid) =:= Jid end,
%%?INFO_MSG("filter user: ~p~n", [lists:filter(Even, State#state.all_users)]),

Res =  case lists:filter(Even, State#state.all_users)  of
	[] -> %% игрока нет в списках на турнир
                        Ask1 = <<"0JLRiyDQvdC1INC80L7QttC10YLQtSDQv9C+0LrQuNC90YPRgtGMINC+0YfQtdGA0LXQtNGMINGB0LvRg9GH0LDQudC90YvRhSDQsdC+0LXQsg==">>,
                        Result = mochijson2:encode(
                        {struct,[{<<"reply">>,
                        {struct,[{<<"type">>,152},
                        {<<"body">>,
                        {struct,[{<<"turnir_queue">>,
                        {struct,[{<<"type">>,2},
                        {<<"msg">>,{struct,[{<<"id">>,152},{<<"comment">>,Ask1}]}}
                        ]}}]}
                        }]}}]})
		, 	     X1 = State,false;
	_ -> %% игрок  в очереди  
	  Xx1=delete_user(State,Jid),
           ?INFO_MSG("del user: ~p~n", [Num]),
	  X1= case Num of
	      0 ->  Xx1;
	      _NN -> Xx1#state{free_num=[Num | Xx1#state.free_num]}     end,
	  %% извещаем остальных что мы ушли
          all_send_change(Num,0,X1),

           P8 = fun(A, AccIn) -> 
		 %% анонимная функция для расстановки запятых между обьектами
		 A1 = unicode:characters_to_binary(get_player_json(A)), 
		 B =  unicode:characters_to_binary(","), 
			  case AccIn of [] ->  [ A1 | AccIn ];
			      _ ->   M = [B | AccIn],[A1 | M ] 
			  end 
	       end,
   	       NewUserJson_bin = lists:foldl(P8,[] , X1#state.all_users),
              Bin1 = <<"{\"reply\":{\"type\":152,\"body\": {  \"turnir_queue\": { \"type\":3 ,  \"list\": [">>,
              Bin3 = <<"]}  } }}">>,
	       Result =list_to_binary([Bin1,NewUserJson_bin,Bin3]),
	       ok
         end
		, send_msg(Jid,  Result),     
{reply, Res, X1};
handle_call({show,Jid}, _From, State) ->
%% отобразить список народа 
    case  all_send_user(Jid , State#state.all_users,150,State#state.prize) of
	 empty -> %% игроков в очереди нет
	    %%   "turnir_list": {        "list": []      }
	    Result=mochijson2:encode({struct,[{<<"reply">>, {struct,[{<<"type">>,150}, {<<"body">>,
	    {struct,[{<<"turnir_queue">>, {struct,[{<<"list">>,[]}]}  }]}
	    }]}}]}),
	    send_msg(Jid,  Result),{reply, ok, State};     
	_ ->{reply, ok, State}
	end;
handle_call(_Request, _From, State) ->
    Reply = wtf,
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
handle_cast(_Msg, State) ->
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
terminate(_Reason, _State) ->
    ok.

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
%% @doc
%% добавляет чела в очередь 
%% @spec add_user(UserData,State,Pid) -> #state{}
%% @end
%%--------------------------------------------------------------------
add_user({Fname,Lname,Mname,Jid,Rang,Lrank,Money,Score,Ava,UserStatus,Sn_id},State,Pid) -> 

	 %% 0 проверить число народу в ожидании если их 15 + текущий отправить в битву и получить State
	 %% 1 изменить State
    %% 2 оповестить всех о новом члене
[ Next_num | Tail ]=State#state.free_num,

       User= #user{
      num=Next_num,
      firstName=Fname,
      lastName=Lname,
      middleName=Mname,
      jid=Jid,
      rank=Rang,
      litleRank=Lrank,
      money_0=Money,
      score=Score,
      ava=Ava
      , slot_state=0
      , user_status = UserStatus
      , pid = Pid      
      , sn_id=Sn_id
     },
    X1 = State#state{
        count = State#state.count + 1
        , all_users = [User | State#state.all_users]
	, free_num =Tail
      },
	%% оповестить что прищел новый чел 
	newuser_send_all(User, State#state.all_users),
%%  дать список всех игроков в очереди
        all_send_user(User#user.jid , X1#state.all_users,151,State#state.prize)
	, case X1#state.count  of
             16 -> %% набрали нужное число народу 
		   %% отправить их в турнир
		   go_to_turnir(X1),
		   %% начать набор с нуля
		  Free_num=lists:seq(1, 16),
		   {#state{free_num=Free_num},Next_num};
	     _ ->
		 {X1,Next_num}
end.

%%--------------------------------------------------------------------
%% @doc
%% @spec newuser_send_all(User, State#state.alluser) -> ok.
%% @end
%%--------------------------------------------------------------------
newuser_send_all(User, All_users) ->
    %% подготовить сообщение о пользователе 
%%    {"reply":{"type":150,"body": { 
%%			   {"player":{"num":1  "firstName":"Vladimir","lastName":"Putin","middleName":"Vladimirovich", 
%%				   "id":"scas@im.xlab.su", "rang":"0JzQsNC50L7RgA==", "money_0":0,
%%				   "lrank":"0Jwt0YA=","reit":7, "ava":"www.xlab.su/qweq.png"}}, 
%%			  }}}
        
    NewUserJson_bin =unicode:characters_to_binary(get_player_json(User)),
    Bin1 = <<"{\"reply\":{\"type\":154,\"body\": {  \"turnir_new_guys\": {  \"list\": [">>,
    Bin3 = <<" ]}} }}">>,
    Text1=list_to_binary([Bin1,NewUserJson_bin,Bin3]),
    %% разослать всем
    P8 = fun(A) -> 
		 %% отправки сообщения
		 A1 = A#user.jid, 
		 send_msg(A1,  Text1)
	 end,
lists:foreach(P8, All_users).

%%--------------------------------------------------------------------
%% @doc рассылает всех пользователей в очереди данному пользователю 
%% @spec  all_send_user(User, alluser) -> ok.
%% @end
%%--------------------------------------------------------------------
all_send_user(_User, [],_Type,_P)->
    empty;
all_send_user(User, All_users,Type,Prize)->

    P8 = fun(A, AccIn) -> 
		 %% анонимная функция для расстановки запятых между обьектами
		 A1 = unicode:characters_to_binary(get_player_json(A)), 
		 B =  unicode:characters_to_binary(","), 
			  case AccIn of [] ->  [ A1 | AccIn ];
			      _ ->   M = [B | AccIn],[A1 | M ] 
			  end 
	 end,

    NewUserJson_bin = lists:foldl(P8,[] , All_users),
    Bin11 = <<"{\"reply\":{\"type\":">>,
    Bin2 = <<",\"body\": {  \"turnir_queue\": { \"prize\":">>, 
    Bin22 = <<", \"list\": [">>,
    Bin1 = list_to_binary([Bin11, io_lib:format("~p", [Type]),Bin2,
         io_lib:format("~p", [Prize]),
       Bin22]),
    Bin3 = <<"]}  } }}">>,
    Text1=list_to_binary([Bin1,NewUserJson_bin,Bin3]), 
    send_msg(User,  Text1),    
ok.    


%%--------------------------------------------------------------------
%% @doc Функция собирает из записи типа user оыщт описывающий игрока
%% @spec get_player_json(User) ->  iolist()
%% @end
%%--------------------------------------------------------------------
get_player_json(User) ->
       Jid_bin = unicode:characters_to_binary(jlib:jid_to_string(User#user.jid)),
     mochijson2:encode( {struct,[
	       {<<"num">>,        User#user.num},
	       {<<"slot_state">>, User#user.slot_state},
               {<<"firstName">>,  User#user.firstName},
               {<<"lastName">>,   User#user.lastName},
               {<<"middleName">>, User#user.middleName},
               {<<"jid">>,         Jid_bin },
               {<<"rang">>,       User#user.rank},
               {<<"money_0">>,    User#user.money_0},
               {<<"lrank">>,      User#user.litleRank},
               {<<"game_img">>,        User#user.ava},
               {<<"user_status">>,        User#user.user_status},
               {<<"sn_id">>,     User#user.sn_id },
               {<<"duel_st">>,   0},
               {<<"reit">>,       User#user.score}
			   ]   }).

%%--------------------------------------------------------------------
%% @doc Высылает нужным форматом весь список народа в турнир
%% @spec go_to_turnir(X1) -> ok.
%% @end
%%--------------------------------------------------------------------
go_to_turnir(State) ->
   %%  отправить в турнир всю толпу.
    {ok, Pid}=plg_tournament:start_link(),
    Users=State#state.all_users,
    gen_server:cast(Pid, {begin_tourn, Users}),
 ok.


%%--------------------------------------------------------------------
%% @doc функция выдает статус удалив оттуда чела с указанным Jid
%% @spec delete_user(State,Jid)  -> #state{}
%% @end
%%--------------------------------------------------------------------
delete_user(State,Jid) ->
Even = fun(X) -> (X#user.jid) =/= Jid end,
X1_all = lists:filter(Even, State#state.all_users),
%% так как в эту функцию попадает только тогда когда игрок в очереди то 
%% функция выше явно отработала и удалила одного игрока из очереди
 State#state{
        count = State#state.count-1
        , all_users = X1_all
      }.


%--------------------------------------------------------------------
%% Func: send_msg(To,  Text) 
%% Description: send Text to user small form
%%--------------------------------------------------------------------
send_msg( To,  Text) ->
p_sender:send_msg(To,  Text).



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
    P8 = fun(A) -> 
		 %% отправки сообщения
		 send_msg(A#user.jid,  Text)
	 end,
lists:foreach(P8, State#state.all_users).
