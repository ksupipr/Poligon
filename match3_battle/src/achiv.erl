%%%-------------------------------------------------------------------
%%% File    : achiv.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Достижения.
%%%
%%% Created :  28 Mar 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(achiv).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").


-include("achiv.hrl").

-include("shared.hrl").

%% API
-export([start_link/0, if_achiv/2, num_getted_achiv/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% export fun for work #userparams{}
-export([add_achiv/2,  del_achiv/2]).

-record(state, { }).

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
    ?INFO_MSG("achiv start: ~p; ", [Sl]),
    Sl
.


%%====================================================================
%% gen_server callbacks
%%====================================================================

if_achiv(Type, UserParams) ->
    gen_server:cast(?SERVER, {if_achiv, Type, UserParams}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Any) ->

    Name = <<"achiv">>,
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
handle_cast({if_achiv, Type, UserParams}, State) ->
%% проверка на получение достижения
if_achiv(Type, UserParams, State),
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
%%% Lib  --  Achiv manipulation functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Функция добавляет список ачивок в список имеющихся
%% @spec add_achiv(Getted=#getted{},List) -> #getted{}
%% @end
%%--------------------------------------------------------------------

add_achiv(Getted,[]) ->
 Getted;
add_achiv(Getted=#getted{medals=Medal,achivs=Achiv},[ Vaaa = {ID, Date,_Value} | Tail]) ->
%% добавление ачивки в getted
%%    , medals = []       %% медали     {Id, Date_get, Qntty}
%%    , achivs = []       %% достижения {Id, Date_get, Qntty}
Pull8 = fun(Valle = {AID, ADate, AValue},  AList) -> 
   %% если ачивка или медаль есть в лист тогда прибавить к валл
       case lists:keytake(AID, 1, AList) of
           {value,_Tp={_, _Dain, AVal_In},ALLp} when (AVal_In+AValue) =/= 0 -> [{AID,ADate,AVal_In+AValue}| ALLp ]  ;
	   false -> [Valle | AList]
               end %% case
 end, %% fun

NewGetted =  case achiv_info:show_info(ID, type) of
		 %% [3 - звание, 2 - медаль, 1 - достижение  4 игнорим я так понимаю]
		 %%  я считаю что могут быть новые звания и достижени и все они хранятся в achivs
        2 -> Getted#getted{ medals=Pull8(Vaaa, Medal) };
        1 -> Getted#getted{ achivs=Pull8(Vaaa, Achiv) };
        3 -> Getted#getted{ rank = {ID, Date, 1} };
    	_ -> Getted	    
	     end,
    
 add_achiv(NewGetted,Tail);
add_achiv(Getted,_) ->
%% матчинг для нестандарных данных на вход
 Getted.

%%--------------------------------------------------------------------
%% @doc Функция удалят список ачивок из имеючихся
%% @spec del_achiv(Getted=#getted{},List) -> #getted{}
%% @end
%%--------------------------------------------------------------------
del_achiv(Getted,[]) ->
    Getted;
del_achiv(Getted=#getted{medals=Medal,achivs=Achiv},[ {ID,_Date,Value} | Tail]) ->
Pull8 = fun( AID,  AValue,  AList) -> 
   %% если ачивка или медаль есть в лист тогда отнять  валл
       case lists:keytake(AID, 1, AList) of
           {value,_Tp={_, _ADin, AVal_In},ALLp} when (AVal_In-AValue) =< 0 -> ALLp   ;
           {value,_Tp={_,  BDin, BVal_In},BLLp}  -> [{AID,BDin,BVal_In-AValue}| BLLp ]  ;
	   false -> AList
               end %% case
 end, %% fun
%% удалить указанные ачивку
NewGetted =  case achiv_info:show_info(ID, type) of
		 %% [3 - звание, 2 - медаль, 1 - достижение  4 игнорим я так понимаю]
		 %%  я считаю что могут быть новые звания и достижени и все они хранятся в achivs
        2 -> Getted#getted{ medals=Pull8( ID,Value,Medal) } ;
        1 -> Getted#getted{ achivs=Pull8( ID,Value,Achiv)} ;
	_ ->Getted	     %% ранг пофик
	     end,
    
del_achiv(NewGetted,Tail);
del_achiv(Getted,_) ->
%% матчинг для нестандарных данных на вход
    Getted.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------



if_achiv(Type, UserParams, State) ->

%% проверка на получение достижения
%% Type - тип события
%%           1 - окончание боя
   %% ?INFO_MSG("achiv.if_achiv Type: ~p ~n UserParams: ~p ; ", [Type, UserParams]),
%%  UserParams: {userparams,<<"vk_20200480@im.xlab.su/xiff">>,7,{getted,1,[],[]}} ;

%% отправка функции get_achiv(AchivType, UserParams, State), где AchivType - тип достижения
%%                                                          1 - звание
%%                                                          2 - медаль  
%%                                                          3 - достижение
%% служит для проверки при определенном событии только определенный тип достижения

NewUserParams = get_achiv(Type, UserParams, State),

    case (NewUserParams) of 
        0 -> ok; % нет новых достижений
        _ -> % отправляем новые достижения
                 Jid = binary_to_list(UserParams#userparams.jid),
                 Pid4Jid = gproc:lookup_local_name(Jid),

                case (libs_functions:alive(Pid4Jid)) of
                    false -> ok;
                     _ -> gen_server:cast(Pid4Jid, {set_achiv, NewUserParams})
                end
    end
.


%%    Name = UserParams#userparams.jid,
%%    Pid4pro=gproc:lookup_local_name(Name),

%%    case (is_pid(Pid4pro)) of
%%        true  ->
        %% отправить запрос туда
%%        gen_server:cast(Pid4pro, {set_achiv, NewUserParams_out});
%%        _ ->
%%            ok
%%    end



%%--------------------------------------------------------------------
%% @doc Запрос на проверку получения достижения. Type - тип достижения (1-звание, 2 - медаль, 3 - достижение)
%% @spec  get_achiv(Type, State) -> #achiv_change_params.
%% @end
%%--------------------------------------------------------------------

get_achiv(Type, UserParams, State) ->
    Jid = UserParams#userparams.jid,
    Getted_achiv = get_user_achivs(Jid),

    case (Getted_achiv) of
        0 -> UserParams;
        _ -> 
            Rank = Getted_achiv#getted.rank,
            Medals = Getted_achiv#getted.medals,
            Achivs = Getted_achiv#getted.achivs,
            All_getted_achivs = lists:flatten([Achivs, Medals, [Rank]]),
            get_achiv(Type, UserParams, All_getted_achivs, State)
    end
.

get_achiv(Type, UserParams, All_getted_achivs, State) ->

%%  UserParams: {userparams,<<"vk_20200480@im.xlab.su/xiff">>,7,{getted,1,[],[]}} ;

%?INFO_MSG("achiv.get_achiv in ~n; ", []),


%Jid          = UserParams#userparams.jid,

%Geted_achiv_now = UserParams#userparams.new_achive,

%?INFO_MSG("achiv.get_achiv UserParams: ~p ~n; ", [UserParams]),


StatisticsDB = UserParams#userparams.statistics,

%?INFO_MSG("achiv.get_achiv Statistics: ~p ~n; ", [StatisticsDB]),

Achivs_all = achiv_info:all_achivs_arr(),

%?INFO_MSG("achiv.get_achiv Achivs_all: ~p ~n; ", [Achivs_all]),


F_WTF = fun (A, AccIn) ->
%% A = {stat_num, stat_count, stat_type}
%% функция проверяет условия наличие достаточного значения параметра статистики
%% more      - параметр больше значения {67, 10, more}
%% less      - параметр меньше значения {67, 10, less}
%% summore   - сумма параметров больше значения {[67, 68, 69], 10, summore}
%% sumless   - сумма параметров меньше значения {[67, 68, 69], 10, sumless}
%% firstmore - 1 параметр больше 2 {{67, 68}, 0, firstmore}
%% firstless - 1 параметр меньше 2 {{67, 68}, 0, firstless}
%% sumfirstmore - сумма 1х параметров больше 2х {{[67, 68], [69, 70]}, 0, sumfirstmore}
%% sumfirstless - сумма 1х параметров меньше 2х {{[67, 68], [69, 70]}, 0, sumfirstless}

%?INFO_MSG("achiv.get_achiv F_WTF in ~n; ", []),

    case A of
        [] -> AccIn;
         _ -> {Stat_num, Stat_count, Stat_type} = A, 
             
              case (Stat_type) of
                equal -> Statistics_param = get_stat_val(Stat_num, StatisticsDB),
                        if (Statistics_param == Stat_count) -> AccIn+1;
                            true -> AccIn
                        end;
                more -> Statistics_param = get_stat_val(Stat_num, StatisticsDB),
                        if (Statistics_param >= Stat_count) -> AccIn+1;
                            true -> AccIn
                        end;

                less -> Statistics_param = get_stat_val(Stat_num, StatisticsDB),
                        if (Statistics_param =< Stat_count) and (Statistics_param >0) -> AccIn+1;
                            true -> AccIn
                        end;

                sumequal -> Statistics_param = get_stat_summ(StatisticsDB, Stat_num),
                        if (Statistics_param == Stat_count) -> AccIn+1;
                            true -> AccIn
                        end;

                summore -> Statistics_param = get_stat_summ(StatisticsDB, Stat_num),
                        if (Statistics_param >= Stat_count) -> AccIn+1;
                            true -> AccIn
                        end;

                sumless -> Statistics_param = get_stat_summ(StatisticsDB, Stat_num),
                        if (Statistics_param =< Stat_count) and (Statistics_param >0) -> AccIn+1;
                            true -> AccIn
                        end;

                firstmore -> {Stat_num1, Stat_num2} = Stat_num,
                             Statistics_param1 = get_stat_val(Stat_num1, StatisticsDB),
                             Statistics_param2 = get_stat_val(Stat_num2, StatisticsDB),
                        if (Statistics_param1 > Statistics_param2) -> AccIn+1;
                            true -> AccIn
                        end;

                firstless -> {Stat_num1, Stat_num2} = Stat_num,
                             Statistics_param1 = get_stat_val(Stat_num1, StatisticsDB),
                             Statistics_param2 = get_stat_val(Stat_num2, StatisticsDB),
                        if (Statistics_param2 > Statistics_param1)  -> AccIn+1;
                            true -> AccIn
                        end;

                sumfirstmore -> {Stat_num1, Stat_num2} = Stat_num,
                            Statistics_param1 = get_stat_summ(StatisticsDB, Stat_num1),
                            Statistics_param2 = get_stat_summ(StatisticsDB, Stat_num2),
                        if (Statistics_param1 > Statistics_param2) -> AccIn+1;
                            true -> AccIn
                        end;

                sumfirstless -> {Stat_num1, Stat_num2} = Stat_num,
                            Statistics_param1 = get_stat_summ(StatisticsDB, Stat_num1),
                            Statistics_param2 = get_stat_summ(StatisticsDB, Stat_num2),
                        if (Statistics_param2 > Statistics_param1) -> AccIn+1;
                            true -> AccIn
                        end;

                        _ -> AccIn
              end
    end
end, 


F_WTA = fun (A, AccIn) ->
%% функция проверяет условия наличия других достижений, их количества и необходимость их удаления
% A = {achiv_id, achiv_num, dell}
% Dell_list = [{Achiv_id, Dell_num} | T]
    {Count_now, Dell_list} = AccIn,

    case A of
        [] -> AccIn;
         _ -> 
              % если в условиях на получение число
              {Achiv_id, Achiv_num, Dell} = A, 
              Num_getted_achiv = num_getted_achiv(Achiv_id, All_getted_achivs), % количество данного достижения
                      case (Num_getted_achiv) of
                                M when M >= Achiv_num -> 
                                    case (Dell) of
                                        D when D > 0 -> {(Count_now+1), lists:flatten([Dell_list, [{Achiv_id, calendar:datetime_to_gregorian_seconds(erlang:localtime()), Dell}]])};
                                                   _ -> {(Count_now+1), Dell_list}
                                    end;
                                _ -> AccIn
                      end
    end
end, 


F_Param = fun(_AIndex, A, AccIn) ->
%% функция проверяет и составляет список достижений, которые надо добавить
    {Add_ach, Dell_ach} = AccIn,

    case (A) of
    0 -> AccIn;
    _ ->
        Achiv_params = A#achiv.state_params,
       % Achiv_group =  A#achiv.group,
        Need_achivs  = A#achiv.need_achivs,
        Achiv_id     = A#achiv.id,
        Stacked      = A#achiv.stacked,
        TType        = A#achiv.t_type,

        case (TType) of
            TT when (TT =/= Type)  -> AccIn; % если тип проверки достижения не совпадает с запрошеным
                                                                  %% , то не проверять

            _  ->   Achiv_qntty  = num_getted_achiv(Achiv_id, All_getted_achivs),
                    Achivs_add           = lists:foldl(F_WTF, 0, Achiv_params), % проверяем соответствие параметров

                    Need_achivs_add_term = lists:foldl(F_WTA, {0, []}, Need_achivs), % проверяем наличие достижений и списки на удаление

                    {Need_achivs_add, Need_dell} = Need_achivs_add_term,

                    % проверяем, накапливаемый ли Стек (если 0, то не копится, если 1 - накапливаемый стек)
                    case Stacked of 
                        0 -> Achiv_qntty_add = Achiv_qntty;
                        _ -> Achiv_qntty_add = 1
                    end,

                    case (Achivs_add + Need_achivs_add) of 
                        % если все параметры тру и накапливаемость соблюдена, то добавляем в список на добавление
                        M when (M < (length(Achiv_params) + length(Need_achivs))) or (Stacked =/= Achiv_qntty_add)  -> AccIn;
                        _ -> { lists:flatten([Add_ach, Achiv_id]), lists:flatten([Dell_ach, Need_dell]) }
                    end
        end
    end
end,

Rezult_list_arr = array:foldl(F_Param, {[], []}, Achivs_all),

case (Rezult_list_arr) of
    {[], []} -> 0;
           _ -> New_UserParams = make_new_UserParams(UserParams, Rezult_list_arr, State),
            %    ?INFO_MSG("achiv.get_achiv New_UserParams: ~p ~n; ", [New_UserParams]),
                New_UserParams
end
.

%% формирование рекорда для отправки пользователю
%% AddAchiveForm - запись достижений в нужном формате
%% 

make_new_UserParams(UserParams, Rezult_list_arr, _State) ->

{AddAchive, AddDellAchive} = Rezult_list_arr,


F_BONUS = fun(A, AccIn) ->
    {A_ids, A_qntty} = A,
    case A_ids of 
        M when is_list(M) -> N = libs_functions:arand(M),
                             A_id_in = lists:nth(N, M);
                        _ -> A_id_in = A_ids
    end,
    lists:flatten([[{A_id_in, A_qntty}], AccIn])
end,

F_ADD = fun(A, AccIn) ->
    Achiv = show_info(A, 1),
    {WAS, WAM, WAB1} = Achiv#achiv.add_params,
    WAB = lists:foldl(F_BONUS, [], WAB1),
    {WNS, WNM, WNB, WNA} = AccIn,
    {lists:flatten([WAS, WNS]), (WAM+WNM), lists:flatten([WAB, WNB]), lists:flatten([WNA, [{A, calendar:datetime_to_gregorian_seconds(erlang:localtime()), 1}]])}    
end,

{AddStatistics, AddMoney_0, AddBonus, AddAchiveForm} = lists:foldl(F_ADD, {[], 0, [], []}, AddAchive),

UserParams#userparams {
                new_statistics = lists:flatten([UserParams#userparams.new_statistics, AddStatistics])
              , statistics = []
              , money_0        = UserParams#userparams.money_0 + AddMoney_0
              , new_bonuses    = lists:flatten([UserParams#userparams.new_bonuses, AddBonus])
              , new_achive     = lists:flatten([UserParams#userparams.new_achive, AddAchiveForm])
              , dell_achive    = lists:flatten([UserParams#userparams.dell_achive, AddDellAchive])
        }
.


num_getted_achiv(Achiv_id, Achivs) ->

case (Achivs) of
    M when is_list(M) -> Achivs_out = M;
                    _ -> Achivs_out = [Achivs]
end,

Achiv_group = achiv_info:get_group_list_by_id(Achiv_id),

A_NUM = fun(A, AccIn) ->
    case A of 
        [] -> AccIn;
        _ -> AccIn + num_getted_achiv_by_id(A, Achivs_out)
    end
end,

lists:foldl(A_NUM, 0, Achiv_group)
.


num_getted_achiv_by_id(Achiv_id, Achivs) ->
%Achivs = [{Id, Date_get, Qntty} |T]

A_NUM = fun(A, AccIn) ->
    {Id, _Date_get, Qntty} = A,
    case Id of 
        M when M == Achiv_id -> Qntty;
        _ -> AccIn
    end
end,

lists:foldl(A_NUM, 0, Achivs)
.


get_user_achivs(Jid) ->
    Pid4Jid = gproc:lookup_local_name(binary_to_list(Jid)),

    case (libs_functions:alive(Pid4Jid)) of
        false -> Rez = 0;
            _ -> Rez = gen_server:call(Pid4Jid, {get_user_achivs})
    end,
Rez
.

%%--------------------------------------------------------------------
%% @doc Запрос на получение информации по достижению. Type - тип достижения (1-звание, 2 - медаль, 3 - достижение)

%% @end
%%--------------------------------------------------------------------


show_info(Achiv_id, Type) ->
%% возвращает информацию о вещи.
%% Type - тип инфы, 1 - рекорд, любое другое - json
achiv_info:show_info(Achiv_id, Type)
.



get_stat_summ(DB, List_stst_param) ->
    List_stst_val = [get_stat_val(Num, DB) || Num <- List_stst_param], 
    lists:sum(List_stst_val)
.

get_stat_val(Num, DB) ->

if (Num<0) -> Num_in = Num*(-1), Val_in = -1;
    true -> Num_in = Num, Val_in = 1
end,

case Num of
          A when A < 20 ->                 
                        Array = DB#statistics.stat_day, 
                        array:get(Num_in, Array)*Val_in;

          B when B > 19 , B <40 -> 
                        Array = DB#statistics.stat_week,
                        array:get(Num_in-20, Array)*Val_in;

          C when C > 39 , C < 60 -> 
                        Array = DB#statistics.stat_moon,
                        array:get(Num_in-40, Array)*Val_in;

          _D  -> 
                        Array = DB#statistics.stat_always,
                        array:get(Num_in-60, Array)*Val_in
        end
.


