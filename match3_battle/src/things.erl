%%%-------------------------------------------------------------------
%%% File    : things.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Магазин.
%%%
%%% Created :  14 Mar 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(things).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").
-include("shop.hrl").
-include("shared.hrl").


% API
-export([start_link/0, show_info/2]).
-export([ get_thing_id/1 , buy_thing/5]).

-export([ get_razdel_descr/1,
get_group_name/2, 
get_group_descr/2,
get_razdel_name/1
]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
      redis_link %% линк редиса
    , things %% массив описаний вещей
    , buyed = []
    , tid = 0
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
    ?INFO_MSG("things start: ~p; ", [Sl]),
    Sl
.


%%====================================================================
%% gen_server callbacks
%%====================================================================


buy_thing(Buy_id, Count, UState, UserJid, Tid) ->
        gen_server:call(?SERVER, {buy_thing, Buy_id, Count, UState, UserJid, Tid}, infinity).


%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Any) ->
%%?INFO_MSG("things init; ", []),
    Name = <<"things">>,
    gproc:add_local_name(Name),
  
    
    State = #state{things=array:new([{size, 0}, {default, 0}, {fixed, false}])},
    %%gen_server:cast(self(), {read_things}),
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


handle_call({buy_thing, Buy_id, Count, UState, UserJid, Tid}, _From, State) ->
    Reply = buy_thing(Buy_id, Count, UState, UserJid, Tid, State),
    {reply, Reply, State};



handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({read_things}, State) ->
        StateRead = State#state{things=array:set(1, things_info:show_info(1, 1), State#state.things)},
    {noreply, StateRead};

handle_cast({show_info, _Pid, Bonus_id, Type}, State) ->
%% информация о вещи. По сути первичный зпрос.

    Bonus = things_info:show_info(Bonus_id, Type),
    Bonus,

    {noreply, State};

handle_cast({show_all, Type, User}, State) ->
%% информация о вещи. По сути первичный зпрос.

%%   ?INFO_MSG("Things Type: ~p~n", [Type]), 

    case (Type) of
        RM4 when RM4 == 4 -> All_things = [{4, [{1, ava_info:get_ava_list()}]}];
                    _ -> All_things = ?ALL_SHOP
    end,

%%    ?INFO_MSG("Things ALL_SHOP: ~p~n", [All_things]), 

    Razdels_list_1 = [show_razdel(Razdel_th, Razdel_num, State) || {Razdel_num, Razdel_th} <- All_things],

        P8 = fun(A, AccIn) -> 
       %% анонимная функция для расстановки запятых между обьектами
         A1 = A,
       B =  unicode:characters_to_binary(","), 
            case AccIn of [] ->  [ A1 | AccIn ];
                _ ->   M = [B | AccIn],[A1 | M ] 
            end 
   end,

    %% ставим запятые
    Razdels_list = lists:foldl(P8,[] , Razdels_list_1),
    
    LLR1 = <<"{\"reply\": {\"type\":40, \"body\": {\"shop\": {">> ,
    LLR2 = <<"} } }}">> ,
    AllListResult = lists:flatten([LLR1,lists:reverse(Razdels_list),LLR2]),

    %%?INFO_MSG("Things AllListResult: ~p~n", [lists:flatten(AllListResult)]), 

    send_msg(User,  list_to_binary(AllListResult)),

    {noreply, State};

handle_cast({incr_tid}, State) ->
        StateRead = State#state{ tid = (State#state.tid+1)},
    {noreply, StateRead};



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


show_razdel(Radel_th, Razdel_num, State) ->
    Razdels_things = [show_group(Group_th, Group_num, Razdel_num, State) || {Group_num, Group_th} <- Radel_th],

    P8 = fun(A, AccIn) -> 
       %% анонимная функция для расстановки запятых между обьектами
         A1 = A,
       B =  unicode:characters_to_binary(","), 
            case AccIn of [] ->  [ A1 | AccIn ];
                _ ->   M = [B | AccIn],[A1 | M ] 
            end 
   end,

    %% ставим запятые
    Razdels_things_out = lists:foldl(P8,[] , Razdels_things),

    Razdel_name = get_razdel_name(Razdel_num),
    Razdel_descr = get_razdel_descr(Razdel_num),

    LLR1 = <<"\"razdel">> ,
    LLR_name1 = <<"\": {\"name\":\"">> ,
    LLR_name2 = <<"\", \"descr\": \"">>,
    LLR_name3 = <<"\",  \"list\": {">> ,
    LLR2 = <<"}} ">> ,
    RazdelsListResult = lists:flatten([LLR1, integer_to_list(Razdel_num), LLR_name1, Razdel_name, LLR_name2, Razdel_descr, LLR_name3, lists:reverse(Razdels_things_out),LLR2]),

    RazdelsListResult
.

show_group(Group_th, Group_num, Razdel_num, State) ->


TH_OUT = fun (A, AccIn) ->

        Buyed_list = State#state.buyed, %ava_info:get_buyed_ava(),
        case lists:member(A, Buyed_list) of
                false -> A_add = 1;
                    _ -> A_add = 0
        end,
                
        B =  unicode:characters_to_binary(","), 
        case AccIn of
                _ when A_add == 0 -> AccIn;
                []  -> things_info:show_info(A, 2);
                  _  ->lists:flatten([AccIn, B, things_info:show_info(A, 2)])
        end
end,

    Razdels_things_out = lists:foldl(TH_OUT, [], Group_th), 


    Razdel_name = get_group_name(Razdel_num, Group_num),
    Razdel_descr = get_group_descr(Razdel_num, Group_num),

    LLR1 = <<"\"group">> ,
    LLR_name1 = <<"\": {\"name\":\"">> ,
    LLR_name2 = <<"\", \"descr\": \"">> ,
    LLR_name3 = <<"\", \"list\": {">> ,
    LLR2 = <<"} }">> ,
    RazdelsListResult = lists:flatten([LLR1, integer_to_list(Group_num), LLR_name1, Razdel_name, LLR_name2, Razdel_descr, LLR_name3, Razdels_things_out, LLR2]),

    RazdelsListResult
.
show_info([], _Type) ->
[];
show_info(Bonus_id, Type) ->
%% возвращает информацию о вещи.
%% Type - тип инфы, 1 - рекорд, любое другое - json

things_info:show_info(Bonus_id, Type).




%%--------------------------------------------------------------------
%% @doc функция отправки сообщения клиенту 
%% @spec send_msg( To,  Text) -> ok.
%% @end
%%--------------------------------------------------------------------
send_msg( To,  Text) ->
p_sender:send_msg(To,  Text).




%% покупка вещи
buy_thing(Buy_id, Count, UState, UserJid, Tid_in, State) ->
         %%?INFO_MSG("buy_thing Buy_id: ~p~n Count: ~p~n UState:~p~n", [Buy_id, Count, UState]),

        Bunus = things_info:show_info(Buy_id, 1),
%state.buyed_ava       
        Type = Bunus#thing.type,

        
%?INFO_MSG("things:buy_thing : ~p~n count:~p -> ~p ~n ", [Buy_id, Count, Tid_in]),

        Monye0 = UState#thing_change_param_rec.money_0,
        Monye1 = UState#thing_change_param_rec.money_1,
        
        {Thing_price0, Thing_price1} = Bunus#thing.price,

        if (is_integer(Count)) ->
            Th_price0 = Thing_price0*Count,
            Th_price1 = Thing_price1*Count;
        true -> Th_price0 = Thing_price0*1,
                Th_price1 = Thing_price1*1
        end,

        Monye0_c = Monye0-Th_price0,
        Monye1_c = Monye1-Th_price1,

        

        if Monye0>=Th_price0, Monye1>=Th_price1, is_integer(Count) ->
                
                    case (Type) of 
                        1 -> 
                                %% покупка кредитов
                                 %Party_name = " ",
                                 %Need = 0,
                                 %Count_out = libs_functions:int_to_list(Count),
                                 %Result = {-5, UState};
                                Sn_pref = libs_functions:get_sn_prefix(UserJid),
                                {K1, K2} = libs_functions:get_kurs(Sn_pref),                                
                                Votes = erlang:round((K2 * Count)*100),

                                %case (Sn_pref) of
                                   % M when M == <<"vk">> -> Buy_kred_rez = vkapi:withdrawVotes(UserJid, Votes);
                                              %    _ -> 
                                Buy_kred_rez = {error, 81},
                                %end,

%?INFO_MSG("buy_thing kredits votes_num: ~p~n kredits_num:~p ~n", [Votes, Count]),

                                case (Buy_kred_rez) of
                                        {error, 2} -> Result = {-8, UState}, Need = 0;
                                        {error, _NumEr} -> Result = {-4, UState}, Need = erlang:round((K2 * Count));
                                        {votes, _Votes_add} -> NumMonet = Monye1+(Count*K1),
                                                                Need = 0,
                                                                CState = UState#thing_change_param_rec{money_1=NumMonet, money_0=Monye0_c},

                                                                %?INFO_MSG("buy kredits good tid: ~p~n", [Tid_in]),

                                                                if (Tid_in>0) ->
                                                                    Pid4pro=gproc:lookup_local_name(jlib:jid_to_string(UserJid)),

                                                                    case (libs_functions:alive(Pid4pro)) of
                                                                        false -> %%запустить нужный процесс
                                                                                {ok,Pid } = match_profile:start_link(jlib:jid_to_string(UserJid)),
                                                                                gen_server:cast(Pid, {make_auto_buy, Tid_in});
                                                                            _ -> gen_server:cast(Pid4pro, {make_auto_buy, Tid_in})
                                                                    end;
                                                                true -> ok
                                                                end,

                                                                Result = {1, CState}
                                end,
                                Party_name = " кредитов",
                                Count_out = integer_to_list(Count);

                        2 -> 
                                %% покупка Монеты войны
                                NumMonet = Monye0+Bunus#thing.party*Count,
                                CState = UState#thing_change_param_rec{money_0=NumMonet, money_1=Monye1_c},
                                Party_name = " монет",
                                Count_out = libs_functions:int_to_list(Bunus#thing.party*Count),
                                Need = 0,

                                                                if (Tid_in>0) ->
                                                                    Pid4pro=gproc:lookup_local_name(jlib:jid_to_string(UserJid)),

                                                                    case (libs_functions:alive(Pid4pro)) of
                                                                        false -> %%запустить нужный процесс
                                                                                {ok,Pid } = match_profile:start_link(jlib:jid_to_string(UserJid)),
                                                                                gen_server:cast(Pid, {make_auto_buy, Tid_in});
                                                                            _ -> gen_server:cast(Pid4pro, {make_auto_buy, Tid_in})
                                                                    end;
                                                                true -> ok
                                                                end,

                                Result = {1, CState};

                        3 -> 
                                %% покупка Прем
                                {Prem_now, _Date_prem} =  UState#thing_change_param_rec.premium,
                                Party_name = " суток",
                                Count_out = libs_functions:int_to_list(Bunus#thing.party*Count),
                                Need = 0,
                                if 
                                    Prem_now < 1 ->
                                        NumDays = Bunus#thing.party,
                                        %%DatePrem = int_date_out(NumDays),
                                        DatePrem = int_timestamp_out(NumDays),
                                        CState = UState#thing_change_param_rec{premium = {1, DatePrem}, money_0=Monye0_c, money_1=Monye1_c},
                                        Result = {1, CState};
                                    true -> Result = {-3, UState}
                                end;

                        100 -> 
                                Num = Bunus#thing.slot,
                                CState = UState#thing_change_param_rec{money_0=Monye0_c, money_1=Monye1_c, slots = chenge_thing_count(UState, Num, Count)},
                                Party_name = " шт.",
                                Count_out = libs_functions:int_to_list(Bunus#thing.party*Count),
                                Need = 0,
                                Result = {1, CState};

                        1000 -> 
                                Ava = Bunus#thing.img,
                                CState = UState#thing_change_param_rec{money_0=Monye0_c, money_1=Monye1_c, ava = Ava},
                                Party_name = " шт.",
                                Count_out = libs_functions:int_to_list(Bunus#thing.party*Count),
                                Need = 0,
                                Result = {1, CState};
                        2000 -> 
                                Buyed_list = ava_info:get_ava_list(),
                                case lists:member(Bunus#thing.id, Buyed_list) of
                                    true -> 
                                            Add_ava_rez = ava_info:add_buyed_ava(Bunus#thing.id), 
                                            if (Add_ava_rez==1) ->
                                                Ava = Bunus#thing.img,
                                                CState = UState#thing_change_param_rec{money_0=Monye0_c, money_1=Monye1_c, ava = Ava},
                                                Party_name = " шт.",
                                                Count_out = libs_functions:int_to_list(Bunus#thing.party*Count),
                                                Need = 0,
                                                Result = {1, CState};
                                            true -> CState = UState,
                                                    Party_name = "",
                                                    Count_out = Count,
                                                    Need = 0,
                                                    Result = {-7, CState}
                                            end;
                                    _ ->  % аватар уже куплен другим пользователем
                                            CState = UState,
                                            Party_name = "",
                                            Count_out = Count,
                                            Need = 0,
                                            Result = {-6, CState}
                                        
                                end
                                ;
                        _ -> 
                                CState = UState,
                                Party_name = "",
                                Count_out = Count,
                                Need = 0,
                                Result = {0, CState}
                    end;
            true -> 
                
                if (Monye0<Th_price0) or (Monye1<Th_price1) ->
                    if Monye0<Th_price0 ->
                        Count_out = libs_functions:int_to_list(Bunus#thing.party*Count),
                        Party_name = " ",
                        Need = Th_price0-Monye0,
                        Result = {-1, UState};
                    true -> 
                        Count_out = libs_functions:int_to_list(Bunus#thing.party*Count),
                        Party_name = " ",
                        Need = Th_price1-Monye1,
                        Result = {-2, UState}
                    end;
                true -> Party_name = " ", Need = 0, Count_out = "1", Result = {-6, UState}
                end
        end,

        {Done, Res} = Result,
%NeedInKredit=kred_to_money(Need)
        case (Done) of
             1 -> Mess = libs_functions:base64_encode("Куплено"), NeedInKredit=Need, DoneR = 1, Tid = 0;
            -1 -> Mess = libs_functions:base64_encode("Нехватает монет"), NeedInKredit=kred_to_money(Need), DoneR = -1, if (Tid_in > 0 ) -> Tid = Tid_in; true ->  Tid = get_auto_buy(State) end;
            -2 -> Mess = libs_functions:base64_encode("Нехватает кредитов"), NeedInKredit=Need, DoneR = -2, if (Tid_in > 0 ) -> Tid = Tid_in; true ->  Tid = get_auto_buy(State) end;
            -3 -> Mess = libs_functions:base64_encode("Премиум аккаунт уже активен"), NeedInKredit=Need, DoneR = 0, Tid = 0;
            -4 -> Mess = libs_functions:base64_encode("Нехватает голосов для покупки кредитов"), NeedInKredit=Need, DoneR = -3, if (Tid_in > 0 ) -> Tid = Tid_in; true -> Tid = 0 end;
            -5 -> Mess = libs_functions:base64_encode("Покупка кредитов закрыта."), NeedInKredit=Need, DoneR = 0, Tid = 0;
            -6 -> Mess = libs_functions:base64_encode("Аватар уже куплен."), NeedInKredit=Need, DoneR = 0, Tid = 0;
            -7 -> Mess = libs_functions:base64_encode("Ошибка при покупке аватара."), NeedInKredit=Need, DoneR = 0, Tid = 0;
            -8 -> Mess = libs_functions:base64_encode("Неудалось осуществить платеж через[br]социальную сеть.[br][br]Попробуйте через 5 минут."), NeedInKredit=Need, DoneR = 0, Tid = 0;
             _ -> Mess = libs_functions:base64_encode("Не куплено"), NeedInKredit=Need, DoneR = 0, Tid = 0

        end,

        Json_params =
           mochijson2:encode({struct,[
                {"done",      DoneR}, 
                {"msg",       Mess},
                {"name",      libs_functions:base64_encode(Bunus#thing.name)},
                {"descr",     libs_functions:base64_encode(Bunus#thing.descr)},
                {"id",        Buy_id },
                {"count",     libs_functions:base64_encode(lists:flatten([Count_out, Party_name]))},
                {"price0",    Th_price0},
                {"price1",    Th_price1},
                {"need",      NeedInKredit},
                {"tid",       list_to_binary(libs_functions:int_to_list(Tid))}
            ]}),

%?INFO_MSG("things:buy_thing end : ~p~n count:~p -> ~p ~n ", [Buy_id, Count, Tid]),
         

         Json_out1 = <<"{\"reply\":{ \"type\":41, \"body\":{ \"buy_item\":  ">>,
         Json_out2 = <<" } } }">>,

         Json_out = lists:flatten([Json_out1, list_to_binary(Json_params), Json_out2]),

        {Json_out, Res, DoneR, Count_out, Tid}
.

kred_to_money(Need) ->
    Money_id = 2,

    Money_party = things_info:show_info(Money_id, party),
    %{_Money_price0, Money_price1} = things_info:show_info(Money_id, price),
    %libs_functions:ceiling((Need/Money_party)*Money_price1)

    % тут количество партий при необходимости
    libs_functions:ceiling(Need/Money_party)
.

chenge_thing_count(State,Num, Count) -> 
    Array = State#thing_change_param_rec.slots, %% Берем массив слотов
    Slot = array:get(Num, Array),                                     %%   берем слот
    NewSlot = Slot#slot{count = Slot#slot.count+Count},                 %% меняем кол-во
    ArrayNew = array:set(Num, NewSlot, Array),                   %% собираем новый массив слота
    ArrayNew
.


%% названия разделов

get_razdel_name(Razdel_num) ->
case (Razdel_num) of
    %%0 ->  <<"Игровая валюта">>;
    0 ->  libs_functions:base64_encode("Игровая валюта");

    %%1 ->  <<"Премиум аккаунт">>;
    1 ->  libs_functions:base64_encode("Премиум аккаунт");
    2 ->  libs_functions:base64_encode("Инвентарь");
    3 ->  libs_functions:base64_encode("Магазин");
    4 ->  libs_functions:base64_encode("Аватары")
    end
.

get_razdel_descr(Razdel_num) ->
case (Razdel_num) of
    %%0 ->  <<"Игровая валюта">>;
    0 ->  libs_functions:base64_encode(libs_functions:make_tooltip("В этом разделе отображается баланс[br]вашего личного счета. За кредиты[br]приобретаются монеты войны, [br]премиум-аккаунты и эксклюзивные[br]аватары. За монеты войны покупаются[br]остальные предметы."));

    %%1 ->  <<"Премиум аккаунт">>;
    1 ->  libs_functions:base64_encode(libs_functions:make_tooltip("Премиум-аккаунт увеличит ваши шансы[br]на победу! Он даст вам все виды бонусов[br]в неограниченном количестве на целую неделю."));
    2 ->  libs_functions:base64_encode(libs_functions:make_tooltip("В этом разделе отображаются имеющиеся[br]у вас бонусы и их количество."));
    3 ->  libs_functions:base64_encode(libs_functions:make_tooltip("В этом разделе приобретаются отдельные[br]виды бонусов за монеты войны."));
    4 ->  libs_functions:base64_encode(libs_functions:make_tooltip("В этом разделе приобретаются новые аватары."))
    end
.


%% названия групп

get_group_name(Razdel_num, Group_num) ->
case (Razdel_num) of
    0 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode("Игровая валюта")
           end;
    1 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode("Премиум аккаунт")
           end;
    2 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode("Тактика");
                1 ->  libs_functions:base64_encode("Стратегия");
                2 ->  libs_functions:base64_encode("Резерв")
           end;
    3 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode("Тактика");
                1 ->  libs_functions:base64_encode("Стратегия");
                2 ->  libs_functions:base64_encode("Резерв")
           end;
    4 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode("Автары");
                1 ->  libs_functions:base64_encode("Эксклюзивные автары")
           end
    end
.

get_group_descr(Razdel_num, Group_num) ->
case (Razdel_num) of
    0 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode("Игровая валюта")
           end;
    1 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode("Премиум аккаунт")
           end;
    2 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode(libs_functions:make_tooltip("Эти бонусы применяются в любом режиме[br]и влияют на расположение фишек на игровом[br]поле. С премиум-аккаунтом количество[br]бонусов «Тактика» не ограничено."));
                1 ->  libs_functions:base64_encode(libs_functions:make_tooltip("Эти бонусы применяются в боях с другими[br]игроками и турнирах. Они не изменяют[br]положение на игровом поле, но влияют[br]на ход всего боя. С премиум-аккаунтом[br]количество бонусов «Стратегия» не ограничено."));
                2 ->  libs_functions:base64_encode(libs_functions:make_tooltip(""))
           end;
    3 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode(libs_functions:make_tooltip("Эти бонусы применяются в любом режиме[br]и влияют на расположение фишек на игровом[br]поле. С премиум-аккаунтом количество[br]бонусов «Тактика» не ограничено."));
                1 ->  libs_functions:base64_encode(libs_functions:make_tooltip("Эти бонусы применяются в боях с другими[br]игроками и турнирах. Они не изменяют[br]положение на игровом поле, но влияют[br]на ход всего боя. С премиум-аккаунтом[br]количество бонусов «Стратегия» не ограничено."));
                2 ->  libs_functions:base64_encode(libs_functions:make_tooltip(""))
           end;
    4 ->  case (Group_num) of
                0 ->  libs_functions:base64_encode(libs_functions:make_tooltip("Простые аватары приобретаются за монеты войны."));
                1 ->  libs_functions:base64_encode(libs_functions:make_tooltip("Купленная вами эксклюзивная аватара[br]будет только у вас. Будьте уникальны![br]Эксклюзивные аватары приобретаются[br]за кредиты."))
                
           end
    end
.



%%--------------------------------------------------------------------
%% @doc выдает ID вещи от слота
%% @spec get_thing_id(Slot) -> integer()
%% @end
%%--------------------------------------------------------------------
get_thing_id([]) ->
[];
get_thing_id(Slot) ->
    100+Slot.


get_timestamp() ->
    %%{Mega,Sec,Micro} = erlang:now(),
    %% (Mega*1000000+Sec)*1000000+Micro
    calendar:datetime_to_gregorian_seconds(erlang:localtime())
.

int_timestamp_out(Interval_of_day) ->
    get_timestamp()+Interval_of_day*86400
.

get_auto_buy(State) ->
    Tid = get_timestamp()+State#state.tid,
    gen_server:cast(?SERVER, {incr_tid}),
    Tid
.