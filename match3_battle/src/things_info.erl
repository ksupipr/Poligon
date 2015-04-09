%%%-------------------------------------------------------------------
%%% File    : things_info.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Вещи. Описание
%%%
%%% Created :  10 May 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(things_info).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").

-include("shop.hrl").

-include("shared.hrl").

%% API
-export([start_link/0, show_info/2, all_things_arr/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
  redis_link %% линк редиса
, things_list = array:new([{size, 1}, {default, 0}, {fixed, false}])
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
    ?INFO_MSG("things_info start: ~p; ", [Sl]),
    Sl
.


%%====================================================================
%% gen_server callbacks
%%====================================================================

show_info(Thing_id, Type) ->
        gen_server:call(?SERVER, {show_info, Thing_id, Type}).

all_things_arr() ->
    gen_server:call(?SERVER, {all_things_arr}).


%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Any) ->

    Name = <<"things_info">>,
    gproc:add_local_name(Name),



    State = #state{},
    State2 = get_things_list(State),
    {ok, State2}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({show_info, Thing_id, Type}, _From, State) ->
    Result = show_info(Thing_id, Type, State),
{reply, Result, State};

handle_call({all_things_arr}, _From, State) ->
    Result = State#state.things_list,
{reply, Result, State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

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


%%--------------------------------------------------------------------
%% @doc Формирование стейта
%% @spec  get_things_list(State) -> #state.achivs_list.
%% @end
%%--------------------------------------------------------------------

get_things_list(State) ->

case (httpc:request(get, {lists:flatten([?RES_DIR, "res/locale/ru_RU/things.txt"]), []}, [], [])) of 
    {ok, Inner_text} -> %{ok, {{"HTTP/1.1",405, "Not Allowed"}, [], ""}}
                        {{_Prot, ReqNum, Mess}, _Any, FText} = Inner_text,
                        case (ReqNum) of
                            200 -> % все норм
                                    {struct,[{<<"things">>, Json_list_params } ] } = mochijson2:decode(FText),
                                    RRRR = make_json_list_params(Json_list_params),
                                    NewState= State#state{ things_list = RRRR };
                              _ -> ?INFO_MSG("get_things_list: Error Num: ~p; Mess: ~p;~n", [ReqNum, Mess]), NewState = State
                        end;
                  FR -> ?INFO_MSG("get_things_list: Error file read: ~p~n", [FR]), NewState= State
end,

    NewState
.

binary_to_erl(Value) ->
{ok,Tokens,_} = erl_scan:string(lists:flatten([binary_to_list(Value), "."])),
{ok,Term} = erl_parse:parse_term(Tokens),
Term
.

make_json_list_params(Json_list_params) ->

Par_array = array:new([{size, 1}, {default, 0}, {fixed, false}]),

F_LINEP = fun (A, AccIn) ->
    
    {Key, Value} = A,

    case Key of
        <<"id">>           -> AccIn#thing{id = Value};
        <<"name">>         -> AccIn#thing{name = binary_to_list(Value)};
        <<"descr">>        -> AccIn#thing{descr = libs_functions:make_tooltip(binary_to_list(Value))};
        <<"img">>          -> AccIn#thing{img = Value};
        <<"type">>         -> AccIn#thing{type = Value};
        <<"slot">>         -> AccIn#thing{slot = Value};
        <<"price">>        -> AccIn#thing{price = binary_to_erl(Value)};
        <<"party">>        -> AccIn#thing{party = Value};
        <<"num">>          -> AccIn#thing{num = 0};
                 _ -> AccIn
    end
end, 


F_ALLP = fun (A, AccIn) ->
    {struct, Params_list } = A,

%    ?INFO_MSG("get_achivs_list:  Params_list ~p~n", [Params_list]),

    Line_param = lists:foldl(F_LINEP, #thing{}, Params_list),

    array:set(Line_param#thing.id, Line_param#thing { apply_thing = get_bonus_AT(Line_param#thing.id), get_desc_json = get_bonus_GD(Line_param#thing.id)}, AccIn)
end,

lists:foldl(F_ALLP, Par_array, Json_list_params)
.


show_info(Th_id, Type, State) ->
%% возвращает информацию о вещи.



Get_thing = fun(Th_id_f, Th_array) ->
    Thing = array:get(Th_id_f, Th_array),

%?INFO_MSG("Th_id_f: ~p~n ", [Th_id_f]),  

    case (Thing) of 
        0 -> % если не могу найти предмет, то проверяю, а не аватара ли это эксклюзивная
             if (Th_id_f>=2000) ->
                    % если таки аватара, то рисую ее
                    AImg = ava_info:show_info(Th_id_f, img),
%?INFO_MSG("AImg: ~p~n ", [AImg]),  
                    #thing{ id    = Th_id_f, 
                            type  = 2000, 
                            img   = AImg, 
                            name  = "Эксклюзивный аватар", 
                            descr = "Эксклюзивный аватар", 
                            slot  = Th_id_f-2000, 
                            price = {0, 20},
                            party = 1,
                            get_desc_json = get_bonus_GD(Th_id_f)
                          };
                true -> #thing{ id = Th_id_f, type = 0, slot=0, get_desc_json = get_bonus_GD(Th_id_f)}
            end;
        _ -> Thing
    end
end,
Thing = Get_thing(Th_id, State#state.things_list),

Th_type = Thing#thing.type,

   case (Type) of
        1 -> Thing;
        id -> Thing#thing.id;
        name -> Thing#thing.name;
        descr -> Thing#thing.descr;
        img -> Thing#thing.img;
        type -> Thing#thing.type;
        slot -> Thing#thing.slot;
        price -> Thing#thing.price;
        party -> Thing#thing.party;
        get_desc_json -> Thing#thing.get_desc_json;
        {6, Qntty} when Th_type == 100 ->
                lists:concat(["<font color=\"#333333\"> Приобретен бонус «<b>", Thing#thing.name, "</b>» (", Qntty, "шт.). Потрачено ", money_GFO(Thing#thing.price, Qntty, Thing#thing.party) ," </font>"])
                ;
        {6, Qntty} when Th_type == 1 ->
                lists:concat(["<font color=\"#333333\"> Приобретены <b>", Thing#thing.name, "</b> (", Qntty, "шт.). </font>"])
                ;
        {6, Qntty} when Th_type == 2 ->
                lists:concat(["<font color=\"#333333\"> Приобретены <b>", Thing#thing.name, "</b> (", Qntty, "шт.). Потрачено ", money_GFO(Thing#thing.price, Qntty, Thing#thing.party) ," </font>"])
                ;
        {6, Qntty} when Th_type == 3 ->
                Date_prem = binary_to_list(libs_functions:get_date_out(libs_functions:int_timestamp_out(Thing#thing.party), 2)),
                lists:concat(["<font color=\"#333333\"> Приобретен премиум-аккаунт. Вам доступны все виды бонусов в неограниченном количестве. Срок действия премиум-аккаунта – до <b>", Date_prem, "</b>. Потрачено ", money_GFO(Thing#thing.price, Qntty, Thing#thing.party) ," </font>"])
                ;
        {6, Qntty} ->
                lists:concat(["<font color=\"#333333\"> Приобретен <b>", Thing#thing.name, "</b> (", Qntty, "шт.). Потрачено ", money_GFO(Thing#thing.price, Qntty, Thing#thing.party) ," </font>"])
                ;
        _ -> 
            Res = (Thing#thing.get_desc_json)(Thing),
            LLR1 = <<"\"slot">> ,
            LLR11 = <<"\": ">> ,
            LLR2 = <<"  ">> ,

            case (Th_type) of 
                W when W <  100  -> Slot = Thing#thing.slot;
                M when ((M >= 100)  and (M < 1000)) -> Slot = Thing#thing.slot rem 3;
                N when ((N >= 1000) and (N < 2000)) -> Slot = Thing#thing.id - 1000;
                K when  (K >= 2000) -> Slot = Thing#thing.id - 2000
            end,

            lists:flatten([LLR1, integer_to_list(Slot), LLR11, Res,LLR2])
            
 end
.


money_GFO(Price, Qntty, Party) ->

    if (Party>0) -> Party_in = Party;
        true -> Party_in = 1
    end,

    case (Qntty) of
         Q when is_list(Q) -> Qntty_in = list_to_integer(Q);
         B when is_binary(B) -> Qntty_in = list_to_integer(binary_to_list(B));
                         _ -> Qntty_in = Qntty
    end,

    {M0, M1} = Price,
    case M0 of 
        M when M >0 -> lists:flatten([integer_to_list((Qntty_in div Party_in)*M)," монет войны "]);
                  _ -> lists:flatten([integer_to_list((Qntty_in div Party_in)*M1)," кредитов "])
    end
.

%%--------------------------------------------------------------------
%% @doc Выдает функцию для активацию в слоте вещи
%% @spec   get_bonus_AT(Bonus_id) -> fun().
%% @end
%%--------------------------------------------------------------------
get_bonus_AT(Bonus_id) ->
%%       Fun(Slot_param_arr,
%%                                    New_state#state.battle_serv,New_state#state.battle_id,
%%                                    First_user, New_state#state.player0, New_state#state.player1)
%% похуй что вернет анонимная функция 

case (Bonus_id) of
    100 -> 
     fun(Slot_param_arr, Battle_serv,Battle_id, First_user, User0,User1) ->
           %% 100 ->Напалм
         Num = array:get(0, Slot_param_arr),

            S1 = unicode:characters_to_binary("{\"slot\":{\"id\":0, \"player\":"),
            S2 = unicode:characters_to_binary(", \"param0\":"),
            S3 = unicode:characters_to_binary("}}"),
            Slot1 = list_to_binary([S1,integer_to_list(1),S2,integer_to_list(Num),S3]),
            Slot2 = list_to_binary([S1,integer_to_list(2),S2,integer_to_list(Num),S3]),
            gen_server:cast(Battle_serv, {line, Battle_id, First_user, User0,User1,Slot1,Slot2,Num})
            end; 
    101 -> 
     fun(Slot_param_arr, Battle_serv,Battle_id, First_user, User0,User1) ->
           %% 101 -> крест
         Num = array:get(0, Slot_param_arr),
         Num1 = array:get(1, Slot_param_arr),
            S1 = unicode:characters_to_binary("{\"slot\":{\"id\":1, \"player\":"),
            S2 = unicode:characters_to_binary(", \"param0\":"),
            S3 = unicode:characters_to_binary(", \"param1\":"),
            S5 = unicode:characters_to_binary("}}"),
            Slot1 = list_to_binary([S1,integer_to_list(1),S2,integer_to_list(Num),S3,integer_to_list(Num1),S5]),
            Slot2 = list_to_binary([S1,integer_to_list(2),S2,integer_to_list(Num),S3,integer_to_list(Num1),S5]),
            gen_server:cast(Battle_serv, {boom, Battle_id, First_user, User0,User1,Slot1,Slot2,{Num,Num1}})
            end; 
    102 -> 
     fun(_Slot_param_arr, Battle_serv,Battle_id, First_user, User0,User1) ->
           %% 102 -> рикошет
             Num = 0,
            S1 = unicode:characters_to_binary("{\"slot\":{\"id\":2, \"player\":"),
            S2 = unicode:characters_to_binary(", \"param0\":"),
            S3 = unicode:characters_to_binary("}}"),
            Slot1 = list_to_binary([S1,integer_to_list(1),S2,integer_to_list(Num),S3]),
            Slot2 = list_to_binary([S1,integer_to_list(2),S2,integer_to_list(Num),S3]),
          %?INFO_MSG(" random12 : ~p~n", [First_user]),
            gen_server:cast(Battle_serv, {random_cell, Battle_id, First_user, User0,User1,Slot1,Slot2,Num})
            end; 
    103 -> 
     fun(_Slot_param_arr, Battle_serv,Battle_id, First_user, User0,User1) ->
           %% 103 -> <<"Атака ПВП">>;
             Dp=?DP_ATACK, Type=0,
            Num = {Type,Dp},
            S1 = unicode:characters_to_binary("{\"slot\":{\"id\":3, \"player\":"),
            S3 = unicode:characters_to_binary("}}"),
            Slot1 = list_to_binary([S1,integer_to_list(1),S3]),
            Slot2 = list_to_binary([S1,integer_to_list(2),S3]),
            gen_server:cast(Battle_serv, {change_health, Battle_id, First_user, User0,User1,Slot1,Slot2,Num})
            end; 
    104 -> 
     fun(_Slot_param_arr, Battle_serv,Battle_id, First_user, User0,User1) ->
           %% 104 -> <<"Защита ПВП">>;
            Dp=?DP_ARMOR, Type=1,
              Num = {Type,Dp},
            S1 = unicode:characters_to_binary("{\"slot\":{\"id\":4, \"player\":"),
            S3 = unicode:characters_to_binary("}}"),
            Slot1 = list_to_binary([S1,integer_to_list(1),S3]),
            Slot2 = list_to_binary([S1,integer_to_list(2),S3]),
            gen_server:cast(Battle_serv, {change_health, Battle_id, First_user, User0,User1,Slot1,Slot2,Num})
            end; 
    105 -> 
     fun(_Slot_param_arr, Battle_serv,Battle_id, First_user, User0,User1) ->
           %% 105 -> <<"Свобода ПВП">>;
              Num = 0,
            S1 = unicode:characters_to_binary("{\"slot\":{\"id\":5, \"player\":"),
            S3 = unicode:characters_to_binary("}}"),
            Slot1 = list_to_binary([S1,integer_to_list(1),S3]),
            Slot2 = list_to_binary([S1,integer_to_list(2),S3]),
            gen_server:cast(Battle_serv, {no_change, Battle_id, First_user, User0,User1,Slot1,Slot2,Num})
            end; 
    _ ->   
        fun(_Slot_param_arr, _Battle_serv,_Battle_id, First_user, _User0,_User1 ) ->
            ?INFO_MSG("error MSG: ~p~n", [First_user])
           end
 %% для аватар тип  общий 9
    end
.
%%--------------------------------------------------------------------
%% @doc Выдает функцию которая выдает описание вещи в JSON
%% @spec   get_bonus_GD(Bonus_id) -> fun().
%% @end
%%--------------------------------------------------------------------

get_bonus_GD(Bonus_id) ->
    case (Bonus_id) of

    1 -> 
        fun(Bonus) ->
            {Money0, Money1} = Bonus#thing.price,

             mochijson2:encode({struct,[
                {"name",      libs_functions:base64_encode(Bonus#thing.name)},
                {"descr",     libs_functions:base64_encode(Bonus#thing.descr)},
                {"type",      Bonus#thing.type},
                {"id",        Bonus_id },
                {"party",     Bonus#thing.party},
                {"price0",    Money0},
                {"price1",    Money1}
            ]})
                             end; 

    2 -> 
        fun(Bonus) ->
            {Money0, Money1} = Bonus#thing.price,

             mochijson2:encode({struct,[
                {"name",      libs_functions:base64_encode(Bonus#thing.name)},
                {"descr",     libs_functions:base64_encode(Bonus#thing.descr)},
                {"type",      Bonus#thing.type},
                {"id",        Bonus_id },
                {"party",     Bonus#thing.party},
                {"price0",    Money0},
                {"price1",    Money1}
            ]})
                             end;  

    3 -> 
        fun(Bonus) ->
            {Money0, Money1} = Bonus#thing.price,

             mochijson2:encode({struct,[
                {"name",       libs_functions:base64_encode(Bonus#thing.name)},
                {"descr",      libs_functions:base64_encode(Bonus#thing.descr)},
                {"type",       Bonus#thing.type},
                {"id",         Bonus_id },
                {"party",      Bonus#thing.party},
                {"num",        Bonus#thing.num},
                {"time_limit", Bonus#thing.time_limit},
                {"price0",     Money0},
                {"price1",     Money1}
            ]})
                             end;

    M when M >= 100, M < 1000 -> 
        fun(Bonus) ->
            {Money0, Money1} = Bonus#thing.price,

            mochijson2:encode({struct,[
                {"name",      libs_functions:base64_encode(Bonus#thing.name)},
                {"descr",     libs_functions:base64_encode(Bonus#thing.descr)},
                {"img",       Bonus#thing.img},
                {"type",      Bonus#thing.type},
                {"id",        Bonus_id },
                {"party",     Bonus#thing.party},
                {"slot",      Bonus#thing.slot},
                {"price0",    Money0},
                {"price1",    Money1}
            ]})
                             end; 
    _ ->   
        fun(Bonus) ->
       %%   ?INFO_MSG("Ava Buy : ~p~n", [Bonus_id]),
            {Money0, Money1} =  Bonus#thing.price,

             mochijson2:encode({struct,[
                {"name",    libs_functions:base64_encode(Bonus#thing.name)},
                {"descr",   libs_functions:base64_encode(Bonus#thing.descr)},
                {"img",     Bonus#thing.img},
                {"type",    Bonus#thing.type},
                {"id",       Bonus_id },
                {"slot",    Bonus#thing.slot},
                {"price0",    Money0},
                {"price1",    Money1}
            ]})
           end
 %% для аватар тип  общий 9
    end
.
