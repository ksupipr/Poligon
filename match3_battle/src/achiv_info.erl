%%%-------------------------------------------------------------------
%%% File    : achiv_info.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Достижения. Описание
%%%
%%% Created :  10 May 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(achiv_info).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").


-include("achiv.hrl").

-include("shared.hrl").

%% API
-export([start_link/0, show_info/2, all_achivs_arr/0, get_achivs_list/0, get_dell_list/1, get_group_list_by_id/1]).

-import(libs_functions, [base64_encode/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
%%  redis_link %% линк редиса
  achivs_list = array:new([{size, 200}, {default, 0}, {fixed, true}])
, group_list  = array:new([{size, 200}, {default, []}, {fixed, true}])
, dell_list   = array:new([{size, 10}, {default, []}, {fixed, true}])
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
    ?INFO_MSG("achiv_info start: ~p; ", [Sl]),
    Sl
.


%%====================================================================
%% gen_server callbacks
%%====================================================================

show_info(Achiv_id, Type) ->
        gen_server:call(?SERVER, {show_info, Achiv_id, Type}).


get_group_list_by_id(Achiv_id) ->
        gen_server:call(?SERVER, {get_group_list_by_id, Achiv_id}).


all_achivs_arr() ->
    gen_server:call(?SERVER, {all_achivs_arr}).


get_dell_list(Num) ->
    gen_server:call(?SERVER, {get_dell_list, Num}).


get_achivs_list() ->
    gen_server:cast(?SERVER, {get_achivs_list}).


%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Any) ->

    Name = <<"achiv_info">>,
    gproc:add_local_name(Name),

    State = #state{},
    State2 = get_achivs_list(State),
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
handle_call({show_info, Achiv_id, Type}, _From, State) ->
    Result = show_info(Achiv_id, Type, State),
%?INFO_MSG("achiv_info end ~p ~n; ", [Result]),
{reply, Result, State};


handle_call({get_group_list_by_id, Achiv_id}, _From, State) ->
    Result = get_group_list_by_id(Achiv_id, State),
{reply, Result, State};

handle_call({all_achivs_arr}, _From, State) ->
    Result = State#state.achivs_list,
{reply, Result, State};


handle_call({get_dell_list, Num}, _From, State) ->
    Result_arr = State#state.dell_list,
    Result = array:get(Num, Result_arr),
{reply, Result, State};
handle_call(get_state_size, _From, State) ->
    Result =  erts_debug:flat_size(State),
{reply, Result, State};
handle_call(get_state, _From, State) ->
{reply, State, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({show_info, Achiv_id, Type}, State) ->
    show_info(Achiv_id, Type, State),
{noreply, State};


handle_cast({get_achivs_list}, State) ->
    NewState = get_achivs_list(State),
{noreply, NewState};


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
%% @doc Формирование стейта, в честности пункта achivs_list
%% @spec  get_achivs_list(State) -> #state.achivs_list.
%% @end
%%--------------------------------------------------------------------

get_achivs_list(State) ->

case (httpc:request(get, {lists:flatten([?RES_DIR, "res/locale/ru_RU/merits.txt"]), []}, [], [])) of 
    {ok, Inner_text} -> %{ok, {{"HTTP/1.1",405, "Not Allowed"}, [], ""}}
                        {{_Prot, ReqNum, Mess}, _Any, FText} = Inner_text,
                        case (ReqNum) of
                            200 -> % все норм
                                    %?INFO_MSG("get_achivs_list:  FText: ~p;~n", [mochijson2:decode(FText)]),
                                    {struct,[{<<"merits">>, Json_list_params } ] } = mochijson2:decode(FText),

                                    RRRR = make_json_list_params(Json_list_params),
                                    Dell_array = array:new([{size, 10}, {default, []}, {fixed, true}]),
                                    Group_list_array = array:new([{size, 200}, {default, []}, {fixed, true}]),

                                    GetDellAchiv = fun(_Index, A, AccIn) ->
                                        {DellAccIn, GroupAccIn} = AccIn,
                                        if ((A#achiv.dell_type) > 0) ->
                                                Dell_array_now = array:get(A#achiv.dell_type, DellAccIn),
                                                Dell_out_Accin = array:set(A#achiv.dell_type, lists:flatten([Dell_array_now, [{A#achiv.id, 0, 1}]]), DellAccIn);
                                            true -> Dell_out_Accin = DellAccIn
                                        end,

                                        if ((A#achiv.group) > 0) ->
                                                Group_array_now = array:get(A#achiv.group, GroupAccIn),
                                                Group_out_Accin = array:set(A#achiv.group, lists:flatten([Group_array_now, A#achiv.id]), GroupAccIn);
                                            true -> Group_out_Accin = GroupAccIn
                                        end,

                                        {Dell_out_Accin, Group_out_Accin}
                                    end,
                                    {Dell_arr_state, Group_list_state} = array:foldl(GetDellAchiv, {Dell_array, Group_list_array}, RRRR),
                                    NewState= State#state{ achivs_list = RRRR, dell_list=Dell_arr_state, group_list=Group_list_state };
                                    
                              _ -> ?INFO_MSG("get_achivs_list: Error Num: ~p; Mess: ~p;~n", [ReqNum, Mess]), NewState = State
                        end;
                  FR -> ?INFO_MSG("get_achivs_list: Error file read: ~p~n", [FR]), NewState= State
end,
    NewState
.

binary_to_erl(Value) ->
{ok,Tokens,_} = erl_scan:string(lists:flatten([binary_to_list(Value), "."])),
{ok,Term} = erl_parse:parse_term(Tokens),
Term
.

make_json_list_params(Json_list_params) ->
%Params = [{ 1, "рядовой",           "ряд.",    "", "http://tanks.github.com/images/pogony/1.png",  "", 3, 1, 0, 1, [{67, 0}], [], {[], 0, []}}]

Par_array = array:new([{size, 200}, {default, 0}, {fixed, true}]),

F_LINEP = fun (A, AccIn) ->
    
    {Key, Value} = A,
    case Key of
        <<"id">>           -> AccIn#achiv{id = Value};
        <<"group">>        -> AccIn#achiv{group = Value};
        <<"name">>         -> AccIn#achiv{name = binary_to_list(Value)};
        <<"sname">>        -> AccIn#achiv{sname = binary_to_list(Value)};
        <<"descr">>        -> AccIn#achiv{descr = binary_to_list(Value)};
        <<"add_descr">>    -> AccIn#achiv{add_descr = binary_to_list(Value)};
        <<"img">>          -> AccIn#achiv{img = Value};
        <<"icon">>         -> AccIn#achiv{icon = Value};
        <<"type">>         -> AccIn#achiv{type = Value};
        <<"t_type">>       -> AccIn#achiv{t_type = Value};
        <<"dell_type">>    -> AccIn#achiv{dell_type = Value};
        <<"stacked">>      -> AccIn#achiv{stacked = Value};
        <<"level">>        -> AccIn#achiv{level = Value};
        <<"state_params">> -> AccIn#achiv{state_params = binary_to_erl(Value)};
        <<"need_achivs">>  -> AccIn#achiv{need_achivs = binary_to_erl(Value)};
        <<"add_params">>   -> AccIn#achiv{add_params = binary_to_erl(Value)};

                 _ -> AccIn
    end
end, 


F_ALLP = fun (A, AccIn) ->
    {struct, Params_list } = A,

%    ?INFO_MSG("get_achivs_list:  Params_list ~p~n", [Params_list]),

    Line_param = lists:foldl(F_LINEP, #achiv{}, Params_list),

    array:set(Line_param#achiv.id, Line_param#achiv { get_desc_json = get_achiv_GD(Line_param#achiv.type)}, AccIn)
end,

lists:foldl(F_ALLP, Par_array, Json_list_params)
.


%%--------------------------------------------------------------------
%% @doc Запрос на получение информации по достижению. Type - тип достижения (1-звание, 2 - медаль, 3 - достижение)
%% @spec  get_achiv(Type, State) -> ok.
%% @end
%%--------------------------------------------------------------------


show_info(Achiv_id, Type, State) ->
%% возвращает информацию о вещи.
%% Type - тип инфы, 1 - рекорд, любое другое - json

Get_achiv = fun(Achiv_id_f, Achiv_array) ->

%?INFO_MSG("show_info Achiv_id: ~p~n", [Achiv_id]), 

    Achivment = array:get(Achiv_id_f, Achiv_array),

    case (Achivment) of 
        0 -> #achiv{ id = Achiv_id_f, type = 0};
        _ -> Achivment
    end
end,
Achivment = Get_achiv(Achiv_id, State#state.achivs_list),

%?INFO_MSG("show_info Achivment: ~p~n", [Achivment]), 

Achiv_type = Achivment#achiv.type,



% определяем какие призы даются при получении.. для вывода в окне

case (Type) of
3 -> % json tuple призов
    A_PARAM = fun(Type_AP, Num_AP, ID_AP) ->
        case Type_AP of
            2 -> Name_AP = things:show_info(ID_AP, name),
                { struct, [ {"type", Type_AP}, {"num", Num_AP}, {"name", base64_encode(lists:flatten(["Бонус «", Name_AP, "»"]))} ]};
            _ -> { struct, [ {"type", Type_AP}, {"num", Num_AP} ]}
        end
    end,


    Achiv_add_params = Achivment#achiv.add_params,

    %{satat, money_0, bonuses}
    %{[{id, val}], money_0, [{id, num}]}

    {Satat_add, Money_0_add, Bonuses_add} = Achiv_add_params,
    if (Money_0_add >0) -> Money_0_add_json = A_PARAM(0, Money_0_add, 0);
        true -> Money_0_add_json = []
    end,

    % хак... статистика может менятся любая, но клиент знает только про рейтинг =/ ... поэтому забиваем и смотрим только 1ю в списке
    case (Satat_add) of
        [] -> Satat_add_json = [];
        _ -> 
                [Satat_add1 | _Satat_addT] = Satat_add,
                {Satat_add2, Satat_add3, _Add} = Satat_add1,
                Satat_add_json = A_PARAM(1, Satat_add3, Satat_add2)
    end,
    
    
    F_BONUSES = fun(A, AccIn) ->
        {Id_b, Num_b} = A,
        case (Id_b) of
            M when is_list(M) -> N = libs_functions:arand(M),
                                 Id_b_in = lists:nth(N, M);
                            _ -> Id_b_in = Id_b
        end,
        lists:flatten([AccIn, [A_PARAM(2, Num_b, (Id_b_in+100))]])
    end,
    
    Rezult_list_arr = lists:foldl(F_BONUSES, [], Bonuses_add),
    
    Prize_achiv = lists:flatten([[Money_0_add_json], [Satat_add_json], Rezult_list_arr]);

6 -> %% текстовый вывод призов
    Achiv_add_params = Achivment#achiv.add_params,
    {Satat_add, Money_0_add, Bonuses_add} = Achiv_add_params,

     if (Money_0_add >0) -> Money_0_add_json = lists:flatten(["монеты войны (", integer_to_list(Money_0_add), " шт.), "]);
        true -> Money_0_add_json = []
    end,

    case (Satat_add) of
        [] -> Satat_add_json = [];
        _ -> 
                [Satat_add1 | _Satat_addT] = Satat_add,
                {_Satat_add2, Satat_add3, _Add} = Satat_add1,
                Satat_add_json = lists:flatten([integer_to_list(Satat_add3), " к рейтингу, "])
    end,

      F_BONUSES = fun(A, AccIn) ->
        {Id_b, Num_b} = A,
        case (Id_b) of
            M when is_list(M) -> N = libs_functions:arand(M),
                                 Id_b_in = lists:nth(N, M);
                            _ -> Id_b_in = Id_b
        end,

        Name_AP = things:show_info((Id_b_in+100), name),

        lists:flatten([AccIn, lists:flatten(["бонус «", Name_AP, "» (", integer_to_list(Num_b), "шт.), "]) ])
    end,
    
    Rezult_list_arr = lists:foldl(F_BONUSES, [], Bonuses_add),

    Prize_achiv1 = lists:flatten([[Satat_add_json], [Money_0_add_json], Rezult_list_arr]),
    if (length(Prize_achiv1)>0) -> 
                                Prize_achiv  =  lists:flatten([" Вознаграждение – ", lists:sublist(Prize_achiv1, 1, (length(Prize_achiv1)-2)), "."]);
        true -> Prize_achiv  = []
    end;

    _ -> Prize_achiv = []
end,

%?INFO_MSG("achiv_info Prize_achiv:~p ~n; ", [Prize_achiv]), 

    case (Type) of
        type      -> Achivment#achiv.type;
        level     -> Achivment#achiv.level;
        name      -> Achivment#achiv.name;
        add_descr -> Achivment#achiv.add_descr;
        descr when Achiv_type == 3 -> Achivment#achiv.descr;
        descr -> libs_functions:make_tooltip(Achivment#achiv.descr);
        group -> Achivment#achiv.group;
        1 -> Achivment;
        3 when Achiv_type == 1 -> 
                mochijson2:encode({struct,[
                    {"header", base64_encode("ПОЗДРАВЛЯЕМ!")},
                    {"line1",  base64_encode("Получено новое достижение:")},
                    {"line2",  base64_encode(Achivment#achiv.name)},
                    {"line3",  base64_encode(Achivment#achiv.descr)},
                    {"type", Achivment#achiv.type},
                    {"img", Achivment#achiv.img},
                    { "prize", {array, 
                     Prize_achiv
                    }}
                ]});

        3 when Achiv_type == 2 -> 
                mochijson2:encode({struct,[
                    {"header", base64_encode("ПОЗДРАВЛЯЕМ!")},
                    {"line1",  base64_encode("Получена новая награда:")},
                    {"line2",  base64_encode(Achivment#achiv.name)},
                    {"line3",  base64_encode(Achivment#achiv.descr)},
                    {"type", Achivment#achiv.type},
                    {"img", Achivment#achiv.img},
                    { "prize", {array, 
                     Prize_achiv
                    }}
                ]});
        3 when Achiv_type == 3 -> 
                mochijson2:encode({struct,[
                    {"header", base64_encode("ПОЗДРАВЛЯЕМ!")},
                    {"line1",  base64_encode("Получено новое звание:")},
                    {"line2",  base64_encode(Achivment#achiv.name)},
                    {"line3",  base64_encode(lists:flatten(["Необходимо ", Achivment#achiv.descr]))},
                    {"type", Achivment#achiv.type},
                    {"img", Achivment#achiv.img},
                    { "prize", {array, 
                     Prize_achiv
                    }}
                ]});
        3 when Achiv_type == 4 -> 
                mochijson2:encode({struct,[
                    {"header", base64_encode("ПОЗДРАВЛЯЕМ!")},
                    {"line1",  base64_encode("Получено")},
                    {"line2",  base64_encode(Achivment#achiv.name)},
                    {"line3",  base64_encode(Achivment#achiv.descr)},
                    {"type", Achivment#achiv.type},
                    {"img", Achivment#achiv.img},
                    { "prize", {array, 
                     Prize_achiv
                    }}
                ]});
        3  -> 
                mochijson2:encode({struct,[
                    {"header", base64_encode("ПОЗДРАВЛЯЕМ!")},
                    {"line1",  base64_encode("Получено")},
                    {"line2",  base64_encode(Achivment#achiv.name)},
                    {"line3",  base64_encode(Achivment#achiv.descr)},
                    {"type", Achivment#achiv.type},
                    {"img", Achivment#achiv.img},
                    { "prize", {array, 
                     Prize_achiv
                    }}
                ]});

        {4, Qntty} -> [
                {"id", Achiv_id},
                {"descr", base64_encode(libs_functions:make_tooltip(Achivment#achiv.descr))},
                {"name", base64_encode(Achivment#achiv.name)},
                {"img", Achivment#achiv.img},
                {"qntty", Qntty}
                ];
        {5, Date} -> [
                {"id", Achiv_id},
                {"date", get_date_out(Date)}
                ];
        6 when Achiv_type == 1 -> 
                lists:flatten(["<font color=\"#4f7942\">", " Получено достижение <b>", Achivment#achiv.name, "</b>! ", Prize_achiv, "</font> "])
                ;
        6 when Achiv_type == 2 -> 
                lists:flatten(["<font color=\"#4f7942\">", " Получена награда <b>", Achivment#achiv.name, "</b>! ", Prize_achiv, "</font> "])
                ;
        6 when Achiv_type == 3 -> 
                lists:flatten(["<font color=\"#4f7942\">", " Получено звание <b>", Achivment#achiv.name, "</b>! ", Prize_achiv, "</font> "])
                ;
        6 -> 
                lists:flatten(["<font color=\"#4f7942\">", " Получено <b>", Achivment#achiv.name, "</b>! ", Prize_achiv, "</font> "])
                ;
        _ -> 

            Res = (Achivment#achiv.get_desc_json)(Achivment),
            LLR1 = <<"\"achiv">> ,
            LLR11 = <<"\": ">> ,
            LLR2 = <<"  ">> ,

%%            case (Bonus_id) of 
%%                W when W <  100  -> Slot = chivment#achiv.slot;
%%                M when ((M >= 100) and (M < 1000))  -> Slot = Bonus#thing.slot rem 3;
%%                N when N >= 1000 -> Slot = Bonus#thing.slot
%%            end,
%%            Slot_out = integer_to_list(Slot),
Slot_out = "",
            lists:flatten([LLR1, Slot_out, LLR11, Res,LLR2])
            
 end
.


get_group_list_by_id(Achiv_id, State) ->

    Achiv_group = show_info(Achiv_id, group, State),
    Group_list = array:get(Achiv_group, State#state.group_list),

    case (Group_list) of 
        0 -> [];
        _ -> Group_list
    end
.

get_achiv_GD(Achiv_type) ->
    case (Achiv_type) of

    1 -> 
        fun(Achiv) ->
             mochijson2:encode({struct,[
                {"id",        Achiv#achiv.id},
                {"name",      base64_encode(Achiv#achiv.name)},
                {"descr",     base64_encode(libs_functions:make_tooltip(Achiv#achiv.descr))},
                {"type",      Achiv#achiv.type},
                {"img",       Achiv#achiv.img},
                {"icon",      Achiv#achiv.icon},
                {"stacked",   Achiv#achiv.stacked},
                {"level",     Achiv#achiv.level}
            ]})
        end; 
    _ -> 
        fun(Achiv) ->
             mochijson2:encode({struct,[
                {"id",        Achiv#achiv.id},
                {"name",      base64_encode(Achiv#achiv.name)},
                {"descr",     base64_encode(libs_functions:make_tooltip(Achiv#achiv.descr))},
                {"type",      Achiv#achiv.type},
                {"img",       Achiv#achiv.img},
                {"icon",      Achiv#achiv.icon},
                {"stacked",   Achiv#achiv.stacked},
                {"qntty",     Achiv#achiv.qntty},
                {"level",     Achiv#achiv.level},
                {"get_date",  Achiv#achiv.get_date},
                {"dell_date", Achiv#achiv.dell_date}
            ]})
        end 
    end
.

get_date_out(PTimestamp) ->
    get_date_out(PTimestamp, 1).

get_date_out(PTimestamp, Type) ->
    P8 = fun(A) -> 
         case A of
             A when is_integer(A),A<10 -> ["0", integer_to_list(A)];
             B when is_integer(B) -> [integer_to_list(B)];
                     _ ->[]
                 end
     end,

         {{Year,Month,Day}, {Hour,Minutes,_Seconds}} = calendar:gregorian_seconds_to_datetime(PTimestamp),
%%{{1970,1,1},{0,0,0}}
%% , " ", P8(Hour), ":", P8(Minutes)
    case (Type) of 
        2 -> Datetime = lists:flatten([P8(Day), ".", P8(Month), ".", integer_to_list(Year), " ", P8(Hour), ":", P8(Minutes)]);
        _ -> Datetime = lists:flatten([P8(Day), ".", P8(Month), ".", integer_to_list(Year)])
    end,
list_to_binary(Datetime)
.
