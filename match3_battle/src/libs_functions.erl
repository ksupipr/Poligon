%%%-------------------------------------------------------------------
%%% File    : libs_functions.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Функции работы с текстом и не только
%%%
%%% Created :  28 Mar 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(libs_functions).

-include("poligon.hrl").
-include("shared.hrl").
%% для отладки
-include("ejabberd.hrl").
-include("jlib.hrl").

-export([base64_encode/0, base64_encode/1, base64_decode/1,  
	 p8_fun/1, get_date_out/1, get_date_out/2, int_timestamp_out/1, arand/1, get_kurs/1, 
    get_sn_prefix/1, make_tooltip/1, int_to_list/1, rand/1, binary_to_erl/1, make_jid/1, list_to_erl/1,
    floor/1, ceiling/1, alive/1
    ]).
%% приведение даты к состоянию чтоб можно было бы печатать на экран
-export([date_to_print/1
        ]).

%% экспорт функций работы с текстами логов
-export([send_log_end_battle/7
        ,send_log_to_user/4
        ]).
-export([
wrap_cron_cancel/1 %% Отменяет задачу запускаемую по таймеру  
, wrap_cron_once/2   %% функция для создания сообщения(запуска функции) через указанное время 
, get_base_slots/1
, sn_id_is_admin/1
, add_zero/1 %% добавляет ведущий 0 в логах числа
        ]).


%%--------------------------------------------------------------------
%% @doc Проверяет что чел из списка админов 
%% @spec sn_id_is_admin(Sn_id_bin) -> 0|1
%% @end
%%--------------------------------------------------------------------

sn_id_is_admin(A) when A == "vk_10472613";A ==  "vk_8610402"; A ==  "vk_68749263"; 
		       A == "vk_20200480"; A == "vk_9653723"; A ==  "vk_20707807";
		       A == "ml_8790300971262502834";   A == "ok_3140466967657967208"
		       ->
    1;
sn_id_is_admin(_Sn_id_bin) ->
    0.

%%--------------------------------------------------------------------
%% @doc  инициализация слотов профиля при становлении онлайн игрока
%% @spec get_base_slots(Thing_id) -> array().
%% @end
%%--------------------------------------------------------------------
get_base_slots([]) -> 

   Acc = array:new([{size, 9}, {default, 0}, {fixed, true}]),

    P8 = fun(I, AccIn) -> 
		 %% отправки сообщения и сбор Jid в один результирующий list в State
                  Value = #slot{ num = I, thing = things_info:show_info(things:get_thing_id(I), 1)}, 
                 array:set(I, Value, AccIn)
	 end,
%% ?INFO_MSG("log   : ~p~n", [2]),     
 lists:foldl(P8, Acc , lists:seq(0,8));
get_base_slots(Things_id) -> 
%% ?INFO_MSG("get_base_slots : ~p~n", [Things_id]),
   Acc = array:new([{size, 9}, {default, 0}, {fixed, true}]),
 %% 0 - 8
    P8 = fun({I,Id,Count}, AccIn) -> 
		 %% отправки сообщения и сбор Jid в один результирующий list в State

                  Value = #slot{ num = I, count=Count, thing = things_info:show_info(things:get_thing_id(Id), 1)}, 
                 array:set(I, Value, AccIn)
	 end,

    lists:foldl(P8, Acc , Things_id).


%%--------------------------------------------------------------------
%% @doc Функция рассылает сообщения в лог игрока после окончания боя
%% @spec send_log_end_battle(Jid, Num,Battle_mode,Result,Add_p,Enemyfio,Pstat)->ok.
%% @end
%%--------------------------------------------------------------------
send_log_end_battle(Jid, Battle_type_for_log, Battle_mode,
		    Result, Add_p, Enemyfio, Pstat)->

%% Battle_type_for_log == 
% 1 Поединок
% 2 Дуэль
% 3 Тренировка
% 
%
% 101 - 131  Турнир
%
    Step1 = log_end_battle_step1(Battle_mode,Battle_type_for_log,Result), %%Вид боя (Капитул)ПОбеда Проигрыш [Вид боя]

    Step2 = ["Противник – " | Enemyfio],
    Step3 =  log_end_battle_step2(Result,Battle_type_for_log,Add_p), %% Достижения если поединке дуэли тренировочном бою финале турнира
    
	


 send_log_to_user(Jid, 0, {lists:flatten([Step1,". ",Step2,". ",Step3])}, "#4c3c18"),
  case Pstat of
		  0 when Result < 1 -> Buy_prem =  ["Купите премиум-аккаунт! Он даст вам на неделю все виды бонусов в неограниченном количестве. Это увеличит ваши шансы на победу."],
				       send_log_to_user(Jid, 0, {Buy_prem}, "#ff0033") ; %%
                 0 when Result ==1 -> Buy_prem =  ["Купите премиум-аккаунт! Он даст вам все виды бонусов на целую неделю в неограниченном количестве."],
		       send_log_to_user(Jid, 0, {Buy_prem}, "#ff0033") ;
	    _ -> ok
	 end
.


%%--------------------------------------------------------------------
%% @doc Выдает вид боя (%%Вид боя (Капитул)ПОбеда Проигрыш [Вид боя])
%% @spec
%% @end
%%--------------------------------------------------------------------
log_battle_type(Battle_type_for_log)->

    case Battle_type_for_log of
	1 -> ["поединке"];
	2 -> ["дуэли"];
	3 -> ["тренировочном бою"];
	A when A >  99,A< 117 -> ["1/8 турнира"];
	B when B > 116,B< 125 -> ["1/4 турнира"];
	C when C > 124,C< 129 -> ["полуфинале турнира"];
	D when D > 128,D< 132 -> ["финале турнира"]
	end.
%%--------------------------------------------------------------------
%% @doc Выдает Результат и вид боя
%% @spec log_end_battle_step1(Battle_mode,Battle_type_for_log,_Result) ->list().
%% @end
%%--------------------------------------------------------------------
log_end_battle_step1(Battle_mode,Battle_type_for_log,Result) when Battle_mode>1,Result =/= 1 ->
    ["Капитуляция в "|log_battle_type(Battle_type_for_log)];
log_end_battle_step1(_Battle_mode,Battle_type_for_log,1) ->
    ["Победа в "|log_battle_type(Battle_type_for_log)];
log_end_battle_step1(_Battle_mode,Battle_type_for_log,_) ->
    ["Поражение в "|log_battle_type(Battle_type_for_log)].

%%--------------------------------------------------------------------
%% @doc Выдает список достижений
%% @spec log_end_battle_step2(Battle_type_for_log,{B,R}) -> list().
%% @end
%%--------------------------------------------------------------------
log_end_battle_step2(1,Battle_type_for_log,{B,R}) when Battle_type_for_log < 4;  Battle_type_for_log > 128  ->
%% результаты при победе
 RB =  case B <0 of
	   true -> (B div 3);
	   _ -> B end,
%%Вознаграждение – [перечень получаемого через запятую].
%%[N] к рейтингу
%%монеты войны ([N] шт.)
  St0 =  ["Вознаграждение – "|integer_to_list(R)],
  St1 = lists:append(St0,[" к рейтингу, монеты войны ("]),
  St2 = lists:append(St1,integer_to_list(RB)),
  lists:append(St2,[" шт.)."])
;
log_end_battle_step2(_Result,Battle_type_for_log,{B,_R}) when Battle_type_for_log < 4;  Battle_type_for_log > 128  ->
%% результаты при проигрыше
 RB =  case B <0 of
	   true -> -(B div 3);
	   _ -> B end,
  St0 =  "Снята ставка в монетах войны (",
  St2 = lists:append(St0,integer_to_list(RB)),
  lists:append(St2,[" шт.)."])
;
log_end_battle_step2(_,_,_) ->
?INFO_MSG(" step2  none : ~p ~p ~n", [1,1]),
    [].

%%--------------------------------------------------------------------
%% @doc Функция обработки логов пользователя
%% @spec send_log_to_user(User, Type, Params,Color) -> ok
%% Params = {???}
%% Type = 1 - достижения (звания, награды, достижения ...)
%% @end
%%--------------------------------------------------------------------
send_log_to_user(User, Type, Params,Color) ->
%%?INFO_MSG("set_log : ~p~n", [Color]),

    LLR1 = <<"{ \"reply\": { \"type\": 49, \"body\": { \"log\": ">>,
    LLR2 = <<"} } }">>,


    Color_in =case  Color  of
                  [] -> "#45573E";
		  A  -> A
	      end,

Date_log = date_to_print(now), 

LOG_LINE = fun(Text_in) ->
    {struct, [{"text", base64_encode(lists:flatten(["<font face=\"Verdana\" size=\"10\" color=\"", Color_in, "\">", Date_log, " ", Text_in, "</font>"]))}] }
end,
LOG_LINE_MSG = fun(Text_msg, Name_msg, Jid_mgs) ->
    [ {struct, [{"text", base64_encode(lists:flatten(["<font face=\"Verdana\" size=\"10\" color=\"", Color_in, "\">", Date_log, " <a href=\"event:", Jid_mgs, "\">", Name_msg, "<b>", Text_msg, "</b></a></font>"])) }] }]
end,
    case (Type) of
        0 -> 
            {Log_text_p} = Params,
            
            Log_text = [LOG_LINE(Log_text_p)];
        1 -> %достижения (звания, награды, достижения ...)
            {Achivs_list} = Params,
            
            case (Achivs_list) of
                [] -> Achivs_info0 = [];
                 _ -> Achivs_info0 = [ LOG_LINE(achiv_info:show_info(Achiv_id, 6))  || {Achiv_id, _ADate, _AQnnty} <- Achivs_list]
            end,
            Log_text  = Achivs_info0; %p8_fun(Achivs_info0);


        2 -> % покупка вещей 
            {Buy_id, Count_out} = Params,
            Log_text  = [LOG_LINE(things_info:show_info(Buy_id, {6, Count_out}))];

        3 -> % следующее звание
            {Rank_achiv} = Params,
            {R_id, _R_date, _R_num} = Rank_achiv,

            R_type = achiv_info:show_info(R_id+1, type),
            case (R_type) of
                RM when (RM ==3) -> R_name  = achiv_info:show_info(R_id+1, name),
                                    R_descr = achiv_info:show_info(R_id+1, descr),
                                    R_add_descr = achiv_info:show_info(R_id+1, add_descr),
                                    Log_text  = [LOG_LINE(lists:flatten(["Для получения следующего звания – <b>", R_name, "</b> – вам нужно ", R_descr , ". Вознаграждение - ", R_add_descr]))];

                               _ -> Log_text  = []
            end;


        4 -> % личные сообщения
            {Text_msg, Name_msg, Jid_mgs} = Params,

            Pid4pro=gproc:lookup_local_name(User),
                case (is_pid(Pid4pro)) of
                    true  ->  %% отправить запрос туда
                        Log_text  = [],
                        gen_server:cast(Pid4pro, {private_message, Jid_mgs, User, Text_msg, Name_msg});
                        _ -> % сообщаем что пользователь не в сети
                        Log_text  = [LOG_LINE(lists:flatten(["Сообщение не отправлено. К сожалению пользователь не в сети."]))]
                end;

        5 -> % личные сообщения
            {Text_msg, Name_msg, Jid_mgs} = Params,
            Log_text  = LOG_LINE_MSG(Text_msg, Name_msg, Jid_mgs);

        _ -> Log_text  = []
    end,
%%?INFO_MSG("set_log : ~p~n", [Log_text]),
case (Log_text) of
    [] -> ok;
     _ ->  
         %%  ?INFO_MSG("Date_log : ~p~n", [Date_log]),
%%  ?INFO_MSG("Date_log : ~p~n",lists:flatten(["<font face=\"Verdana\" size=\"10\" color=\"", Color_in, "\">", Date_log, " ", Log_text, "</font>"])),
           Log_out  = mochijson2:encode(
                    Log_text
                ),
  %%        ?INFO_MSG("set_log : ~p~n", [Log_out]),
          Log_send = list_to_binary([LLR1, Log_out, LLR2]),
          p_sender:send_msg(User,  Log_send)
end.

%%--------------------------------------------------------------------
%% @doc Вывод даты в нормальной для отображения форме
%% @spec date_to_print(DataTime) -> binary()
%% @end
%%--------------------------------------------------------------------
date_to_print(now) -> 
%%?INFO_MSG("Date_log : ~p~n", [now]),
%%calendar:gregorian_seconds_to_datetime(PTimestamp)
 date_to_print(erlang:localtime());
date_to_print(PDate) -> 
%%?INFO_MSG("Date_log : ~p~n", [PDate]),
 {{Year,Month,Day}, {Hour,Minutes,_Seconds}} = PDate,         %%{{1970,1,1},{0,0,0}}
  Datetime = lists:flatten([add_zero(Day), ".", add_zero(Month), ".", integer_to_list(Year), " ",
        add_zero(Hour), ":", add_zero(Minutes)]),
  list_to_binary(Datetime)
.

%%--------------------------------------------------------------------
%% @doc Дополняет цифру ведущим 0   %% дополнение числа которое меньше 10 ведущим нулем
%% @spec add_zero(A) -> list()
%% @end
%%--------------------------------------------------------------------
add_zero(A) when is_integer(A),A<10 -> 
    ["0", integer_to_list(A)];
add_zero(B)when is_integer(B) -> 
    [integer_to_list(B)];
add_zero(_) -> [].


%%--------------------------------------------------------------------
%% @doc Функция кодирования в модифицированный base64
%% @spec
%% @end
%%--------------------------------------------------------------------

base64_encode() ->
[].
base64_encode(Str) ->
    case Str of 
        M when is_list(M) -> Bin2 = base64:encode(list_to_binary(Str)),
                        list_to_binary(re:replace(Bin2, "/", "*", [global, {return,list}]));
        W when is_binary(W) -> Bin2 = base64:encode(Str),
                                list_to_binary(re:replace(Bin2, "/", "*", [global, {return,list}]));
        _ -> Str
    end
.

%%--------------------------------------------------------------------
%% @doc Функция декодирования модифицированный base64 
%% @spec base64_decode(Str) ->list().
%% @end
%%--------------------------------------------------------------------
base64_decode(Str) ->
    Str1= re:replace(Str,  "\\*", "/", [global, {return,binary}]),
 base64:decode(Str1).



p8_fun(List_in) ->

P8 = fun(A, AccIn) -> 
         %% анонимная функция для расстановки запятых между обьектами
         A1 = A,
         B =  unicode:characters_to_binary(","), 
              case AccIn of [] ->  [ A1 | AccIn ];
                  _ ->   M = [B | AccIn],[A1 | M ] 
              end 
     end

    %% ставим запятые
    , List_out = lists:foldl(P8,[] , List_in)
    , List_out
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

get_timestamp() ->
    %%{Mega,Sec,Micro} = erlang:now(),
    %% (Mega*1000000+Sec)*1000000+Micro
    calendar:datetime_to_gregorian_seconds(erlang:localtime())
.

int_timestamp_out(Interval_of_day) ->
    get_timestamp()+Interval_of_day*86400
.


arand(List) ->
    {{_Year, Month, Day}, {_Hour,_Minutes,_Seconds}} = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime())),
    Num4Rand = Month+Day,
    List_length = erlang:length(List),
    (Num4Rand rem (List_length))+1
%% то есть число от 1 до erlang:length(List),
.

rand(Max) ->
if (Max>0) ->
    {{_Year, _Month, _Day}, {_Hour,_Minutes,Seconds}} = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime())),
    (Seconds rem (Max))+1;
true -> 0
end
.

getKurs(Alist, Prefix) ->
case (Alist) of
    [] -> {1, 1};
    _ ->
        [A|T] = Alist,
        {Pref, K1, K2} = A,
        case (Pref) of
               M when Prefix ==  M -> {K1, K2};
                        _ -> getKurs(T, Prefix)
        end
end
.

get_kurs(Prefix) ->
Sn_to_cred = ?SN_TO_CRED,
getKurs(Sn_to_cred, Prefix)
.

get_sn_prefix(Jid) ->
case (Jid) of
    M when is_list(M) -> Jid_in = M;
    W when is_binary(W) -> Jid_in = binary_to_list(W);
               _ -> Jid_in = jlib:jid_to_string(Jid)
end,

    [Pref1, Pref2|_T]= Jid_in,
    Pref = list_to_binary([Pref1, Pref2]),
    Pref.

make_tooltip(Text) ->
    lists:flatten(["[color=\"#003300\" size=\"10\" font=\"Verdana\" leading=\"-1.5\"]", Text])
.

int_to_list(Num) ->
    case (Num) of
        M when is_integer(M) -> integer_to_list(M);
        W when is_float(W)   -> float_to_list(W);
        G when is_binary(G)  -> binary_to_list(G);
                           _ -> Num
    end
.

list_to_erl(Value) ->
{ok,Tokens,_} = erl_scan:string(lists:flatten([Value, "."])),
{ok,Term} = erl_parse:parse_term(Tokens),
Term
.

binary_to_erl(Value) ->
{ok,Tokens,_} = erl_scan:string(lists:flatten([binary_to_list(Value), "."])),
{ok,Term} = erl_parse:parse_term(Tokens),
Term
.

make_jid(UName) ->

case (UName) of
    M when is_binary(M) -> UName_str = binary_to_list(M);
    Q when is_atom(Q) -> UName_str = atom_to_list(Q);
                    _ -> UName_str = UName
end,
%%I(<0.2752.0>:match_profile:145) : init done : "ml_11756746109924451743@im.xlab.su/xiff"
%% Opened session for ml_11756746109924451743@poligon.xlab.su/xiff
lists:flatten([UName_str, ?DOMAIN_RES_JID])
.

%%--------------------------------------------------------------------
%% @doc функция для создания сообщения(запуска функции) через указанное время 
%% @spec wrap_cron_once(Interval_sec, {M, F, A}) -> {ok,Tref}
%% @end
%%--------------------------------------------------------------------
wrap_cron_once(Interval_sec, {M, F, A}) ->
%% {ok,A} = timer:apply_after(3000, timer, send_after, [3000,self(),ok]), 
%% timer:cancel(A). 

    %%?INFO_MSG("Interval_sec : ~p~n", [Interval_sec*1000]),
    
timer:apply_after(Interval_sec*1000, M, F, A).
%%--------------------------------------------------------------------
%% @doc Отменяет задачу запускаемую по таймеру
%% @spec wrap_cron_cancel(A) -> {ok,canceled}
%% @end
%%--------------------------------------------------------------------
wrap_cron_cancel(A) ->
%%    ?INFO_MSG("wrap_cron_cancel   : ~p  ~n", [catch erlang:error(foobar)]),     
timer:cancel(A). 


floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).

ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.


alive(A) when is_pid(A) -> is_process_alive(A);
alive(_A) -> false.
