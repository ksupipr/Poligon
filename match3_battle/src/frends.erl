%%%-------------------------------------------------------------------
%%% File    : frends.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Функции работы списком друзей игрока
%%%
%%% Created :  25 Apr 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(frends).

%% API списки
-export([get_frends/1, get_frends_online/1]).

%% API формирование
-export([make_frends/2, make_frends_online/2]).

%% API действия
-export([add_frend/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").


%%--------------------------------------------------------------------
%% Function: get_frends(Frend_list) -> [JSON]
%% Description: Возвращает список записей в формате JSON друзей пользователя отсортированных по очкам рейтинга
%%              вне зависимости от того онлайн пользователь или нет
%%-----------------------------------------------------------------------

get_frends(Frend_list) ->
    Frend_list_out = [FInfo || {_Score, FInfo} <- Frend_list],

    if 
        (Frend_list=/=[])  ->  libs_functions:p8_fun(Frend_list_out);
        true               ->  []
    end
.


%%--------------------------------------------------------------------
%% Function: get_frends(Frend_list, User) -> [JSON1, JSON2...]
%% Description: Возвращает список записей в формате JSON друзей пользователя отсортированных по очкам рейтинга
%%              только онлайн пользователи
%% Frend_list - [JSON1, JSON2] JSON - binary сформированная инфа о пользователе
%%-----------------------------------------------------------------------

get_frends_online(Frend_list) ->
    Frend_list_out = [FInfo || {_Score, FInfo} <- Frend_list],

    if 
        (Frend_list=/=[])  ->  libs_functions:p8_fun(Frend_list_out);
        true               ->  []
    end
.


%%--------------------------------------------------------------------
%% Function: make_frends(Frend_list) -> [JSON]
%% Description: Возвращает список записей в формате JSON друзей пользователя
%%              вне зависимости от того онлайн пользователь или нет
%% Frend_list - [Jid1, Jid2] Jid* - string
%%-----------------------------------------------------------------------

make_frends(Frend_list, _User) ->

    Redis_link = plg_redis_wrapper:get_link_redis(?EREDIS_INF),
    P8 = fun(A, AccIn) -> 
         A1 = get_user_info(Redis_link, A),

              case A1 of 
                         {Score, _Inf} when Score >=0  ->   [A1 | AccIn];
                         _ ->  AccIn
              end 
     end,

    Info_Friends_List = lists:foldl(P8, [] , Frend_list),

   


   

    sort_frend(Info_Friends_List)
.


%%--------------------------------------------------------------------
%% Function: make_frends(Frend_list, User) -> [JSON]
%% Description: Формирует список записей в формате JSON друзей пользователя
%%              только онлайн пользователи
%% Frend_list - [Jid1, Jid2] Jid* - string
%%-----------------------------------------------------------------------

make_frends_online(Frend_list, User) ->

    Online_friends = [Fr || Fr <- Frend_list, is_online(Fr)],

    List_fr = fun(Fro) ->
        FroStr = binary_to_list(Fro), 
        Pid4Jid = get_gproc_local_name(FroStr),
        Self    = get_gproc_local_name(User),
        gen_server:cast(Pid4Jid, {top_state_user_info, Self}),
        Fro
    end,

    _Friends_list = [List_fr(Fro) ||Fro <- Online_friends]
.




%%--------------------------------------------------------------------
%% Function: add_frend(Frend_list, Frend) -> [JSON]
%% Description: Возвращает список записей в формате JSON друзей пользователя отсортированных по очкам рейтинга
%%              с только что добавленным пользователем
%% Frend_list - [JSON1, JSON2] JSON - binary сформированная инфа о пользователе
%%-----------------------------------------------------------------------

add_frend(Frend_list, FrInfo) ->
    NewFList = lists:append(Frend_list, [FrInfo]),
    %NewFList = [ Frend_list | FrInfo ],
    sort_frend(NewFList)
.


%%--------------------------------------------------------------------
%% Function: sort_frend(Frend_list) -> [JSON]
%% Description: Возвращает список записей в формате JSON друзей пользователя отсортированных по очкам рейтинга
%% Frend_list - [JSON1, JSON2] JSON - binary сформированная инфа о пользователе
%%-----------------------------------------------------------------------

sort_frend(Frend_list) ->
    Sort_on = fun(A, B) ->
        {AS, _AR} = A,
        {BS, _BR} = B,
        if (AS > BS) -> false;
            true -> true
        end
    end,

    lists:sort(Sort_on, Frend_list)    
.

%%--------------------------------------------------------------------
%% Function: is_online(Jid) -> true | false
%% Description: проверяет, есть ли пользователь онлайн
%%              в реале, проверяет, запущен ли процесс 
%%-----------------------------------------------------------------------
is_online(Fr) ->
case (Fr) of
    B when is_binary(B) ->
        FrStr = binary_to_list(Fr), 
        Pid4Jid=get_gproc_local_name(FrStr),

        case (libs_functions:alive(Pid4Jid)) of
        false -> false;
            _ -> true
        end;
     _    -> false
end
.



%%--------------------------------------------------------------------
%% @doc обвязка вокруг поиска пида процесса зареганного в Gproc
%% @spec  get_gproc_local_name(FroStr) -> false | <p.i.d>
%% @end
%%-------------------------------------------------------------------------------------------

get_gproc_local_name(FroStr) ->
        case catch gproc:lookup_local_name(FroStr) of
            {What, Why} ->
                     ?INFO_MSG("get_gproc_local_name What: ~p~n Why: ~p~n", [What, Why]),
                    false;
            Val ->
                    Val
        end
.


get_user_info(C, Jid) ->
    {ok, UInfo} = plg_redis_wrapper:q(C, ["GET", list_to_binary([Jid, <<"info">>])]),
    case (UInfo) of
        undefined -> {-1, []};
        B when is_binary(B) -> binary_to_term(UInfo);
        _ -> {-1, []}
    end
.
