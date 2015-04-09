%%%-------------------------------------------------------------------
%%% File    : match3_lid.erl
%%% Author  : Marat Yusupov
%%% Description : Обвязка вокруг ядерного модуля Ерланг для ускорения вычислений с картой 8 x 8  для игры match3
%%% Copyright   : Marat Yusupov marat@yusupov.me
%%% Created :
%%%-------------------------------------------------------------------
-module(snappy).

-include("ejabberd.hrl").
-include("jlib.hrl").
-export([free/1, get_map/1,
         change/5,boom/3,line/2,
         next_user/1,no_change/1,
         new/6, change_health/3, random_cell/1
	 ,end_timeout/1
         ,init/0]).

-on_load(init/0).


init() ->
    SoName = case code:priv_dir(?MODULE) of
    {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
            filename:join(["..", "priv", "snappy_nif"]);
        false ->
            %%filename:join(["priv", "snappy_nif"])
            filename:join(filename:absname("/usr/lib/ejabberd/priv/lib"), "snappy_nif")
        end;
    Dir ->
        filename:join(Dir, "snappy_nif")
    end,

%%    ?INFO_MSG("init snappy: ~p~n", [SoName]),
    (catch erlang:load_nif(SoName, 0)).


%%   {"new", 6, all_query },
new(_IoList, _Arg0,_Arg1,_Arg2,_Arg3,_Arg4) ->
    exit(snappy_nif_not_loaded).
%% {"next_user", 1,next_user }, // переход хода
next_user(_IoList) ->    exit(snappy_nif_not_loaded).
  %% {"free", 1, unlock},
 free(_IoList) -> exit(snappy_nif_not_loaded).
  %%{"get_map", 1,get_map },
 get_map(_IoList) -> exit(snappy_nif_not_loaded).
  %%{"change", 5, all_query},
 change(_Arg0,_Arg1,_Arg2,_Arg3,_Arg4) -> exit(snappy_nif_not_loaded).
  %%{"boom", 3, all_query},
 boom(_Arg0,_Arg1,_Arg2) -> exit(snappy_nif_not_loaded).
  %%{"line", 2, all_query}
 line(_Arg0,_Arg1) -> exit(snappy_nif_not_loaded).
 no_change(_IoList) ->    exit(snappy_nif_not_loaded).
%%    {"change_health", 3, change_health}, // добавление и снятие "здовровья"
 change_health(_Id,_Type,_Dp) ->    exit(snappy_nif_not_loaded).
%%    {"random_cell", 1, random_cell},    // 12 ячеек
 random_cell(_Id)  ->    exit(snappy_nif_not_loaded).

 end_timeout(_id)  ->    exit(snappy_nif_not_loaded).
