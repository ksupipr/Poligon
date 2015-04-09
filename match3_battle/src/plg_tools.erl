%%%-------------------------------------------------------------------
%%% @author Marat Yusupov <marat@yusupov.me> and Михаил Богатырев <ksupipr@yandex.ru>
%%% @doc
%%% Библиотека  для обслуживания игрового сервера Полигон
%%% @end
%%% Created :  4 Jul 2012 by Marat Yusupov <unlexx@gmail.com>
%%%-------------------------------------------------------------------
-module(plg_tools).

%% API
-export([add_all_money/1,bin_to_num/1, add_medal_all/2, add_medal/2]).


-include("poligon.hrl").
-include("achiv.hrl").
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add_all_money(Money) ->
add_all_money(Money,  "vk_*xiff").

add_all_money(Money, Jid_pattern) ->

%% получить список игроков 
Link = plg_redis_wrapper:get_link_redis(?EREDIS_PROFILE_SERV),

{ok, AA} = plg_redis_wrapper:q(Link, ["KEYS", Jid_pattern]),

%% рекурсивно определяет PID профиля и накидывает 
Profile_is_live = fun(AR, _F_self) when is_pid(AR) -> {AR,is_process_alive(AR)}; 
                     (D,  F_self) when is_list(D) ->
     		       Pid=gproc:lookup_local_name(D),  
			 F_self(Pid,F_self) ;
		     (_,_) -> {false,false} 
		  end,

%% анонимная функция для добавления монет игроку

SendMoney = fun(A, Count) ->
    {Jid,Name}  =     case (A) of
    W when is_binary(W) -> {jlib:string_to_jid(binary_to_list(W)),binary_to_list(W)};
    M when is_list(M) -> {jlib:string_to_jid(M),M};
               _ -> {[],[]}
	      end, %% case

	   case {Jid,Name} of
	       {Jid,Name} when is_tuple(Jid) ->
		   %% проверить живой ли чел 
		   case Profile_is_live(Name,Profile_is_live) of
		       {Pid,true} ->  gen_server:cast(Pid, {add_money, Money, 0}),Count+1 ;
		       _ ->  %% чел оффлайн
			   Keyg = fun(Akeyg) -> list_to_binary([Name,"_T_",integer_to_list(Akeyg) ]) end,
			   plg_redis_wrapper:q(Link, ["INCRBY",  Keyg(1),integer_to_list(Money)])
			   ,Count+1

		   end;
		   _ -> Count   
	end %% case
		    
end,

 FF = lists:foldl(SendMoney,  0 , AA),

{ok,FF}.


%%--------------------------------------------------------------------
%% @doc Инструмент раздачи медалей 
%% @spec
%% @end
%%--------------------------------------------------------------------

%% получить список игроков 
add_medal_all(Medal_id, Jid_pattern) ->
    Link = plg_redis_wrapper:get_link_redis(?EREDIS_PROFILE_SERV),
    {ok, Users_list} = plg_redis_wrapper:q(Link, ["KEYS", Jid_pattern]),
add_medal(Medal_id, Users_list)
.

%% раздача медалей списку пользователей 
add_medal(Medal_id, Users_list) ->
Link = plg_redis_wrapper:get_link_redis(?EREDIS_PROFILE_SERV),

%% рекурсивно определяет PID профиля и накидывает 
Profile_is_live = fun(AR, _F_self) when is_pid(AR) -> {AR,is_process_alive(AR)}; 
                     (D,  F_self) when is_list(D) ->
                   Pid=gproc:lookup_local_name(D),  
             F_self(Pid,F_self) ;
             (_,_) -> {false,false} 
          end,

Make_medal_list = fun(AM, MAccin) ->
    case (AM) of
        [] -> MAccin;
        _ -> lists:flatten([MAccin, [{AM,calendar:datetime_to_gregorian_seconds(erlang:localtime()),1}]])
    end

end, 

Medals_list = lists:foldl(Make_medal_list,  [] , Medal_id),

%% анонимная функция для добавления монет игроку

SendMedal = fun(A, Count) ->
    {Jid,Name}  =     case (A) of
    W when is_binary(W) -> {jlib:string_to_jid(binary_to_list(W)),binary_to_list(W)};
    M when is_list(M) -> {jlib:string_to_jid(M),M};
               _ -> {[],[]}
          end, %% case

       case {Jid,Name} of
           {Jid,Name} when is_tuple(Jid) ->
           %% проверить живой ли чел 
           case Profile_is_live(Name,Profile_is_live) of
               {Pid,true} ->  
                                NewUserParams = #userparams {
                                      jid =   Jid   %% идентификатор пользователя
                                    , statistics  = []   %% статистика 
                                    , money_0 = 0        %% монеты
                                    , new_statistics = [] %% список изменения статистики [{id, +-add_val} ]
                                    , new_bonuses = []    %% список полученых бонусов     [{id, add_val}]
                                    , new_achive  = Medals_list
                                    , dell_achive  = []    %% список удаляемых достижений [{id, date, val}]
                                },
                                gen_server:cast(Pid, {set_achiv, NewUserParams}),
                                Count+1 ;
               _ ->  %% чел оффлайн
                               {ok, B} = plg_redis_wrapper:q(Link, ["GET", Name]),

                               case (B) of
                                    BM when is_binary(BM) ->
                                                    {P00,P01,P02,P03,P04,P05,P06,P07,P08,P09,P10,P11,P12
                                                    ,P13,P14   ,P140,0,0,0,0,0,0,0,0,0} =   erlang:binary_to_term(B),

                                                    P07_new = achiv:add_achiv(P07,Medals_list),

                                                    SaveState0 =  erlang:term_to_binary({P00,P01,P02,P03,P04,P05,P06,
                                                            P07_new,P08,P09,P10,P11,P12,P13,P14,P140,0,0,0,0,0,0,0,0,0}, [compressed, {minor_version,1}]),
                                                    plg_redis_wrapper:q(Link, ["SET",  Name, SaveState0])
                                                    ,Count+1;
                                    _ -> Count
                                end
           end;
           _ -> Count   
    end %% case
            
end,

 FF = lists:foldl(SendMedal,  0 , Users_list),

{ok,FF}.

%%--------------------------------------------------------------------
%% @doc бинарную строку <<"100">> в число 100
%% @spec bin_to_num(Bin) -> integer()
%% @end
%%--------------------------------------------------------------------
bin_to_num(undefined) -> 0;
bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F 
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Функция определяет людей которые давно не заходили в игру и сохраняет их в БД
%% @spec
%% @end
%%--------------------------------------------------------------------
%% TODO дописать (%)
