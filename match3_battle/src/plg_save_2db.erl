%%%-------------------------------------------------------------------
%%% @author Marat Yusupov <unlexx@gmail.com>
%%% @copyright (C) 2012, Marat Yusupov
%%% @doc
%%% функции которые сохраняет/восстанавливает профиль игрока в postgresql 
%%% @end
%%% Created : 27 Jun 2012 by Marat Yusupov <unlexx@gmail.com>
%%%-------------------------------------------------------------------
-module(plg_save_2db).

-include("shared.hrl").
-include("poligon.hrl").

%% API
-export([save_profile_2db/1, restore_profile_from_db/1]).
-export([clear_redis/0]).

-record( state,   ?PLG_USER_PROFILE_REC  ).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Функция анализа и помещения профилей игрока в БД postgresql которые давно не играли
%% @spec clear_redis() -> ok
%% @end
%%--------------------------------------------------------------------
clear_redis() ->
%% для ясности возмем срок 1 месяц кто дольше этого времени тот редиска
%% и его в базу
 Link = plg_redis_wrapper:get_link_redis(?EREDIS_PROFILE_SERV),
 {ok, AA} = plg_redis_wrapper:q(Link, ["zrange",  "all_init_users",0, 100,"WITHSCORES"]),
    Res = need_save(AA),
    
case {Res,AA} of
    {get_me,[]} -> ok;
    {get_me,_} -> clear_redis();
    _ -> ok
    end.


%%--------------------------------------------------------------------
%% @doc функция сохранения профиля в БД
%% @spec save_profile_2db(#state{}) -> ok
%% @end
%%--------------------------------------------------------------------
save_profile_2db(_F = #state {   jid        = P00
			       , jid_bin    = P01
			       , firstName  = P02
			       , lastName   = P03
			       , middleName = P04
			       , sn_id      = P05
			       , reg_time   = P06
			       , achiv      = P07
			       , msg_send   = P08
			       , user_param = P09
			       , is_admin   = P10
			       , last_init_time = P11
			       , last_enter_day = P12
			       , everyday_count = P13
			       , profile_link   = P14
			       , user_data      = P140
			       , tran_number    = P15
			       , statistics     = P16   
 			       , buy_kredits    = P17
                               , thing_change_param = C01} ) ->

SaveState0 =  erlang:term_to_binary({P00,P01,P02,P03,P04,P05,P06,P07, P08,P09,P10,P11,P12
             ,P13,P14   ,P140,0,0,0,0,0,0,0,0,0}, [compressed, {minor_version,1}]),
SaveState1 =  P15, %% явно сохраняем как число  НОМЕР ТРАНЗАКЦИИ
SaveState2 = erlang:term_to_binary({P16,P17,0,0,0,0,0,0,0,0,0}, [compressed, {minor_version,1}]),

#thing_change_param_rec{
		  money_1 = T0       %% кредиты
		, money_0 = T1     %% монеток
		, ava =     T2     %% аватара
		, premium = T3     %% флаг премиум режима {Режим, Timestamp}  дата в unix timestamp
		, slots   = T4     %% array слотов
 } =C01,
    
ST0 = T0,
ST1 = T1,
ST2 = T2,
ST3 = erlang:term_to_binary(T3, [compressed, {minor_version,1}]),
 Get_all_things = fun( _, _Value= #slot{num = I, count=Count }, AccIn) -> 
                 lists:append(AccIn,[{I,I,Count}]) %% ID совпадает с номером слота
	 end,
ST4 = erlang:term_to_binary(array:foldr(Get_all_things,[],T4), [compressed, {minor_version,1}]),
%% 192.168.1.1:5432

    {ok, C} = pgsql:connect(?DB_HOST, [?DB_USERNAME], [?DB_PASS], [?DB_PLG_OPTIONS]),
    
Empty_list=[],
    Tuple = jlib:string_to_jid(P00),
    {Buy_date,Buy_count}= case P17 of
	{L,L1}->{L,L1};
	_ ->{0,0}
		end,
    

    Win_turn    = array:get(8, P16#statistics.stat_always), %% 68 
    Lose_turn   = array:get(9, P16#statistics.stat_always), %% 69 
    Win_trener  = array:get(0, P16#statistics.stat_always), %% 60 
    Lose_trener = array:get(1, P16#statistics.stat_always), %% 61 
    Win_duel    = array:get(3, P16#statistics.stat_always), %% 63 
    Lose_duel   = array:get(4, P16#statistics.stat_always), %% 64 
    Win_vs      = array:get(5, P16#statistics.stat_always), %% 65 
    Lose_vs     = array:get(6, P16#statistics.stat_always), %% 66 
 Get_count_all_things = fun( _, _Value= #slot{num = _I, count=Count }, AccIn) -> 
                 AccIn+ Count
	 end,
    Bonus_count = array:foldr( Get_count_all_things,0,T4),
% 97- количество использованных бонусов слот0
% 98- количество использованных бонусов слот1
% 99- количество использованных бонусов слот2
% 101- количество использованных бонусов слот3
% 102- количество использованных бонусов слот4
% 103- количество использованных бонусов слот5
% 104- количество использованных бонусов слот6
% 105- количество использованных бонусов слот7
% 106- количество использованных бонусов слот8
 Sum_stat = fun( Id, Sum) -> 
		    Sum + array:get(Id-60, P16#statistics.stat_always)
	    end,

    Bonus_apply = lists:foldl(Sum_stat,0, [97,98,99,101,102,103,104,105,106]) ,
    
Save_list = [SaveState0,SaveState1,SaveState2,ST0,ST1,ST2,ST3,ST4,element(2, Tuple),calendar:gregorian_seconds_to_datetime(P06),calendar:gregorian_seconds_to_datetime(P12),Buy_count,calendar:gregorian_seconds_to_datetime(Buy_date),
 P13,Win_turn,Lose_turn,Win_trener,Lose_trener,Win_duel,Lose_duel,Win_vs,Lose_vs,Bonus_count,Bonus_apply |  Empty_list ],
{ok, 1} = pgsql:equery(C, "insert into users(P1,P2,P3,in_val,money_0,T2,T3,T4,username,reg_time,last_enter_day,buy_count,
    last_buy_date,   everyday_count, win_turn,lose_turn,win_trener,lose_trener,win_duel,lose_duel,win_vs,lose_vs,bonus_count,bonus_apply)        
   values ($1,$2,$3,    $4,     $5,$6,$7,$8,      $9, $10,    $11,             $12,      $13,         $14,      $15,        $16        ,$17    , $18       ,$19,     $20,    $21 ,$22,$23,$24  );", Save_list),
    
 pgsql:close(C).



%%--------------------------------------------------------------------
%% @doc Функция восстановления профиля из БД
%% @spec restore_profile_from_db(List) -> #state{}
%% @end
%%--------------------------------------------------------------------
restore_profile_from_db(Jid) ->
{ok, C} = pgsql:connect(?DB_HOST, [?DB_USERNAME], [?DB_PASS], [?DB_PLG_OPTIONS]),
    Tuple = jlib:string_to_jid(Jid),
Login = element(2, Tuple),
{ok, _Columns, Rows} = pgsql:equery(C, "select P1,P2,P3,in_val,money_0,T2,T3,T4 from users where username=$1;", [Login]),

case Rows of
	D when is_list(D)  -> 
	[{SaveState0,P15,SaveState2,T0,T1,T2,ST3,ST4}]=Rows,



	{P00,P01,P02,P03,P04,P05,P06,P07,P08,_09,_P10,P11,P12
	 ,P13,P14   ,P140,0,0,0,0,0,0,0,0,0} =   erlang:binary_to_term(SaveState0),
	%% SaveState1 =  P15, %% явно сохраняем как число  НОМЕР ТРАНЗАКЦИИ
	{P16,P17,0,0,0,0,0,0,0,0,0} =  erlang:binary_to_term(SaveState2),
	%% премиум
	T3 = erlang:binary_to_term(ST3),
	%% вещи
	Thr =erlang:binary_to_term(ST4),
	T4 = libs_functions:get_base_slots(Thr), 
	C01 = #thing_change_param_rec{
           	money_1 = T0       %% кредиты
		, money_0 = T1       %% монеток
		, ava =     T2 %% аватара
		, premium = T3          %% флаг премиум режима {Режим, Timestamp}  дата в unix timestamp
		, slots   = T4            %% array слотов
	       },
        {ok,1} = pgsql:equery(C, "delete from users where username=$1;", [Login]),
	pgsql:close(C),
	#state{ jid        = P00
	    , jid_bin    = P01
	    , firstName  = P02
	    , lastName   = P03
	    , middleName = P04
	    , sn_id      = P05
	    , reg_time   = P06
	    , achiv      = P07
	    , msg_send   = P08
	    , user_param = #perk{}
	    , is_admin   = libs_functions:sn_id_is_admin(Login)
	    , last_init_time = P11
	    , last_enter_day = P12
	    , everyday_count = P13
	    , profile_link   = P14
	    , user_data      = P140
	    , tran_number    = P15
	    , statistics     = P16   
	    , buy_kredits    = P17
	    , thing_change_param = C01} ;

	_ -> pgsql:close(C), #state{}
		end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc  функция анализирует list jidов игроков с их датой входа и сохраняет в БД
%% @spec
%% @end
%%--------------------------------------------------------------------
need_save([User0,Timestamp0|Tail]) ->
%%  <<"vk_73219541@im.xlab.su/xiff">>, <<"63507956669">>,    
 User      = binary_to_list(User0),
 Timestamp = calendar:gregorian_seconds_to_datetime(plg_tools:bin_to_num(Timestamp0)),
 Current   = calendar:local_time(), %% {{2012,4,16},{9,44,33}}
 {Days , _Time0}   = calendar:time_difference(Timestamp,Current),  
    case Days > 31 of
        true -> %% ZREM key member 
            Link = plg_redis_wrapper:get_link_redis(?EREDIS_PROFILE_SERV),
            {ok, AA} = plg_redis_wrapper:q(Link, ["GET", User]),
            case AA of
                <<"saved_in_db">> -> plg_redis_wrapper:q(Link, ["ZREM", "all_init_users" , User]), need_save(Tail) ;
                B when is_binary(B) ->
                    %% TODO надо не сохранять тех кто не отыграл ни одной битвы

                    User_state=match_profile:recovery_user_state(Link,B), 
                    plg_redis_wrapper:q(Link, ["ZREM", "all_init_users" , User]),
                    save_profile_2db(User_state),
                    plg_redis_wrapper:q(Link, ["SET", User ,  saved_in_db]), %% 2) "vk_9653723@im.xlab.su/xiff"
                    Keygen = fun(A) -> list_to_binary([User,"_P_",integer_to_list(A) ]) end,
                    plg_redis_wrapper:q(Link, ["DEL", Keygen(1), Keygen(2)]),
                    %% 3) "vk_9653723@im.xlab.su/xiff_P_1" 4) "vk_9653723@im.xlab.su/xiff_P_2"
                    Keyge = fun(A) -> list_to_binary([User,"_T_",integer_to_list(A) ]) end,
                    Del_key = [ Keyge(I) || I <- lists:seq(0, 4)],

                    plg_redis_wrapper:q(Link, ["DEL" | Del_key]),
                    %% "vk_9653723@im.xlab.su/xiff_T_0" ... 4
                    plg_redis_wrapper:q(Link, ["DEL", list_to_binary([User,"info" ])]),
                    %%1) "vk_9653723@im.xlab.su/xiffinfo"
                    %% plg_redis_wrapper:stop(Link),
                    need_save(Tail);
                _ -> plg_redis_wrapper:q(Link, ["ZREM", "all_init_users" , User]), need_save(Tail)
            end;
        _ -> ok
    end;
need_save([]) ->
get_me.

    
%% zrange all_init_users  0 10 WITHSCORES
%% ZREM key member 
