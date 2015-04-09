%%%-------------------------------------------------------------------
%%% @author Marat Yusupov <unlexx@gmail.com>
%%% @copyright (C) 2012, Marat Yusupov
%%% @doc
%%%  Супервизор Ядра игры полигон
%%% @end
%%% Created :  5 Jun 2012 by Marat Yusupov <unlexx@gmail.com>
%%%-------------------------------------------------------------------
-module(plg_root_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


-include("ejabberd.hrl").
-include("poligon.hrl").


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
%% ?INFO_MSG("sup root start: ~p~n", [1]),
{ok, Pid} =  supervisor:start_link({local, ?SERVER}, ?MODULE, []),
start_childs(Pid).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
%%  one_for_one, 
%% ?INFO_MSG("sup init root start: ~p~n", [1]),
    RestartStrategy =  one_for_all , %% если перезапускаем то всех
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc запуск всех детей
%% @spec  start_childs(Pid) -> {ok, Pid}.
%% @end
%%--------------------------------------------------------------------

start_childs(Pid) ->
  Restart = permanent,
    Shutdown = 2000,
    Type = worker,
%% eredis
   AChild1 = {'plg_redis_wrapper_pserv', {'plg_redis_wrapper', start_link, []}, Restart, Shutdown, Type, ['plg_redis_wrapper']},
%%?INFO_MSG("  start plg_redis_wrapper: ~p~n", [supervisor:start_child(Pid, AChild1)]),
%% p_sender p_sender:start_link(),
%% возможно стоит сделать пуул сендеров и тут стартовать супервизор пула
   AChild2 = {'p_sender', {'p_sender', start_link, []}, Restart, Shutdown, Type, ['p_sender']},
%% rbattle {ok, Pid } = match_rbattle:start_link(),
   AChild3 = {'match_rbattle', {'match_rbattle', start_link, []}, Restart, Shutdown, Type, ['match_rbattle']},
%% систему турнира
   AChild4 = {'plg_go_turnir', {'plg_go_turnir', start_link, []}, Restart, Shutdown, Type, ['plg_go_turnir']},
%% сервер битвы
   AChild5 = {'match_battle', {'match_battle', start_link, []}, Restart, Shutdown, Type, ['match_battle']},
%% top_online_server
   AChild6 = {'top_online_server', {'top_online_server', start_link, []}, Restart, Shutdown, Type, ['top_online_server']},
%% аватары эксклюзив описание
   AChild7 = {'ava_info', {'ava_info', start_link, []}, Restart, Shutdown, Type, ['ava_info']},
%% вещи
   AChild8 = {'things', {'things', start_link, []}, Restart, Shutdown, Type, ['things']},
%% магазин
   AChild9 = {'shop', {'shop', start_link, []}, Restart, Shutdown, Type, ['shop']},
%% вещи описание
   AChild10 = {'things_info', {'things_info', start_link, []}, Restart, Shutdown, Type, ['things_info']},
%% достижения описание
   AChild11 = {'achiv_info', {'achiv_info', start_link, []}, Restart, Shutdown, Type, ['achiv_info']},
%% достижения
   AChild12 = {'achiv', {'achiv', start_link, []}, Restart, Shutdown, Type, ['achiv']},
%% топ офлайн
   AChild13 = {'top_offline', {'top_offline', start_link, []}, Restart, Shutdown, Type, ['top_offline']},
%  советы
   AChild14 = {'sovets', {'sovets', start_link, []}, Restart, Shutdown, Type, ['sovets']},
%  логи
   AChild15 = {'buy_log', {'buy_log', start_link, []}, Restart, Shutdown, Type, ['buy_log']},
%  платежи
   AChild16 = {'paymants', {'paymants', start_link, []}, Restart, Shutdown, Type, ['paymants']},
%% p_route p_router:start_link(Link)
   AChild17 = {'plg_route_sup', {'plg_route_sup', start_link, []}, Restart, infinity, supervisor, ['plg_route_sup']},

AChild = [AChild1,AChild2,AChild3,AChild4,AChild5,AChild6,AChild7,AChild8,AChild9,AChild10,
          AChild11,AChild12,AChild13,AChild14, AChild15, AChild16, AChild17],
DD = [ supervisor:start_child(Pid, X) || X <- AChild],
 ?INFO_MSG(" result start  child: ~p~n", [DD]),
{ok, Pid}.    
