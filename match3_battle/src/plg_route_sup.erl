%%%-------------------------------------------------------------------
%%% @author Marat Yusupov <unlexx@gmail.com>
%%% @copyright (C) 2012, Marat Yusupov
%%% @doc
%%% Супервизор пула процессов принимающие сообщения и маршрутизирующие сообщения в нужный процесс
%%% @end
%%% Created :  5 Jun 2012 by Marat Yusupov <unlexx@gmail.com>
%%%-------------------------------------------------------------------
-module(plg_route_sup).

-behaviour(supervisor).

-define(MAX_ROUTER_POOL,5).
-include("ejabberd.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

      %% запускаем  пул
 [supervisor:start_child(Pid, {{'p_router',X}, {'p_router', start_link, []},
	      Restart, Shutdown, Type, ['p_router']}) || X <- lists:seq(1,?MAX_ROUTER_POOL)],
%%?INFO_MSG("p_router starn_link: ~p~n", [FF]),
{ok, Pid}
.

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
{ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
