%%%-------------------------------------------------------------------
%%% File    : top_server.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Работа с top игроков.
%%%
%%% Created :  9 Feb 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(top_server).
-behaviour(gen_server).

-include("poligon.hrl").

%% API
-export([start_link/0,
        get_top100/1,
        adduser_online/2,
        getusers_online/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,  handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
redis_link %% линк редиса
}).


-define(SERVER, ?MODULE).
-define(INFO_MSG(Msg, Arg), io:format(Msg, Arg)).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: get_top100(Name) -> ...
%% Description: Получение топа 100 игроков.
%%--------------------------------------------------------------------
get_top100(User) ->
    gen_server:cast(?SERVER, {top100, User}).


%%--------------------------------------------------------------------
%% Function: adduser_online(User, Prefix) -> ...
%% Description: Добавление пользователя в список онлайн игроков
%%--------------------------------------------------------------------
adduser_online(User, Prefix) ->
    gen_server:cast(?SERVER, {adduser_online, User, Prefix}).


%%--------------------------------------------------------------------
%% Function: getusers_online(Prefix) -> ...
%% Description: Извлечение списка пользователей онлайн
%%--------------------------------------------------------------------
getusers_online(Prefix) ->
    gen_server:cast(?SERVER, {getusers_online, Prefix}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Any) ->
    C = plg_redis_wrapper:get_link_redis(?EREDIS_INF),
    State = #state{redis_link=C},
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

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({top100, User}, State) ->
    C = State#state.redis_link,
    ?INFO_MSG("Top100: ~p [~p]~n", [User, C]),
    {noreply, State};

handle_cast({adduser_online, User, Prefix}, State) ->
    Redis_link = State#state.redis_link,
    %%?INFO_MSG("redis_link: ~p~n", [C]),
    plg_redis_wrapper:q(Redis_link, ["SADD", list_to_binary([Prefix, <<"online_users">>]), User]),
    {noreply, State};

handle_cast({getusers_online, Prefix}, State) ->
    Redis_link = State#state.redis_link,
    %%?INFO_MSG("redis_link: ~p~n", [C]),
    Result_smember = plg_redis_wrapper:q(Redis_link, ["SMEMBERS", list_to_binary([Prefix, <<"online_users">>])]),
    {ok, Online_users} = Result_smember,
    ?INFO_MSG("online_users: ~p~n", [Online_users]),
    {noreply, State};

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
