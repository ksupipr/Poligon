%%%-------------------------------------------------------------------
%%% File    :
%%% Author  : Marat Yusupov
%%% Description : Плагие к ejabberd  который обеспечивает связку джаббер шины с остальными подсистемами ИГры
%%% Copyright   : Marat Yusupov marat@yusupov.me
%%% Created :
%%%-------------------------------------------------------------------
-module(mod_polig).
-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).





-include("ejabberd.hrl").
-include("jlib.hrl").




-record(state, {host,done_pool=[],new_pool=[]}).

-define(PROCNAME, ejabberd_mod_polig).
-define(MYDEBUG(Format, Args),io:format("D(~p:~p:~p) : " ++ Format ++ "~n",[calendar:local_time(), ?MODULE, ?LINE] ++ Args)).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%
%% gen_server:start_link(...)
%% Первый аргумент – {local,?SERVER} – определяет имя сервера. В нашем случае сервер будет локально
%% зарегистрирован под именем ch3. Если имя будет опущено, то gen_server не будет зарегистрирован.
%% Вместо имени будет использоваться идентификатор процесса. В качестве имени также может быть
%% передан кортеж {global, Name}, при этом сервер будет зарегистрирован с помощью функции global:register_name/2
%%
%% Второй аргумент – ?MODULE  – имя callback-модуля, в котором расположены callback-функции, реализованные нами.
%% В нашем случае интерфейсные функции (start_link, alloc и free) и callback- функции (init, handle_call и handle_cast)
%% расположены в одном модуле. Это достаточно обычная ситуация, поскольку удобно размещать код, связанный с процессом,
%% в одном модуле.
%%
%% Третий аргумент – [Host, Opts] – терм, который передаётся в качестве параметра callback-функции init.
%% В нашем случае init  хост и опции из конфига
%%
%% Четвёртый аргумент – [] – список опций. Список доступных опций приводится в описании модуля gen_server
%%
%% Для синхронизации процедуры запуска сервера, функция start_link/3,4 не возвращает управление до тех пор,
%% пока не завершится выполнение функции Module:init/1.
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

%% я так понимаю запускаем себя как часть дерева супервизоров
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
 
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
%% запуск нужных приложений для корректной роботоспособности ИГры
gproc:start_link(),
%% inets для httpc запросов к апи
inets:start(),
%%  стартуем крон
ecrn_app:manual_start(),
%% запускаем супервизор  ядра
%% запускаем как часть дерева контроля 
%% то есть регистрируем супервизоря ядра как ребенка ejabberd_sup
 ?INFO_MSG(" start(): ~p~n", [Host]),
    Kern_Polig_Spec =
        {'plg_root_sup',
         {plg_root_sup, start_link, []},
         Restart,
	 infinity,  supervisor,
         ['plg_root_sup']},
   supervisor:start_child(ejabberd_sup, Kern_Polig_Spec),
%% теперь запускаем модуль ejabberd то есть элемент полигона
% ?INFO_MSG(" start(2): ~p~n", [MM]),
    ChildSpec =
        {Proc,
         {?MODULE, start_link, [Host, Opts]},
          Restart,
         Shutdown,
         Type,
         [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).


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
init([Host, Opts]) ->
%% ?INFO_MSG("init start: ~p~n", [Host]),
    MyHost = gen_mod:get_opt_host(Host, Opts, "echo.@HOST@"),
    ejabberd_router:register_route(MyHost),
?INFO_MSG("mod init done: ~p~n", [MyHost]),
{ok, #state{host=MyHost}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: send_to_pool(Msg) -> {empty} |
%%                                 {ok}|
%%
%% Description: это функции рассылки сообщений по пулу точнее в экземпляр пула
%%--------------------------------------------------------------------
send_to_pool(To, From, Packet2) ->
    Name = pool_router,
    Any_pool = pg2:get_closest_pid(Name),    
gen_server:cast(Any_pool, {query_from_client,To, From, Packet2}),
{ok}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%
%% Основная рабочая часть:
%%
%%--------------------------------------------------------------------

handle_info({route, From, To, Packet}, State) ->
            Packet2 = case From#jid.user of
                    "" -> jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST);
                    _ -> Packet
            end,

 
        send_to_pool(To, From, Packet2),

  {noreply, State};

handle_info(_Info, State) ->
        {noreply, State}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) ->
?INFO_MSG("init done: ~p ~p~n", [Reason,State#state.host]),
    ejabberd_router:unregister_route(State#state.host),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





