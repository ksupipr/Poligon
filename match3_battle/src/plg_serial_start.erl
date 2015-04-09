%%%-------------------------------------------------------------------
%%% @author Marat Yusupov <unlexx@gmail.com>
%%% @copyright (C) 2012, Marat Yusupov
%%% @doc
%%% предназначен для последовательного старта функций и гарантирует 
%%% что не будет эффекта гонок при конкурентных запросах к 
%%% процессам которые зарегистрированы в gproc
%%% @end
%%% Created : 22 Aug 2012 by Marat Yusupov <unlexx@gmail.com>
%%%-------------------------------------------------------------------
-module(plg_serial_start).

-behaviour(gen_server).

%% API
-export([start_process/5]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

-include("ejabberd.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc передает данные необходимые для проверки что процесс зареган в gproc 
%%      в процесс что гарантирует последовательный старт 
%% @spec
%% @end
%%--------------------------------------------------------------------
start_process(Name, Mod,Fun,Arg,Q) ->
    Pid=gproc:lookup_local_name("plg_serial_start"),
Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
P =case (Profile_is_live(Pid)) of
        true  -> Pid;
        _ ->
        {ok,Pid_new}=plg_serial_start:start_link(),
        Pid_new
  end,
gen_server:cast(P, {start,Name, Mod,Fun,Arg,Q}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
gproc:add_local_name("plg_serial_start"),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({start,Name, Mod,Fun,Arg,Query}, State) ->
%% проверяем не стартавал ли кто либо процесс игрока
Pid4pro=gproc:lookup_local_name(Name),
   Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
	   
case (Profile_is_live(Pid4pro)) of
        true  ->
        gen_server:cast(Pid4pro, Query);
        U ->
        %%запустить нужный процесс
	?INFO_MSG("init done : ~p ~p~n", [Name,U]),
        {ok,Pid}=Mod:Fun(Arg),
        gen_server:cast(Pid, Query)

    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
