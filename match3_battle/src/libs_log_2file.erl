%%%-------------------------------------------------------------------
%%% @author Marat Yusupov <unlexx@gmail.com>
%%% @copyright (C) 2012, Marat Yusupov
%%% @doc
%%% Процесс который рождается и ведет логи в определенный файл
%%% создан для определния проблемы в Турнире
%%% но может использоватся и для других задач
%%% @end
%%% Created :  3 Aug 2012 by Marat Yusupov <unlexx@gmail.com>
%%%-------------------------------------------------------------------
-module(libs_log_2file).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
file_patch=0
, pid_for_control
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Pid,Patch) ->
    gen_server:start_link( ?MODULE, [Pid,Patch], []).

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
init(Arg)  ->
%% открыть файл для записи
[Pid,Patch] =Arg,
{{_Year,_Month,Day}, {Hour,Minutes,_Seconds}} = erlang:localtime(),         %%{{1970,1,1},{0,0,0}}
 Datetime = lists:flatten(["_",add_zero(Day), "_",
        add_zero(Hour), "_", add_zero(Minutes),".txt"]),
Patch0 =Patch ++ Datetime,
{ok, #state{
file_patch=Patch0
, pid_for_control =Pid
},60*1000*11}.

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
handle_call({write,Data}, _From, State=#state{file_patch=Patch0}) ->
{{_Year,_Month,_Day}, {Hour,Minutes,Seconds}} = erlang:localtime(),         %%{{1970,1,1},{0,0,0}}
 Datetime = lists:flatten([  add_zero(Hour), ":", add_zero(Minutes),":",add_zero(Seconds)," "]),
    Reply = ok,
    file:write_file( Patch0, io_lib:fwrite("[~p] ~p.\n", [ Datetime,Data]),[append]),

    {reply, Reply, State,14*60*1000};

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
handle_cast(_Msg, State) ->
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
handle_info(timeout, State=#state{file_patch=Patch0, pid_for_control =Pid}) ->
{{_Year,_Month,_Day}, {Hour,Minutes,Seconds}} = erlang:localtime(),         %%{{1970,1,1},{0,0,0}}
 Datetime = lists:flatten([  add_zero(Hour), ":", add_zero(Minutes),":",add_zero(Seconds)]),
%% запрос на наблюдаемый процесс
%% живой процесс?
Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,
    case Profile_is_live(Pid) of
	true ->
	    Info = gen_server:call(Pid, dump_state), 
	    file:write_file( Patch0, io_lib:fwrite("[~p] ~p.\n\n", [ Datetime,Info]),[append]),
	    {noreply, State,14*60*1000};
	_ ->  	    file:write_file( Patch0, io_lib:fwrite("[~p] done \n", [ Datetime]),[append]),
		    {stop, State}
end;

handle_info(Info, State=#state{file_patch=Patch0}) ->
{{_Year,_Month,_Day}, {Hour,Minutes,Seconds}} = erlang:localtime(),         %%{{1970,1,1},{0,0,0}}
 Datetime = lists:flatten([  add_zero(Hour), ":", add_zero(Minutes),":",add_zero(Seconds)]),
 file:write_file( Patch0, io_lib:fwrite("[~p] ~p.\n", [ Datetime,Info]),[append]),


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
