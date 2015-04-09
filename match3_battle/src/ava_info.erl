%%%-------------------------------------------------------------------
%%% File    : ava_info.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Эксклюзивные аватары
%%%
%%% Created :  10 Jul 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(ava_info).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").
-include("achiv.hrl").
-include("shared.hrl").

%% API
-export([start_link/0, show_info/2, get_ava_state/0, get_buyed_ava/0, add_buyed_ava/1, get_ava_list/0, get_ava_by_id/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
%%  redis_link %% линк редиса
  ava_list = []
, buyed_ava = []
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    Sl = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ?INFO_MSG("ava_info start: ~p; ", [Sl]),
    Sl
.

%%====================================================================
%% gen_server callbacks
%%====================================================================

show_info(Achiv_id, Type) ->
        gen_server:call(?SERVER, {show_info, Achiv_id, Type}).

get_ava_list() ->
        gen_server:call(?SERVER, {get_ava_list}).

get_buyed_ava() ->
        gen_server:call(?SERVER, {get_buyed_ava}).

get_ava_by_id(A_id) ->
        gen_server:call(?SERVER, {get_ava_by_id, A_id}).


add_buyed_ava(A_id) ->
        gen_server:call(?SERVER, {add_buyed_ava, A_id}).

get_ava_state() ->
    gen_server:cast(?SERVER, {get_ava_state}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Any) ->

    Name = <<"ava_info">>,
    gproc:add_local_name(Name),

    State = #state{},
    State2 = get_ava_state(State),
    {ok, State2}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({show_info, Achiv_id, Type}, _From, State) ->
    Result = show_info(Achiv_id, Type, State),
%?INFO_MSG("achiv_info end ~p ~n; ", [Result]),
{reply, Result, State};


handle_call({get_ava_list}, _From, State) ->
    Result = get_ava_list(State),
{reply, Result, State};

handle_call({get_buyed_ava}, _From, State) ->
    Result = get_buyed_ava(State),
{reply, Result, State};


handle_call({add_buyed_ava, A_id}, _From, State) ->
    Result = add_buyed_ava(A_id, State),
    if (Result =/= State) -> ResOut = 1;
            true -> ResOut = 0
    end,
{reply, ResOut, Result};


handle_call({get_ava_by_id, A_id}, _From, State) ->
    Result = get_ava_by_id(A_id, State),
{reply, Result, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({show_info, Achiv_id, Type}, State) ->
    show_info(Achiv_id, Type, State),
{noreply, State};

handle_cast({get_ava_state}, State) ->
    NewState = get_ava_state(State),
{noreply, NewState};

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

%%--------------------------------------------------------------------
%% @doc Формирование стейта, в честности пункта achivs_list
%% @spec  get_achivs_list(State) -> #state.achivs_list.
%% @end
%%--------------------------------------------------------------------

get_ava_state(State) ->

case (pgsql:connect(?DB_HOST, [?DB_USERNAME], [?DB_PASS], [?DB_PLG_OPTIONS])) of 
    {ok, DB_Link} -> 

                    case (pgsql:equery(DB_Link, "SELECT id, img FROM lib_ava WHERE buy > 0 ORDER BY RANDOM() OFFSET 0 LIMIT 40;", [])) of
                    {ok, _Columns, Rows}  ->
                        if (is_list(Rows)) -> 
                                    GAL = fun (A, AccIn) ->
                                        case (A) of
                                            {AId, AImg} -> lists:flatten([AccIn, [{2000+AId, binary_to_list(AImg)}]]);
                                                    _ -> AccIn
                                        end
                                    end,
                                    Ava_list = lists:foldl(GAL, [], Rows),
                                    libs_functions:wrap_cron_once(3600, {ava_info, get_ava_state, []}),
                                    NewState = State#state{ ava_list = Ava_list, buyed_ava = [] };

                            true -> libs_functions:wrap_cron_once(60, {ava_info, get_ava_state, []}),
                                    NewState = State
                        end;
                    ErM -> ?INFO_MSG("Error on DB query. Respone: ~p~n ", [ErM]), 
                           libs_functions:wrap_cron_once(60, {ava_info, get_ava_state, []}), 
                           NewState = State
                    end,
                    pgsql:close(DB_Link);

   {error, Reason} -> ?INFO_MSG("Error on DB connect. Reason: ~p~n ", [Reason]),  
                      libs_functions:wrap_cron_once(60, {ava_info, get_ava_state, []}),
                      NewState = State;

                _ ->  libs_functions:wrap_cron_once(60, {ava_info, get_ava_state, []}),
                      NewState = State
end,
NewState
.

%%--------------------------------------------------------------------
%% @doc Запрос на получение информации 
%% @spec  show_info(Type, State) -> ok.
%% @end
%%--------------------------------------------------------------------
show_info(Ava_id, Type, State) ->
%% возвращает информацию о аватаре

Ava = get_ava_by_id(Ava_id, State#state.ava_list),

case (Type) of
        1 -> Ava;
        img -> {_AvaId, AvaImg} = Ava,
               list_to_binary(AvaImg);
        _ -> Ava
end
.

%%--------------------------------------------------------------------
%% @doc Запрос на получение списка аватар для продажи
%% @spec  get_ava_list(State) -> [].
%% @end
%%--------------------------------------------------------------------

get_ava_list(State) ->
%    Buyed_ava = State#state.buyed_ava,
    GAvaList = fun(A, AccIn) ->
        {ANumCount, ALAcc} = AccIn,
        case (A) of 
            _M when (ANumCount =<29) ->
                                    {AId, _AImg} = A,
                                  %  MFlag = lists:member(AId, Buyed_ava),
                                   % if (MFlag == false) ->
                                        ANumCount_out = ANumCount+1,
                                        ALAcc_out     = lists:flatten([ALAcc, AId]),
                                        {ANumCount_out, ALAcc_out};
                                    %true -> AccIn
                                    %end;
            _ -> AccIn
        end
    end,

    {_ACount, AvaList} = lists:foldl(GAvaList, {0, []}, State#state.ava_list),
    AvaList
.

get_buyed_ava(State) ->
    State#state.ava_list
.

add_buyed_ava(A_id, State) ->
    %State#state { buyed_ava = lists:flatten([State#state.buyed_ava, [A_id]]) }

case (pgsql:connect(?DB_HOST, [?DB_USERNAME], [?DB_PASS], [?DB_PLG_OPTIONS])) of 
    {ok, DB_Link} -> 
            case (pgsql:equery(DB_Link, "update lib_ava set buy=0, date=now() where id = $1;", [(A_id-2000)])) of
                    {error, ReasonQ} -> NewState = State, ?INFO_MSG("Querry error on DB. Reason: ~p~n ", [ReasonQ]);
                                   _ -> % если изменили в базе, то и стэйт надо сменить
                                        DAL = fun (A, AccIn) ->
                                                {AId, _AImg} = A,
                                                case (A) of
                                                    [] -> AccIn;
                                                    _M when AId =/= A_id -> lists:flatten([AccIn, [A]]);
                                                    _ -> AccIn
                                                end
                                        end,
                                        Ava_list = lists:foldl(DAL, [], State#state.ava_list),
                                        NewState = State#state { ava_list = Ava_list, buyed_ava = lists:flatten([State#state.buyed_ava, [A_id]])}
            end,
            pgsql:close(DB_Link);

   {error, Reason} -> ?INFO_MSG("Error on DB connect. Reason: ~p~n ", [Reason]),  
                       NewState = State;
                _ -> NewState = State
end,

NewState


.

get_ava_by_id(Ava_id, Ava_list) ->
    AvaInf = fun(A, AccIn) ->
        {AId, _AImg} = A,
        case (A) of
            [] -> AccIn;
            M when AId == Ava_id -> M;
             _ -> AccIn
        end
    end,
    lists:foldl(AvaInf, {0, ""}, Ava_list)
.
