%%%-------------------------------------------------------------------
%%% File    : shop.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Магазин.
%%%
%%% Created :  14 Mar 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(shop).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").

%% API
-export([start_link/0, show_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {

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
    ?INFO_MSG("shop start: ~p; ", [Sl]),
    Sl
.


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
    Name = <<"shop">>,
    gproc:add_local_name(Name),

%    C = plg_redis_wrapper:get_link_redis(?EREDIS_INF),     State = #state{redis_link=C},
    State = #state{},
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

handle_cast({show_all, Type, User}, State) ->
%% показ всего магазина. По сути первичный зпрос.

    Name = <<"things">>,
    Pid4pro=gproc:lookup_local_name(Name),

    case (libs_functions:alive(Pid4pro)) of
        false -> %%запустить нужный процесс
                {ok,Pid} = things:start_link(),
                gen_server:cast(Pid, {show_all, Type, User});
            _ -> gen_server:cast(Pid4pro, {show_all, Type, User})
    end,
    {noreply, State};


handle_cast({buy_th, User, Buy_id, UState}, State) ->
%% показ всего магазина. По сути первичный зпрос.
%% rfr dfhbfyn dtob jnltkmysv vjlektv b rf;le. ghjwtccjv
 %%   _C = State#state.redis_link,

    Name = <<"things">>,
    Pid4pro=gproc:lookup_local_name(Name),
    
    case (libs_functions:alive(Pid4pro)) of
        false -> %%запустить нужный процесс
                {ok,Pid} = things:start_link(),
                gen_server:cast(Pid, {buy_thing, User, Buy_id, UState});
            _ -> gen_server:cast(Pid4pro, {buy_thing, User, Buy_id, UState})
    end,
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

show_info(Bonus_inf) ->
    Bonus_inf.
