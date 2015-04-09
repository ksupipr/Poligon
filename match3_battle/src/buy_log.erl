%%%-------------------------------------------------------------------
%%% File    : buy_log.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Логи покупок
%%%
%%% Created :  18 Jul 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(buy_log).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").

%% API
-export([start_link/0, send_log/3]).

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
    ?INFO_MSG("buy_log start: ~p; ", [Sl]),
    Sl
.

%%====================================================================
%% gen_server callbacks
%%====================================================================

send_log(Jid, Credits_now, Credits_add) ->
    gen_server:cast(?SERVER, {send_log, Jid, Credits_now, Credits_add}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Any) ->
    Name = <<"buy_log">>,
    gproc:add_local_name(Name),

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


handle_cast({send_log, Jid, Credits_now, Credits_add}, State) ->
    send_log(Jid, Credits_now, Credits_add, State),
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

%%--------------------------------------------------------------------
%% @doc Запись в базу
%% CREATE TABLE buy_log (id serial,  jid text NOT NULL,  credits_now integer DEFAULT 0, credits_add integer DEFAULT 0, date timestamp without time zone DEFAULT now() );
%% @spec  send_log(Jid, Credits_now, Credits_add, _State) -> ok.
%% @end
%%--------------------------------------------------------------------

send_log(Jid, Credits_now, Credits_add, _State) ->

Querry = lists:flatten(["INSERT INTO buy_log (jid, credits_now, credits_add) VALUES ('", Jid, "', ", integer_to_list(Credits_now), ", ", integer_to_list(Credits_add), ");"]),
    
case (pgsql:connect(?DB_HOST, [?DB_USERNAME], [?DB_PASS], [?DB_PLG_OPTIONS])) of 
    {ok, DB_Link} -> 
            case (pgsql:equery(DB_Link, Querry, [])) of
                    {error, ReasonQ} -> ?INFO_MSG("Querry log error on DB. Reason: ~p~n Querry: ~p ~n", [ReasonQ, Querry]);
                                   _ -> % если изменили в базе, то все хорошо
                                        ok
            end,
            pgsql:close(DB_Link);

   {error, Reason} -> ?INFO_MSG("Error log on DB connect. Reason: ~p~n Querry: ~p ~n", [Reason, Querry]);
                ME ->  ?INFO_MSG("Error log on DB connect. X3: ~p~n Querry: ~p ~n", [ME, Querry])
end.