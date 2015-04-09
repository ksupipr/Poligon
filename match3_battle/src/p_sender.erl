%%%-------------------------------------------------------------------
%%% File    :
%%% Author  : Marat Yusupov <marat@yusupov.me> and Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Пересылка сообщений игрокам от модулей Полигона
%%% Copyright   : Marat Yusupov marat@yusupov.me
%%% Created :
%%%-------------------------------------------------------------------
-module(p_sender).
-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/0]).
%% EJABBER_mod API
-export([send_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).





-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").

-record(state, {jid=0}).

-define(PROCNAME, ejabberd_p_sender).


%%====================================================================
%% API
%%====================================================================
send_msg(_To,  []) ->
ok;
send_msg(To,  Text) when is_record(To,jid); is_list(To) ->
%%  правильная рассылка сообщений
Name = p_sender,
    Ali = pg2:get_closest_pid(Name),
    gen_server:cast(Ali, {route, To,  Text}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
gen_server:start_link( ?MODULE, [], []).




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
init(_) ->
 Name = p_sender,
 pg2:create(Name),
 pg2:join(Name, self()), 
 From1 = jlib:string_to_jid( ?SEND_FROM),
{ok, #state{jid=From1}}.

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

handle_cast({route, From, To, Text}, State) ->
        echo(From, To, Text),
        {noreply, State};

handle_cast({route, To, Text}, State) ->
        echo(State#state.jid,To, Text),
        {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%
%%
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
 Name = p_sender,
pg2:leave(Name, self()),
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: echo(From, To, Taxt)
%% Description: send Text to user
%%--------------------------------------------------------------------

echo(From, To, Text) ->
    send_message(From, To, "chat", Text).



%%--------------------------------------------------------------------
%% Func: send_message(From, To, TypeStr, BodyStr)
%% Description: send Text to user
%%--------------------------------------------------------------------
send_message(From, To, TypeStr, BodyStr) when is_list(From),is_list(To) ->
    XmlBody = {xmlelement, "message",
           [{"type", TypeStr},
        {"from", From},
        {"to", To}],
           [{xmlelement, "body", [],
         [{xmlcdata, BodyStr}]}]},
 ejabberd_router:route(jlib:string_to_jid(From),jlib:string_to_jid(To), XmlBody);
send_message(From, To, TypeStr, BodyStr) when  is_record(From,jid),is_record(To,jid) ->
    XmlBody = {xmlelement, "message",
           [{"type", TypeStr},
        {"from", jlib:jid_to_string(From)},
        {"to", jlib:jid_to_string(To)}],
           [{xmlelement, "body", [],
         [{xmlcdata, BodyStr}]}]},
    ejabberd_router:route(From, To, XmlBody);
send_message(LFrom, LTo, TypeStr, BodyStr) when 
(
is_record(LFrom,jid) and
is_list(LTo) )
orelse
( 
is_list(LFrom) and
is_record(LTo,jid)
)
  ->

  From =case LFrom  of
	      M when is_record(M,jid)  -> JFrom=M, jlib:jid_to_string(M);
	      A -> JFrom=jlib:string_to_jid(A),A	  
	  end,
  To  =case LTo  of
	      Mto when is_record(Mto,jid)  -> JTo = Mto, jlib:jid_to_string(Mto);
	      Ato -> JTo=jlib:string_to_jid(Ato),Ato	  
	  end,
    XmlBody = {xmlelement, "message",
           [{"type", TypeStr},
        {"from", From},
        {"to", To}],
           [{xmlelement, "body", [],
         [{xmlcdata, BodyStr}]}]},
    ejabberd_router:route(JFrom, JTo, XmlBody).





