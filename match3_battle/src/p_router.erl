%%%-------------------------------------------------------------------
%%% File    :
%%% Author  : Marat Yusupov
%%% Description :
%%% Copyright   : Marat Yusupov marat@yusupov.me
%%% Created :
%%%-------------------------------------------------------------------
%%
%% Задача принять сообщения и смаршрутизировать в нужный процесс
%%
%%

-module(p_router).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {host,all_pool=[]}).

-define(SERVER, ?MODULE).
-include("shared.hrl").


-behaviour(gen_mod).
-include("ejabberd.hrl").
-include("jlib.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Запускаемся (запускает локальный сервер)
%%--------------------------------------------------------------------
start_link() ->
gen_server:start_link( ?MODULE, [], []).
%%start_link(R) ->   %% это чтоб имя другое можено было задать для пула
    %%{ok,Pid} = gen_server:start_link({local, R}, ?MODULE, [], []),Pid.
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description:  Пока нечего инициализировать
%%--------------------------------------------------------------------
init([]) ->
    Name = pool_router,
    pg2:create(Name),
   FF = pg2:join(Name, self()), 
  ?INFO_MSG("add pool_router group : ~p", [FF]),
{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages Call — это метод, блокирующий клиент.
%%--------------------------------------------------------------------
handle_call(error, _From, State) ->
%% для теста перезапуска супервизором
{stop, error, State};
handle_call(stop, _From, State) ->
{stop, normal, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% Cast — это неблокирующий или асинхронный метод.
%%--------------------------------------------------------------------

handle_cast({query_from_client,To, From, Packet2  = {xmlelement, "message", Attrs, _Els}}, State ) ->
%% принять запрос и смаршрутизировать куда следует
    case xml:get_attr_s("type", Attrs) of
        "chat" -> %% то что  надо маршрутизировать
	    %%  ?INFO_MSG("dropping groupchat: ~s", [xml:element_to_string(Packet2)]),
            Body = xml:get_path_s(Packet2, [{elem, "body"}, cdata]),
	    Jid=jlib:jid_to_string(From),
	    Pid4pro=gproc:lookup_local_name(Jid),
	    %%    ?INFO_MSG("Body msg in: ~s", [Body]),
	    Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,

	    case (Profile_is_live(Pid4pro)) of
		true  ->
		    %% отправить запрос туда
		    gen_server:cast(Pid4pro, {query_type, To, From,  Body});
		_ ->
		    %%запустить нужный процесс
		    %% для предотвращения состояния гонок передаем другому процессу для старта - единая точка старта всех профилей
		    plg_serial_start:start_process(Jid, match_profile,start_link,Jid,{query_type, To, From,  Body})
	    end,
            ok;
        "error" -> %% we don't log errors
	    %%            ?INFO_MSG("dropping error: ~s", [xml:element_to_string(Packet2)]),
	    %%            ?INFO_MSG("dropping error: ~p", [Packet2]),
            ok;
        _ ->
	    %%  ?INFO_MSG("any packet: ~s", [xml:element_to_string(Packet2)]),
	    Body = xml:get_path_s(Packet2, [{elem, "body"}, cdata]),
	    Jid=jlib:jid_to_string(From),
	    Pid4pro=gproc:lookup_local_name(Jid),
	    Profile_is_live = fun(A) when is_pid(A) -> is_process_alive(A); (_) -> false end,

	    case (Profile_is_live(Pid4pro)) of
		true  ->
		    %% отправить запрос туда
		    gen_server:cast(Pid4pro, {query_type, To, From,  Body});
		_ ->
		    %%запустить нужный процесс
		    %% для предотвращения состояния гонок передаем другому процессу для старта - единая точка старта всех профилей
		    plg_serial_start:start_process(Jid, match_profile,start_link,Jid,{query_type, To, From,  Body})
	    end,
           ok
    end,


{noreply, State};

handle_cast(_A, State) ->
%% другие сообщения нас не волнуют
{noreply, State}.

    %% пример как определять есть ли указанный тег в xml
    %%case xml:get_subtag(Packet, "body") of
    %%    false ->  'empty';
    %%     Body_xml ->
    %%       case xml:get_tag_attr_s("type", Packet) of
    %%            "groupchat" -> 'empty';
    %%            _ ->  {ok, xml:get_tag_cdata(Body_xml)}
    %%         end





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
%% Description:
%% Сервер может прервать свою работу по многим причинам. Один из handle_Something вызовов может вернуть
%% {stop, Reason, NewState}, либо сервер может рухнуть при сообщении {‘Exit’, reason}.
%% При любом раскладе будет вызвана функция terminate(Reason, NewState).
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
 Name = pool_router,
pg2:leave(Name, self()),
  ?INFO_MSG("dropping error: ~s", ["termininate"]),
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

