%%%-------------------------------------------------------------------
%%% File    : sovets.erl
%%% Author  : Михаил Богатырев <ksupipr@yandex.ru>
%%% Description : Полигон. Модуль советов для логов.
%%%
%%% Created :  19 Jun 2012 by Михаил Богатырев <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(sovets).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").

%% API
-export([start_link/0, get_sovet/1, read_sovets/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
    sovets = []
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
    ?INFO_MSG("sovets start: ~p; ", [Sl]),
    Sl
.


%%====================================================================
%% gen_server callbacks
%%====================================================================

read_sovets() ->
        gen_server:cast(?SERVER, {read_sovets}).

get_sovet(User) ->
        gen_server:cast(?SERVER, {get_sovet, User}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Any) ->
    Name = <<"sovets">>,
    gproc:add_local_name(Name),

    State = read_sovets(#state{}),
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
handle_cast({read_sovets}, State) ->
    State1 = read_sovets(State),
{noreply, State1};

handle_cast({get_sovet, User}, State) ->
        get_sovet(User, State),
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
%%read_line(FileIO, LineOut) ->
%%    FileLine = file:read_line(FileIO),
%%    case FileLine of
%%        {ok, DataLine} -> Sovets = lists:flatten([DataLine, LineOut]),
%%                          read_line(FileIO, Sovets);
%%                     _ -> LineOut
%%    end                          
%%.



get_line(Text) ->
    TolineBR = re:replace(Text, "(\n)+", "<br>", [global]),

    P8 = fun(A, AccIn) -> 
        if (A == <<"<br>">>) ->
            AccIn;
        true -> lists:flatten([AccIn, [A]])
        end
     end

    %% ставим запятые
    , List_out = lists:foldl(P8,[] , TolineBR)
    , List_out
.

read_sovets(State) ->
    case (httpc:request(get, {lists:flatten([?RES_DIR, "res/locale/ru_RU/sovets.txt"]), []}, [], [])) of 
    {ok, Inner_text} -> %{ok, {{"HTTP/1.1",405, "Not Allowed"}, [], ""}}
                        {{_Prot, ReqNum, Mess}, _Any, FText} = Inner_text,
                        case (ReqNum) of
                            200 -> % все норм
                                    %?INFO_MSG("read_sovets:  FText: ~p;~n", [get_line(FText)]),
                                    NewState= State#state{ sovets = get_line(FText) };
                                    
                              _ -> ?INFO_MSG("read_sovets: Error Num: ~p; Mess: ~p;~n", [ReqNum, Mess]), NewState= State#state{ sovets = [] }
                        end;
                  FR -> ?INFO_MSG("read_sovets: Error file read: ~p~n", [FR]), NewState= State#state{ sovets = [] }
end,
NewState
.

get_sovet(User, State) ->
    Sovets = State#state.sovets,
    MaxSov = length(Sovets),
    N      = libs_functions:rand(MaxSov),
    if (N>0) ->
        Sovet  = lists:nth(N, Sovets),
        libs_functions:send_log_to_user(User, 0, {lists:flatten(["Совет: ", [Sovet]])}, "#613203");
    true -> ok
    end
.
