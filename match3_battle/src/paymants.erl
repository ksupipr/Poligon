%%%-------------------------------------------------------------------
%%% File    : paymants.erl
%%% Author  : Mihail Bogatyrev <ksupipr@yandex.ru>
%%% Description : ml & ok & other socials networks paymants
%%%
%%% Created :  02 Jun 2012 by Mihail Bogatyrev <ksupipr@yandex.ru>
%%%-------------------------------------------------------------------
-module(paymants).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("poligon.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
  redis_link %% линк редиса
, name
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
    start_link(?EREDIS_PAYMANT_SUB).
start_link([]) ->
    gen_server:start_link( ?MODULE, [?EREDIS_PAYMANT_SUB], []);
start_link(Arg) ->
    gen_server:start_link( ?MODULE, Arg, []).

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
init([Name , {HostR, PortR,  _DatabaseR, PasswordR}]) ->
   {ok, Link}  = eredis_sub:start_link(HostR, PortR,  PasswordR),
   gproc:add_local_name(Name),
   %eredis:q(Link, ["SUBSCRIBE", "paymant.ml"]),
   %_Receiver = spawn_link(fun () ->
                                  eredis_sub:controlling_process(Link, self()),
                                  %eredis_sub:subscribe(Link, [<<"paymant.ml">>]),
                                  eredis_sub:psubscribe(Link, [<<"paymant.*">>]),
                                  %receiver(Link),
    %                      end),
{ok, #state{redis_link=Link, name = Name}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%%handle_call({send_top, Jid, Sort}, _From, State) ->
    %%?INFO_MSG("send_top: Jid: ~p; Sort: ~p", [Jid, Sort]),
%%    send_all({0, Jid, Sort},  State),
%%    Reply = Sort,
%%    State1 = State#state{send_flag = 1},
    %%{reply, Reply, State1};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(Mess , State) ->
    ?INFO_MSG("Paymmm Info subscribe: ~p;", [Mess]),
{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info(Msg, State) ->
 %Res1 = eredis:q(C, ["PSUBSCRIBE", <<"paymant.*">>]),
%?INFO_MSG("Paymmm handle_info: ~p;", [Msg]),

 case (Msg) of
                {pmessage,<<"paymant.*">>, _SKey, SMess, _SPid} -> 
                                                 
                                                SMess_term = libs_functions:binary_to_erl(SMess),
                                                {UserName, Money_add, Tid} = SMess_term,
                                                ?INFO_MSG("Pay UserName: ~p; Money_add: ~p;", [UserName, Money_add]),
                                                UserJid = libs_functions:make_jid(UserName),
                                                Pid4pro=gproc:lookup_local_name(UserJid),
                                             %%   ?INFO_MSG("Pay Pid4pro: ~p; UserJid: ~p;", [Pid4pro, UserJid]),


                                                case (libs_functions:alive(Pid4pro)) of
                                                    false -> %%запустить нужный процесс
                                                            {ok,Pid } = match_profile:start_link(UserJid),
                                                            gen_server:cast(Pid, {add_money, 0, Money_add}),

                                                            libs_functions:send_log_to_user(UserJid, 2, {1, Money_add},[]),

                                                            if (Tid>0) -> gen_server:cast(Pid, {make_auto_buy, Tid});
                                                                true  -> ok
                                                            end;

                                                        _ -> %% отправить запрос туда
                                                            gen_server:cast(Pid4pro, {add_money, 0, Money_add}),

                                                            libs_functions:send_log_to_user(UserJid, 2, {1, Money_add},[]),

                                                            if (Tid>0) -> gen_server:cast(Pid4pro, {make_auto_buy, Tid});
                                                                true  -> ok
                                                            end
                                                end;
                                           _ -> ?INFO_MSG("Pay NO IN BLIA msg= ~p;", [Msg]),
                                                ok
            end,

Link = State#state.redis_link,
eredis_sub:ack_message(Link),
    
    %%?assertEqual({ok, [<<"psubscribe">>, <<"paymant.*">>, <<"1">>]}, Res1),

  {noreply, State}.
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State = #state{redis_link=Link} ) ->
   eredis_sub:stop(Link),
   %% из gproc автоматом выпадем как только завершимся
    ?INFO_MSG("Paymants has TERMINATE!! Reason: ~p ~n State: ~p;", [Reason, State]),
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





