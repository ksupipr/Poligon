%% от кого приходят сообщения
-define(DOMAIN_RES_JID, "@im.xlab.su/xiff").
%%-define(DOMAIN_RES_JID, "@im.xlab.su/xiff").
%%-define(DOMAIN_RES_JID, "@poligon.xlab.su/xiff").


%% от кого приходят сообщения
-define(SEND_FROM, "io@echo.localhost").
%%-define(SEND_FROM, "test@echo.im.xlab.su").
%%-define(SEND_FROM, "game@echo.poligon.xlab.su").

%% префиксы социалок
-define(SN_PREFIXS, [<<"vk">>, <<"ok">>, <<"ml">>]).

%% курс покупки кредитов
-define(SN_TO_CRED, [{<<"vk">>, 1, 1}, {<<"ok">>, 1, 800}, {<<"ml">>, 1, 800}]).

%% подключение к редису {Name, HostR, PortR,  DatabaseR, PasswordR}
-define(EREDIS_INF, ["redis_inf_serv",{"localhost", 6379, 0, ""}]).
%%-define(EREDIS_INF, ["redis_inf_serv",{"192.168.1.4", 6379, 0, ""}]).
%%-define(EREDIS_INF, ["redis_inf_serv",{"192.168.1.20", 6379, 0, ""}]).


-define(EREDIS_PROFILE_SERV, ["redis_profile_serv_link",{"localhost", 6379, 0, ""}]).
%%-define(EREDIS_PROFILE_SERV, ["redis_profile_serv_link",{"192.168.1.4", 6379, 0, ""}]).
%%-define(EREDIS_PROFILE_SERV, ["redis_profile_serv_link",{"192.168.1.20", 6379, 0, ""}]).


-define(EREDIS_PAYMANT_SUB, ["redis_paymant_sub_link",{"localhost", 6379, 0, ""}]).
%% -define(EREDIS_PAYMANT_SUB, ["redis_paymant_sub_link",{"192.168.1.4", 6379, 0, ""}]).
%% -define(EREDIS_PAYMANT_SUB, ["redis_paymant_sub_link",{"192.168.1.20", 6379, 0, ""}]).

%% подключение к редису для топа {Name, HostR, PortR,  DatabaseR, PasswordR}
-define(EREDIS_TOP, ["redis_inf_serv",{"localhost", 6379, 0, ""}]).
%%-define(EREDIS_TOP, ["redis_inf_serv",{"192.168.1.4", 6379, 0, ""}]).
%%-define(EREDIS_TOP, ["redis_inf_serv",{"192.168.1.20", 6379, 0, ""}]).

%%-define(RES_DIR, "http://res2.xlab.su/poligon/").
-define(RES_DIR, "http://unlexx.no-ip.org/polig/").

-define(DB_HOST, "192.168.45.66").
%%-define(DB_HOST, "192.168.1.6").
-define(DB_USERNAME, "xlabpoliginterface").
-define(DB_PASS, "tmpT7jq4TOlrY").
-define(DB_PLG_OPTIONS, {database, "poligon"},{port,     6432}).
