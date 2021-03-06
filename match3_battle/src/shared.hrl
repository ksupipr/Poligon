%%%-------------------------------------------------------------------
%%% File    :
%%% Author  : Marat Yusupov
%%% Description :
%%% Copyright   : Marat Yusupov marat@yusupov.me
%%% Created :
%% Файл разделяемых ресурсов
%%
%%
%%%-------------------------------------------------------------------

-record(query_from_client, {to_jid,querrr}).

%% часть рекорда описания пользователя в турнирной таблице
-define(PLG_TOURNAMET_USER_REC, {
	  num = 0  
	  , slot_state %% состояние слота после начала турнира 0 ожидание начала турнира 1 начало битвы 3 вылет
	  , firstName
	  , lastName = <<""/utf8>>
          , middleName = <<""/utf8>>
	  , jid %% игрок 
          , rank = <<""/utf8>> %% Звание
	  , litleRank = <<""/utf8>> %% краткое звание
	  , money_0  %%  монеток
          , pid %% пид процесса отвечающего за профиль
	  , score=0 %% очков рейтинга
	  , ava = <<""/utf8>> %% ава
  	  , user_status=0  %% статус пользователя
	  , sn_id =0 
}).
%% часть рекорда описания пользователя 
-define(PLG_USER_PROFILE_REC, {
  jid = <<""/utf8>>
, jid_bin = <<""/utf8>>
, firstName = <<""/utf8>> %% ФИО
, lastName = <<""/utf8>>
, middleName = <<""/utf8>>
, sn_id %%  ид  из соц сети же
, reg_time  %% время регистрации
, achiv         %% достижения #getted{}
, profile_link = <<" "/utf8>>  %% ссылка на профиль
, user_data = <<" "/utf8>> %% хранилище данных от игрока
%% онлайн топы
, msg_send = 1 %% можно ли слать игроку сообщения >0 
, user_param %% перки игрока - численность родов войск
, is_admin %% флаг того что игрок админ
%% время крайнего входа(init)
, last_init_time
%% когда крайний раз выдали игроку монетки
, last_enter_day
%% текущее число дней для "классности"
, everyday_count = 0
%% номер исполненной транзакции после входа // событий входа 
, tran_number=0 %% номер уже исполненной транзакции
%% статистика
, statistics %% #statistics{ }

%% набор свойств которые могут изменить вещи
, thing_change_param 
, buy_kredits %% {крайняя дата покупки, кол-во покупок}


%%  НИЖЕ не важные при сохранении игрока переменные
%%  Состояние профиля 0 запрос разрешен
%%  Состояние профиля 1 игрок в дуэли
%%  Состояние профиля 2 игрок в поединке
%%  Состояние профиля 3 игрок в турнире
%%  Состояние профиля 4 игрок в битве
%%  Состояние профиля 5 игрок в ожидании начала турнира
, transaction_state = 0
, send_status=0 %% 
, link_to_redis
, frends = []
, frends_list = [] %% json список друзей
, frends_list_offline = [] %% json список друзей оффлайн
%% турнир
, tour_num = 0 %% позиция в турнире
, tour_serv    %% сервер турнира
, tour_state = 0   %% 1 чел в турнире 0 не в турнире
, tour_ref_bet_timer = 0 %% запоминаем таймер ставки для того чтобы отменить при выходе игрокасообщение о ставке

%% Дуэль
, duel_state=0  %% состояние приглашения в дуэль  1 тот кто вызвает  2 чела вызвали
, duel_allow = 1 %% можно ли приглашать на дуэли свойство

, duel_enemy  %% {Jid,UserPerk} противника заполняется только у того кого вызвали
, duel_time   %% Время посденего приглашения в дуэль каждые 2 секунды приглашение в дуэль

, duel_bet=0  %% ставка
, duel_ref    %% идентификатор для таймаута дуэли
%% для боя
, ref_turn_timer %% ссылка на крон таймер для перехода хода
, timer_start_time={1, 0} %% время когда запущен таймер для перехода ходя (требуется для восстановления боя)
%% 0 все стандартно   2 ожидание напарника в случаной битве  3 битва   4 тренировка
, route_to = 0 %% куда роутить запросы типа то есть если чел в битве то 3

, premium_in_battle=0 %% активен ли на начало битвы у игрока режим премиум
, battle_id=0 %% идетификатор битвы карты
, player0 %% игрок который ходит первым
, player1 %% игрок который ходит вторым
, you_turn %% мой ли ход 0 не твой  -1 таймер сработал и уже не твой   1 твой
, battle_serv %%pid боевого сервера который обслуживает битву
, addScore=0 %% сколько рейтинга принесет победа
, ref_battle_max_timeout =0 %% идентификатор таймера который остонавливает слишком длинную битву
, bet=0    %% ставка
, battle_type = 0 %% тип битвы  1 поединок  2 дуэль  3 турнир   
, battle_invite_msg %% сообщение о начале битве сохраненное для восстановления битвы при обновлении
, kd_gr1 = 0 %% кд первой группы бонусов 0 кд нет
, kd_gr2 = 0 %% кд второй группы бонусов 0 кд нет
, enemyFio = "звание Имя Фамилия" %% хранилище данных от игрока 
, stat_bonus_count=0 %%14- Количество бонусов, использованных в битве
, stat_turn_count=0 %%для 15- кол-во переходов  хода за битву
, stat_change_in_turn = 0  %% для 82 - количество передвижений фишек  до перехода хода.
, stat_tr_count = 0  %% количество побед тренировок подряд

%% таймер само сохранения запускается автоматом и отрабатывает сам без регулирований
%% таймер терминации профиля требуется отменять и иницировать 
, term_timer_ref
%% ссылка на идентификатор таймера для премиума
, premium_ref
%% ссылка на идентификатор таймера для завершения 
, timeout_ref
, test_valid_state =0
%% тип правой панели
, top_status = 2
%% ридентификатор таймера make frend
, stop_timer
%% очередь автопокупок игрока
, auto_buy = []

}).

%% кол-во секунд в течении которых идет автоотказ от дуэли
-define(DUEL_QUERY_INTERVAL, 2).
%% Кол-во опыта прибавляемого дуэлью
-define(DUEL_SCORE_ADD, 15).
%% опыт добавляемы по победе в тренировке
-define(ADD_SCORE_TR, 0).
%% Ставка на случаной битве
-define(RBATTLE_BET, 50).

%% Кол-во ходов КД для 1 группы
-define(KD_GROUP_0, 4).
-define(KD_GROUP_1, 6).
-define(KD_GROUP_2, 4).
-define(END_BATTLE_SLEEP_INTERVAL, 4).

%% урон слотов защита 
-define(DP_ARMOR, 40).
%% урон слота Атака 
-define(DP_ATACK, 40).

%% рекорд для пересылки в вещь (то есть все что могут менять вещи в профиле)
-record(thing_change_param_rec, {
  money_1 = 0       %% кредиты
, money_0 = 0       %% монеток
, ava = <<""/utf8>> %% аватара
, premium           %% флаг премиум режима {Режим, Timestamp}  дата в unix timestamp
, slots             %% array слотов
}).

%% описание слота
-record(slot, {
  count = 0         %% кол-во вещей в слоте
, num               %%  номер слота оно же и есть thing_type
, thing             %% вешь
}).

-record(perk, {avia=200,    %%
               brone = 200,
               soldier = 200
               }).

%% статистика  %% { stat_day , stat_week, stat_moon, stat_always}
-record(statistics, {
   counter = 0 %% счетчик измененных параметров (при изменении каждого массива +1)
  , stat_day    %% дневоной массив
  , stat_week   %% недельный
  , stat_moon   %% месячный
  , stat_always  %% вечный

}).
%% число параметров статистики зарегистрированных для наблюдения
-define(STATISTICS_PARAM_COUNT, 107).
%% время на сохранение 
%% 1000 - секунда  пока установлено 3 минуты
-define(SAVE_STATE_TIME_INTERVAL,360000).
%% время в секундах после которых профиль завершает свою работу
-define(PROFILE_TIMEOUT,660).
%% длительность боя 20 минуты 1200 = 1020+180 
-define(BATTLE_TIMEOUT_DEF,1020).
%% длительность боев турнира кроме финала 12 минут 540+180= 540
-define(BATTLE_TIMEOUT_TURNIR,540).
%% Дебаг проверка валидности стейта
-define(VALID_STATE(X),valid_state(X)).
