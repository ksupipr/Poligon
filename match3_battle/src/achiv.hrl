%% http://projects.xlab.su/projects/poligon/wiki/%D0%94%D0%BE%D1%81%D1%82%D0%B8%D0%B6%D0%B5%D0%BD%D0%B8%D1%8F

-record(achiv, {
      id     = 0           %% идентификатор
    , group  = 0           %% группа
    , name = ""
    , sname = ""
    , descr = ""
    , add_descr = ""
    , img   = ""        %% Иконка вручения
    , icon  = ""        %% Иконка отображения
    , type   = 0        %% местоположение вывода [1 - звание, 2 - медаль, 3 - достижение и пр.]
    , t_type   = 0      %% тип проверки 1-постоянные, 2 - Суточные параметры, 3 - недельные, 4 - месячные
    , dell_type = 0     %% тип удаления 0 - не удаляется, 1 - сутки, 2 - неделя, 3 - месяц
    , stacked  = 0      %% Стек (если 0, то не копится, если 1 - накапливаемый стек)
    , qntty    = 0      %% Количество (если 0 - то нету)
    , level     = 0     %% Качество (0 - обычное. Потолка нет)
    , get_date          %% Дата-время получения
    , dell_date         %% Дата-время удаления достижения
    , state_params = [] %% Параметры статистики для сравнения {stat_num, stat_count, arith_type}
                        %% stat_num - номер записи в статистике, stat_count - значение параметра достаточное для получения достижения, arith_type - как сравнивать
                        %% more      - параметр больше значения {67, 10, more}
                        %% less      - параметр меньше значения {67, 10, less}
                        %% summore   - сумма параметров больше значения {[67, 68, 69], 10, summore}
                        %% sumless   - сумма параметров меньше значения {[67, 68, 69], 10, sumless}
                        %% firstmore - 1 параметр больше 2 {{67, 68}, 0, firstmore}
                        %% firstless - 1 параметр меньше 2 {{67, 68}, 0, firstless}
                        %% sumfirstmore - сумма 1х параметров больше 2х {{[67, 68], [69, 70]}, 0, sumfirstmore}
                        %% sumfirstless - сумма 1х параметров меньше 2х {{[67, 68], [69, 70]}, 0, sumfirstless}
    , need_achivs = []  %% Необходимость других достижений для получения {achiv_id, achiv_num, dell}  
                        %% achiv_id - ид достижения которое надо , achiv_num - количество (0 считается как неучитываемый), dell- удалить ли достижение после получения текущего
    , add_params = {[], 0, []}   %% Параметры для увеличения {satat, money_0, bonuses} 
                                 %% satat = {id, val}, money_0 = Int, bonuses = {id, num}
    , get_desc_json     %% Fun  выдает описание вещи
}).


%% запись профиля касаемая достижений

-record(getted, {
      rank = {1, 0, 1}         %% Звание  {Id, Date_get, Qntty}
    , medals = []       %% медали {Id, Date_get, Qntty}
    , achivs = []       %% достижения {Id, Date_get, Qntty}
}).


%% передаваемые на проверку параметры
-record(userparams, {
      jid                 %% идентификатор пользователя
    , statistics          %% статистика 
    , money_0 = 0        %% монеты
    , new_statistics = [] %% список изменения статистики [{id, +-add_val} ]
    , new_bonuses = []    %% список полученых бонусов     [{id, add_val}]
    , new_achive  = []    %% список полученых достижений о которых следует известить пользователя [{id,_date,val}]
    , dell_achive  = []    %% список удаляемых достижений [{id, date, val}]
}).

%{ 1, "рядовой",           "ряд.",    "", "http://tanks.github.com/images/pogony/1.png",  "", 3, 1, 0, 1, [], [] }
              %, { 2, "сержант",           "серж.",   "", "http://tanks.github.com/images/pogony/2.png",  "", 3, 1, 0, 1, [{67, 1000}], [{1, 1, 1}] }
              %, { 3, "старшина",          "ст-на",   "", "http://tanks.github.com/images/pogony/3.png",  "", 3, 1, 0, 1, [{67, 2800}], [{2, 1, 1}] }
              %, { 4, "лейтенант",         "л-т",     "", "http://tanks.github.com/images/pogony/4.png",  "", 3, 1, 0, 1, [{67, 6400}], [{3, 1, 1}] }
              %, { 5, "ст. лейтенант",     "ст.л-т",  "", "http://tanks.github.com/images/pogony/5.png",  "", 3, 1, 0, 1, [{67, 26400}], [{4, 1, 1}] }
              %, { 6, "капитан",           "к-н",     "", "http://tanks.github.com/images/pogony/6.png",  "", 3, 1, 0, 1, [{67, 46400}], [{5, 1, 1}] }
              %, { 7, "майор",             "м-р",     "", "http://tanks.github.com/images/pogony/7.png",  "", 3, 1, 0, 1, [{67, 71400}], [{6, 1, 1}] }
              %, { 8, "подполковник",      "п/п-к",   "", "http://tanks.github.com/images/pogony/8.png",  "", 3, 1, 0, 1, [{67, 96400}], [{7, 1, 1}] }
              %, { 9, "полковник",         "п-к",     "", "http://tanks.github.com/images/pogony/9.png",  "", 3, 1, 0, 1, [{67, 126400}], [{8, 1, 1}] }
              %, {10, "генерал-майор",     "ген.м-р", "", "http://tanks.github.com/images/pogony/10.png", "", 3, 1, 0, 1, [{67, 176400}], [{9, 1, 1}] }
              %, {11, "генерал-лейтенант", "ген.л-т", "", "http://tanks.github.com/images/pogony/11.png", "", 3, 1, 0, 1, [{67, 226400}], [{10, 1, 1}] }
              %, {12, "генерал-полковник", "ген.п-к", "", "http://tanks.github.com/images/pogony/12.png", "", 3, 1, 0, 1, [{67, 276400}], [{11, 1, 1}] }
              %, {13, "маршал",            "маршал",  "", "http://tanks.github.com/images/pogony/13.png", "", 3, 1, 0, 1, [{67, 326400}], [{12, 1, 1}] }
              %, {14, "генералиссимус",    "ген-с",   "", "http://tanks.github.com/images/pogony/14.png", "", 3, 1, 0, 1, [{67, 376400}], [{13, 1, 1}] }
