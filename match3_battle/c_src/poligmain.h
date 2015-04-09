/*defsef*/
#ifndef __POLIGMAIN_H
#define __POLIGMAIN_H

//#define Test_serv 1

const unsigned int MAPX_Y =8;
const int MAPX =MAPX_Y; // i
const int MAPY = MAPX_Y; // k
const int face_choice_max=6; // 6 разных фишек да
const int MAX_MAP_COUNT = 12;
const int IF_TURN_CHANGE_SLEEP = 0; // время сколько ждем перед отправкой сообщения что ход сменился

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/timeb.h>
#include <ctype.h>
#include <stdio.h>
#include <memory.h>
#include <set>
#include <vector>

#include <sys/time.h>
#include <vector>
#include <iostream>
#include <sstream>
#include <stdint.h>

class CCell {
public:
    int choice_type; // тип фишки в ячейке
    int for_drop_id; // идентификатор сектора при уничтожении
    int get_type() { return choice_type; }
};

class Match_Map {
public:
    int se_pan_b;
    // массив параметров для фишек
    //int  value_effect_choice[face_choice_max];
    //массив параметров для параметров
    // 0-2 не используется 3 щит 4 аптечка 5 доп урон
    int  value_effect_perk[face_choice_max];
    CCell board[MAPX][MAPY];
    int is_free;
    int win_user;

    // получить сектор координаты
    int  get_sector_id(int i, int k);
    // установить сектор координаты
    void set_sector_id(int i, int k,int id);
    // получить тип фишки по координатам i k
    int get_type(int i, int k);
    Match_Map();
    void set_free_map();
    // актуализация эффекта двойного урона
    void actual_double_effect();
    // ====== схватка ==========

    // кто ходит 0 первый игрок 1 второй игрок
    int user_active_turn;
    // Тип перехода хода который уходит клиенту
    // через эту промежутучную переменную в tools type_move_user
    int send_active_turn_type;
    //флаг что ход не переходит устанавливается
    int not_change_turn;
    //флаг бонусный со слотов накапливается даже с учетом флага not_change_turn
    // срабатывает строго на текущего игрока
    int not_change_turn_bonus;

    // кол-во пропусков хода
    int count_alarm[2];
    // желтая карточка для игрока через три желтые техническое поражение
    int alarm_active_turn_user(int type);
    // сменить ход на ход следующего игрока
    int change_active_turn_user();
    // пользователь на которого срабатывают первые три эффекта
    int get_noactive_user();
    int get_val_choice(int num_choice);
    //предельные значения перков
    int max_user_perk[2][6];
    // здоровье и параметры игроков
    int user_perk[2][6];
    // флаги изменения перков игрока
    int user_perk_change[2];
    // игровые флаги    1 эффект есть 0 эффекта нет
    // user_flag[user][type]
    // type
    // 0 блокировка урона 1 удвоенный урон значение
    // 2 удвоенный урон готовность сработать
    int user_flag[2][3];
    // изменяет указаный параметр игрока
    void change_perk(int user,int num_perk,int value);
    // установить параметр игрока "вручную" без проверок эффектов
    void set_perk(int user,int num_perk,int value);
    // проверка допустимости значения параметра
    void valid_perk(int user,int num_perk);
    //двойной ли урон
    int double_dp(int user);
    // изменения параметров игрока по результатам хода
    int apply_effect_choice(int num_choice,int count_choice,int add_five);
    //применение эффекта параметров типа удвоение урона вылечивание или блокировка
    void apply_perk_effect (int user,int num_perk);
};


class CSegment {
public:

    int is_free; //маркер свободен занят
    int choice_type; // тип фишки в ячейке
    int count_choice; // кол-во фишек в сегменте
    std::ostringstream in_line_str; // "f_5_0":3,"f_5_1":3,"f_5_2":3,"f_5_3":3,"f_5_4":3,"f_3_1":1,"f_4_1":1,"f_5_1":1
    CSegment(){
       is_free=1;
    }
    void set_segment( int choice_type1,int i,int k){
        if(is_free==1) {
            is_free=0;
            count_choice=1;
            choice_type=choice_type1;
            in_line_str<<"\"f_"<<i<<"_"<<k <<"\":"<<choice_type1;
            return;
        }
        count_choice++;
        in_line_str<<",\"f_"<<i<<"_"<<k <<"\":"<<choice_type1;
        return;
    }

};

class Utilits4Maps {
public:
    // id сектора
    int next_id;
    // функция подготовки к ходу
    void ready_for_turn();
    Utilits4Maps();
    void setup_random_map(Match_Map *apply);
    void clear_out_matches(Match_Map *apply);
    // подготовка карты к новой битве ход всегда у первого чела
    void new_battle(Match_Map *apply, int user0x0,    int user0x1,    int user0x2,
                    int user1x0,    int user1x1,    int user1x2);
    void set_win_status(Match_Map *apply);
    void get_perk_status(Match_Map *apply);
    // перестроить карту например когда нет ходов даем новую карту
    void re_init_map(Match_Map *apply);
    // возможно ли поменять местами координаты ?
    int cap_swap(int i1, int k1, int i2, int k2);
    // смена местами двух координат
    void swap_board(Match_Map *apply,int i1, int k1, int i2, int k2);
    // имеются ли возможности для хода  в указанной точке
    int could_have_match(Match_Map *apply,int i, int k);
    // поиск подсказки
    int find_hint(Match_Map *apply,int *i, int *k);
    // есть ли совпадения на карте
    int any_matches(Match_Map *apply);
    // есть ли совпадения в точке
    // TODO выдавать результат исследования
    // TODO добавить пересечения считаются совместные - то есть и по вертикали и по горизонтали то есть буквами(Г, Т, итд)
    int is_part_of_match(Match_Map *apply,int i, int k);
    // вертикальное совпадение
    int has_vertical_match(Match_Map *apply,int i, int k,bool make_str );
    // горизонтальное
    int has_horizontal_match(Match_Map *apply,int i, int k,bool make_str);
    // падение фишек фактически при установленном флаге  make_str=true
    // помечает ячейки в которых должны появиться новые фишки
    // типом apply->board[i][z].choice_type= face_choice_max+i+z*8;
    // иначе заставляет "падать" столбы фишек при уничтожении ammount фишек от координаты i, to
    void drop_row(Match_Map *apply,int i, int to, int ammount,int make_str);
    // падение фишек и полный анализ результатоы битвы
    void drop_all(Match_Map *apply,int drop_cicle);
    // функция анализа карты и генерации фишки с понижением выпадений совпадений 4рок
    int get_chois(Match_Map *apply,int i, int k);

    // Ошибка
    void error_number(int number,const std::string &info);
    // поиск всех match3 на карте
    int find_all_matches(Match_Map *apply);
// ход
    void change(Match_Map *apply, int i1, int k1, int i2, int k2);
// бонус доп хода
    void add_turn(Match_Map *apply);
// Взрыв креста
    void boom(Match_Map *apply, int i1, int k1);
//
    void refresh(Match_Map *apply, int k1);
// случайные 12 координат
    void random(Match_Map *apply);

    void Print(Match_Map *apply);
// время битвы вышли считаем победителя
    void end_timeout(Match_Map *apply);





    // строка в которую собрано по порядку уничтоженные и появившиеся ячейки
    std::ostringstream all_destroy_f;
    std::ostringstream new_f_after_drop; // строка с new_f за один раунд игры
    // состояние перков игроков
    std::ostringstream user_panel[2][6];
    //флаги что изменились параметры игроков
    int user_perk_change[2];
    // ход чей ??
    int now_move_user;
    // тип перехода (сохранения хода)
    /*
%% move_type определяет тип перехода хода или тип сохранения хода
%% 0 уничтожение >3 фишек ход сохранился
%% 1 ход сменился в результате стандартных правил игры (начало боя, уничтожено 3 фишки)
%% 2 неправильные координаты
.. см вики
%% 6 сохранение хода в результате применения бонуса
*/
    int type_move_user;
    // время через которое следует отправить сообщение о ходе - фактически показатель
    // того сменился ход или нет
    int send_interval;

    // флаг того были ли ошибки при ходе
    int error_flag;
    //кто победил
    int win_user;

    union  MapID {
           struct {uint8_t byte1:8;  uint8_t byte2:8; } b;
           uint16_t allbyte;
                 } query_map_id; //< идентификатор карты над которой проводится эксперемент

    //Сделано вроде
    uint8_t flag_match3_more; //< флаг уничтожили больше 3х



 };


#endif
