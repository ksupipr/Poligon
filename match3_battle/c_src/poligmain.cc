//============================================================================
// Name        : $0.cpp
// Author      : Marat yusupov
// Version     :
// Copyright   : Marat Yusupov marat@yusupov.me
// Description : Начало Игры полигон
//============================================================================





#include "poligmain.h"


//// <<<<<<<<<<<< BEGIN >>>>>>>>>>>>>>>>>>>>>>>


/**
    {"free", 1, unlock},
    {"get_map", 1,get_map },
    {"next_user", 1,next_user }, // переход хода
    {"new", 6, all_query },
    {"change", 5, all_query},
    {"boom", 3, all_query},
    {"line", 2, all_query}

new (jid1,tx0,tx1,tx2, jid2,tx0,tx1,tx2) -> id, << map >>
get (id) ->  << map >>
change(id,x1,y1,x2,y2) -> battle_result, << map_change >>
line(id,x1,y1,x2,y2) -> battle_result, << map_change >>
boom(id,x1,y1,x2,y2) -> battle_result, << map_change >>

Карта -> id, bool free
битва -> id, id_map, turn, user_status

*/



 int Match_Map::get_type(int i, int k){
 return board[i][k].get_type();
 }
 int Match_Map::get_sector_id(int i, int k){
 return board[i][k].for_drop_id;
 }
 void Match_Map::set_sector_id(int i, int k,int id){
 board[i][k].for_drop_id=id;
 }

Match_Map::Match_Map(){
    count_alarm[0]=0;
    count_alarm[1]=0;
     user_active_turn=0;
     //%% 1 ход сменился в результате снартных правил игры (начало боя, уничтожение 4 фишек, использование )
     send_active_turn_type=1;
     se_pan_b=0;
     not_change_turn=0;
     not_change_turn_bonus=0;
     win_user=3; //битва идет
     for (int i=0; i<2; i++)
         for (int j=0; j<3; j++){
        user_flag[i][j]=0;
         }

     for (int i=0; i<2; i++)
         for (int j=0; j<6; j++){

             max_user_perk[i][j]=100;
             user_perk[i][j]=100;
             if(j>2) user_perk[i][j]=0;
         }
  /*   max_user_perk[0][0]=100; // пехота
     max_user_perk[0][1]=100; // бронетехника
     max_user_perk[0][2]=100; // авиация
     max_user_perk[1][0]=100;
     max_user_perk[1][1]=100;
     max_user_perk[1][2]=100;*/
        max_user_perk[0][3]=220; // //аптечка
        max_user_perk[0][4]=100; // крит
        max_user_perk[0][5]=160; //  блок

        max_user_perk[1][3]=220;
        max_user_perk[1][4]=100;
        max_user_perk[1][5]=160;
     user_perk_change[0]=0;
     user_perk_change[1]=0;

       //max_user_perk[0][3]=100;// аптечка
      // max_user_perk[0][4]=120; //двойной урон
       //max_user_perk[0][5]=140; //блок
       //max_user_perk[1][4]=120; //двойной урон
       //max_user_perk[1][5]=140; //блок
     // массив параметров для фишек
     //пехота, бронетехника, авиация, аптека, дв. урон, защита
   /*
     value_effect_choice[0]=-15; // урон
     value_effect_choice[1]=-15; // урон броневикам
     value_effect_choice[2]=-15; // урон

     value_effect_choice[3]=2; //
     value_effect_choice[4]=2; //
     value_effect_choice[5]=2; //
  */
     //массив параметров для параметров
     // 0-2 не используется 3 щит 4 аптечка 5 доп урон
     value_effect_perk[0]=0; // не используется
     value_effect_perk[1]=0;
     value_effect_perk[2]=0;

     value_effect_perk[3]=40; // эффект аптечки
     value_effect_perk[4]=3; // множитель урона
     value_effect_perk[5]=1;  // флаг щита

    // здоровье первого игрока
    /* user_perk[0][0]=100; // user1x0
    user1x0=100;   0 0
    user1x1=100;   0 1
    user1x2=100;   0 2
    user1p0=0;     0 3
    user1p1=0;     0 4
    user1p2=0;     0 5
    // здоровье второго
    user2x0=100;
    user2x1=100;
    user2x2=100;
    user2p0=0;
    user2p1=0;
    user2p2=0; */
    set_free_map();
}
// сделать карту свободной
void Match_Map::set_free_map(){
is_free=1;
}

void Match_Map::actual_double_effect(){


    for (int user=0;user<2;user++) {
#if defined  Test_serv
    printf( "\n Крит!! user %d flag1 %d  flag2 %d " , user ,user_flag[user][1] ,user_flag[user][2]);
 #endif
    if((user_flag[user][1]==0)&&(user_flag[user][2]>0)){

    user_flag[user][1]=user_flag[user][2];
    user_flag[user][2]=0;

    }

    }


}
int Match_Map::alarm_active_turn_user(int type=2){
    /*
Функция вызывается для генерации принудительного перехода хода при ошибке
на текущий момент есть следующие типы перехода хода:
type
2 Неправильные координаты // error_number(1,"change() incorrect coordinate ");
3 нет совпадений после хода // error_number(2," no matches found ");
4 вышел таймер

*/
#if defined  Test_serv
    printf( "\n alarm_active_turn_user type %d count_alarm[0] %d  count_alarm[1] %d " , type,count_alarm[0],count_alarm[1]);
 #endif
    count_alarm[user_active_turn]++;
    int next_turn=1;
 if(user_active_turn==1) { next_turn=0; }
 not_change_turn_bonus=0; // это чтобы доп ход не наследовался при неправильном ходе
 send_active_turn_type=type;
 user_active_turn=next_turn;
#if defined  Test_serv
 printf( "\n next user: %d count_alarm[0] %d  count_alarm[1] %d " , user_active_turn,count_alarm[0],count_alarm[1]);
 //   printf( "\n alarm_active_turn_user type %d count_alarm %d  user_active_turn %d " , type,count_alarm[user_active_turn],user_active_turn);
 #endif

 return IF_TURN_CHANGE_SLEEP;
}
// сменить ход на следующего игрока
int Match_Map::change_active_turn_user(){

    if(not_change_turn>0){
        not_change_turn=0;
        // эффект набранный фишек
         send_active_turn_type=0; //стандартная причина не переносить ход (правила)
        return 0;
          }
    if(not_change_turn_bonus>0){
        // эффект бонуса доп ход
        not_change_turn_bonus=0;
        send_active_turn_type=5;
        return 0;
          }
int last=user_active_turn;
 int next_turn=1;
 if(user_active_turn==1) { next_turn=0; }
 user_active_turn=next_turn;
 send_active_turn_type=1;
 if(last!=user_active_turn) return IF_TURN_CHANGE_SLEEP; // ожидание при смене хода
 return 0; // ожидание если ход не сменился
}
 // пользователь на которого срабатывают первые три эффекта
int Match_Map::get_noactive_user(){
    if(user_active_turn==1)return 0;
    return 1;
}
 // изменяет указаный параметр игрока
void Match_Map::change_perk(int user,int num_perk,int value){
    user_perk[user][num_perk]+=value;
    valid_perk(user,num_perk);
    return;
}
// установить параметр игрока
void Match_Map::set_perk(int user,int num_perk,int value){

    user_perk[user][num_perk]=value;
    valid_perk(user,num_perk);
}
// проверка допустимости значения параметра
void Match_Map::valid_perk(int user,int num_perk){
    if((num_perk<=2)&&(user_perk[user][num_perk]> max_user_perk[user][num_perk])){
        user_perk[user][num_perk] = max_user_perk[user][num_perk];
    }

    if(user_perk[user][num_perk]<0){
        user_perk[user][num_perk]=0;
        return;
    }
    if((num_perk>2)&&(user_perk[user][num_perk]> max_user_perk[user][num_perk])){
        user_perk[user][num_perk]= max_user_perk[user][num_perk];
    }
#if defined  Test_serv

    if((num_perk==4)&&
            (user_perk[user][num_perk] == max_user_perk[user][num_perk]))
        printf( "\n Крит!!! Пользователь %d  %d] " , user ,user_perk[user][num_perk]);
        //     printf( "\n  MAPX [i k] [%d,%d] == %d" ,  MAPX-1, MAPY-1,(abs(i1 - i2) + abs(k1 - k2)));
 #endif
}
int Match_Map::double_dp(int user){

    if(user_flag[user][1]>0){
        int ttt=user_flag[user][1];
    user_flag[user][1]=0;
    set_perk(user,4,0); // сбрасываем перк двойного урона в 0
#if defined  Test_serv
    printf( "\n Крит!! user %d множитель %d   " , user ,ttt);
 #endif
    return ttt;
    }
        return 1;
}
  //применение эффекта параметров проверили и применили эффект
void Match_Map::apply_perk_effect (int user,int num_perk){
  // данный флаг следует устанавливать только при аптечке и  user_perk_change[user]=1;
    // при разных бонусах

    if(num_perk<3){
    // проверка нет ли завершения битвы
        int sum=user_perk[user][0]+user_perk[user][1]+user_perk[user][2];

        if(sum<=0){
        win_user=user_active_turn;
        }
        return;
    }
    // пехота, бронетехника, авиация,
    // аптека, дв. урон, защита

    if(user_perk[user][num_perk]>=  max_user_perk[user][num_perk]){
        switch (num_perk) {

        case 3:
             { //аптечка
            user_perk_change[user]=2;
            change_perk(user,0,value_effect_perk[num_perk]);
            change_perk(user,1,value_effect_perk[num_perk]);
            change_perk(user,2,value_effect_perk[num_perk]);
            set_perk(user,num_perk,0);
#if defined  Test_serv
    printf( "\n Аптечка!! user %d flag %d  " , user ,user_perk_change[user]);
 #endif
             } break;

        case 4:
              { // двойной урон на текущем ходе он не применяется
            // он будет применятся только после дестроя первого раунда
      //  line60_10":{"f_5_2":4,"f_5_3":4,"f_5_4":4}}, .
      //   {"line125_30":{"f_5_5":0,"f_5_6":0,"f_5_7":0}}]},
      // Отключить установку флага при действии крита
            if(user_flag[user][1]==0)
        user_flag[user][2]=value_effect_perk[num_perk];
#if defined  Test_serv
    printf( "\n Крит!! user %d flag %d  " , user ,user_flag[user][2]);
 #endif

                 } break;
        default:
                { //блокировка урона
               //   user_perk_change[user]=1;
                  user_flag[user][0]=value_effect_perk[num_perk];
                  }

             } // end switch (number)


    }



}
int Match_Map::get_val_choice(int num_choice){

    switch (num_choice) {

    case 0: case 1: case 2:
    {   int object=get_noactive_user(); //тот на кого применяют негативный эффект

         return user_perk[object][num_choice];

    } break;

    case 3:case 4: case 5:
    {
        int subject =user_active_turn;  // тот на кого применяют позитивный эффект
        return user_perk[subject ][num_choice];
    } break;

    default:
            { // todo log it

              }

} // end switch (number)
return 0;
}



// изменения параметров игрока по результатам хода
int Match_Map::apply_effect_choice(int num_choice,int count_choice,int add_five){

    if(count_choice<3) {
    // 0 нормально а вот 2 1 что за нах
        return 0;
    }
    int dp=0;
            //count_choice* value_effect_choice[num_choice]*double_dp(user_active_turn);

    if(count_choice==3) {
        dp=20;
        } else if(count_choice>3){
            dp=20+(count_choice-3)*15;
        }


      if(add_five==1) dp=dp+25;
      if(add_five>1) dp=dp+20;


    switch (num_choice) {
 /*
пехота, бронетехника, авиация, аптека, дв. урон, защита
    0 уменьшает противника x3 Пехота
    1 уменьшает противника x2 Бронетанки,
    2 уменьшает противника x1 Авиация,

    3 увеличивает y2 аптечка,
    4 увеличивает y3 двойной урон
    5 увеличивает y1 блокировка урона,
Минимальное количество урона наноситься уничтожением трех фишек
и составляет 10 единиц урона. За каждую дополнительную фишку начисляется
+5 к урону, т.е. за 4 одновременно убранные фишки начисляется 15 единиц урона,
 за 5- соответственно 20 единиц урона.
Кроме того дополнительно начисляются единицы урона за повторно совпавшие комбинации,
за каждую дополнительно возникшую комбинацию начисляется +5 единиц урона (без накопления).
    */
    case 0: case 1: case 2:
    {   int object=get_noactive_user(); //тот на кого применяют негативный эффект
       dp=dp*double_dp(user_active_turn);

       if(user_flag[object][0]==1) // если есть щит то урона нет
           { user_flag[object][0]=0;
               // сбросить щит в 0
               set_perk(object,5,0);// сбрасываем перк щита в 0
               return 0; }

            dp=-dp;
         change_perk(object,num_choice,dp);   // изменили параметр
        apply_perk_effect (object,num_choice); // проверили и применили эффект

    } break; //  эффект

    case 3:case 4: case 5:
    {
        int subject =user_active_turn;  // тот на кого применяют позитивный эффект
        change_perk(subject,num_choice,dp);
        apply_perk_effect (subject,num_choice);
    } break; //   эффект

    default:
            { // todo log it

              }

} // end switch (number)
return dp;
}
void Utilits4Maps::ready_for_turn(){
   next_id=1;
  flag_match3_more=0;
  if(all_destroy_f.tellp()!=0)  all_destroy_f.str("");
  error_flag=0;
  user_perk_change[0]=0;
  user_perk_change[1]=0;
  new_f_after_drop.str("");
  // результаты хода брать с карты
   now_move_user=0;
   type_move_user=1;
   send_interval=0; //по умолчанию перехода хода не было
}
void Utilits4Maps::new_battle(Match_Map *apply, int user0x0,    int user0x1,    int user0x2,
                int user1x0,    int user1x1,    int user1x2)
{
// apply->is_free=1; // карта занята она помечена как занята еще на этапе lock_map(0);
int user_num=0;

if(apply->max_user_perk[0][0]!=user0x0)apply->max_user_perk[0][0]=user0x0; // пехота
if(apply->max_user_perk[0][1]!=user0x1)apply->max_user_perk[0][1]=user0x1; // бронетехника
if(apply->max_user_perk[0][2]!=user0x2)apply->max_user_perk[0][2]=user0x2; // авиация
if(apply->max_user_perk[1][0]!=user1x0)apply->max_user_perk[1][0]=user1x0; // пехота
if(apply->max_user_perk[1][1]!=user1x1)apply->max_user_perk[1][1]=user1x1; // бронетехника
if(apply->max_user_perk[1][2]!=user1x2)apply->max_user_perk[1][2]=user1x2; // авиация
apply->set_perk(user_num,0,user0x0);
apply->set_perk(user_num,1,user0x1);
apply->set_perk(user_num,2,user0x2);
apply->set_perk(user_num,3,0      );
apply->set_perk(user_num,4,0      );
apply->set_perk(user_num,5,0      );

user_num=1;
apply->set_perk(user_num,0,user1x0);
apply->set_perk(user_num,1,user1x1);
apply->set_perk(user_num,2,user1x2);
apply->set_perk(user_num,3,0      );
apply->set_perk(user_num,4,0      );
apply->set_perk(user_num,5,0      );
apply->se_pan_b = 0;
apply->win_user=3;
// ходит всегда первый игрок
apply->user_active_turn=0;
//%% 1 ход сменился в результате снартных правил игры (начало боя, уничтожение 4 фишек, использование )
apply->send_active_turn_type=1;
// тип перехода хода брать с карты
now_move_user=0;
type_move_user=1;
send_interval=6;
// кол-во замечаний у игрока
apply->count_alarm[0]=0;
apply->count_alarm[1]=0;
apply->not_change_turn_bonus=0;
for (int i=0; i<2; i++)
    for (int j=0; j<3; j++){
   apply->user_flag[i][j]=0;
    }
// состояние перков у игроков
 //"panel":{"tx0":"0/100","tx1":"0/100","tx2":"0/100","tx3":"0/100","tx4":"0/100","tx5":"0/100"}
get_perk_status(apply);
}

int Utilits4Maps::cap_swap(int i1, int k1, int i2, int k2)
{
#if defined  Test_serv

        //     printf( "\n cap_swap [i k] [%d,%d]:  [%d,%d] " , i1,k1,i2,k2);
        //     printf( "\n  MAPX [i k] [%d,%d] == %d" ,  MAPX-1, MAPY-1,(abs(i1 - i2) + abs(k1 - k2)));
 #endif
        //check if all inputs are valid
        if(i1 < 0) return 0;
        if(i1 > MAPX-1) return 0;
        if(k1 < 0) return 0;
        if(k1 > MAPY-1) return 0;
        if(i2 < 0) return 0;
        if(i2 > MAPX-1) return 0;
        if(k2 < 0) return 0;
        if(k2 > MAPY-1) return 0;

        //only one of the directions can have a difference of 1
        if(abs(i1 - i2) + abs(k1 - k2) == 1)
                return 1;
        else
                return 0;
}

void Utilits4Maps::swap_board(Match_Map *apply,int i1, int k1, int i2, int k2)
{
        int temp_value;

        temp_value = apply->board[i1][k1].choice_type;
        apply->board[i1][k1].choice_type=apply->board[i2][k2].choice_type;
        apply->board[i2][k2].choice_type = temp_value;
}

int Utilits4Maps::find_all_matches(Match_Map *apply)
{
        int i,k, ammount, streak,gstreak;
        gstreak=0;
        streak =0;
        // надо вернуть сколько было всего типов совпадений

// требуется пройти всю карту пометить все совпадения
// и уже после этого все помеченный совпадения уничтожить по очереди



int drop_cicle=0;

        while(1)
        {
              gstreak+=streak;
              streak = 0;
            for(k=0;k<8;k++)
                for(i=0;i<8;i++) // сверху вниз слева на право
                {
/*

{"destroy":[ {"line10":{"f_5_0":3,"f_5_1":3,"f_5_2":3,"f_5_3":3,"f_5_4":3}},{"line15":{"f_3_1":1,"f_4_1":1,"f_5_1":1}}]},
{"new_f":{"f_5_4":5,"f_5_3":5,"f_5_2":4,"f_5_1":1,"f_5_0":3}},
{"destroy":[ ]},
{"new_f":{"f_3_0":2,"f_4_0":5,"f_5_0":3}},

*/

                      // пробегаем всю карту помечаем все ячейки на удаление
                      // узнаем сколько вообще может или должно исчезнуть
                    ammount = has_vertical_match(apply,i,k,true);

                    #if defined  Test_serv
                  //  printf( "\n find_all_matches [i k] [%d,%d]: %d " , i,k,ammount);
                    #endif
                        if(ammount>0)
                        {
                                streak++;
                        }
                    ammount = has_horizontal_match(apply,i,k,true);

                        if(ammount>0)
                        {                             
                                streak++;
                        }

                } // пометили все совпадения теперь их удаляем

                //  теперь дропаем удаленные фишки
                if(streak>0) {

                    drop_all(apply,drop_cicle);
                    drop_cicle++;
                    apply->actual_double_effect();
                }


              if(streak==0)  //цикл не рвется пока не обсмотрим всю карту без того чтоб reset = 1;
                        break;

                //do this because we have to wait

        }

        return gstreak;
}
// я так понял это проверка что есть ходы
int Utilits4Maps::could_have_match(Match_Map *apply,int i, int k)
{

int proto=apply->get_type(i,k);
 //handle all the special cases first
                 //extended I check
                 if(i > 2) if(proto == apply->get_type(i-2,k)
                              && proto == apply->get_type(i-3,k)) return 1;
                 if(i < 5)  if(proto == apply->get_type(i+2,k)
                               && proto == apply->get_type(i+3,k)) return 1;
        //the L checks

        if(k < 7)
         {
        if(i < 6) if(proto == apply->get_type(i+1,k+1) && proto == apply->get_type(i+2,k+1)) return 1;
        if(i > 1) if(proto == apply->get_type(i-1,k+1) && proto == apply->get_type(i-2,k+1)) return 1;
        //V checks
        if(i < 7 && i-1 > -1) if(proto == apply->get_type(i+1,k+1) && proto == apply->get_type(i-1,k+1)) return 1;
        if(i < 7 && k-1 > -1) if(proto == apply->get_type(i+1,k+1) && proto == apply->get_type(i+1,k-1)) return 1;
        if(i-1 > -1 && k-1 > -1) if(proto == apply->get_type(i-1,k-1) && proto == apply->get_type(i-1,k+1)) return 1;

        if(k < 6)
         {
               if(i < 7) if(proto == apply->get_type(i+1,k+1) && proto == apply->get_type(i+1,k+2)) return 1;
               if(i > 0) if(proto == apply->get_type(i-1,k+1) && proto == apply->get_type(i-1,k+2)) return 1;
               //extended I check
                       if(k < 5)  if(proto == apply->get_type(i,k+2) && proto == apply->get_type(i,k+3)) return 1;
         }
        }



         if(k > 0)
          {
        if(i > 1) if(proto == apply->get_type(i-1,k-1) && proto == apply->get_type(i-2,k-1)) return 1;
        if(i < 6) if(proto == apply->get_type(i+1,k-1) && proto == apply->get_type(i+2,k-1)) return 1;
        //V checks
        if(i-1 > -1 && i < 7) if(proto == apply->get_type(i-1,k-1) && proto == apply->get_type(i+1,k-1)) return 1;
             if(k > 1)
                  {
                   if(i > 0) if(proto == apply->get_type(i-1,k-1) && proto == apply->get_type(i-1,k-2)) return 1;
                   if(i < 7) if(proto == apply->get_type(i+1,k-1) && proto == apply->get_type(i+1,k-2)) return 1;
                   if(k > 2)  if(proto == apply->get_type(i,k-2) && proto == apply->get_type(i,k-3)) return 1;
                  }
          }

        return 0;
}
int Utilits4Maps::find_hint(Match_Map *apply,int *i, int *k)
{
        for(*i=0;*i<8;(*i)++)
                for(*k=0;*k<8;(*k)++)
                        if(could_have_match(apply,*i,*k))
                                return 1;

        return 0;
}
int Utilits4Maps::any_matches(Match_Map *apply)
{
        for(int i=0;i<8;i++)
                for(int k=0;k<8;k++){
                    if(is_part_of_match(apply,i,k))
                                return 1;
                }

        return 0;
}
void Utilits4Maps::clear_out_matches(Match_Map *apply)
{
        int i,k,z, ammount;
        int reset;

        //init this
        reset = 0;

        while(1)
        {
                reset =0;

                for(i=0;i<8 && !reset;i++)
                        for(k=0;k<8 && !reset;k++)
                {
                    ammount = has_vertical_match(apply,i,k,false);
                    #if defined  Test_serv
                 //   printf( "\n clear_out_matches [i k] [%d,%d]: %d " , i,k,ammount);
                     #endif
                    if(ammount>0)
                        {
                                drop_row(apply,i,k+(ammount-1),ammount,0);

                                reset = 1;
                                break;
                        }

                 ammount = has_horizontal_match(apply,i,k,false);
                            if(ammount>0)
                        {
                                for(z=0;z<ammount;z++)
                                        drop_row(apply,i+z,k,1,0);

                                reset = 1;
                                break;
                        }
                            if((i==7)&&(k==7))reset=2;
                }

               if(reset==2)break;
        }
}
// функция определяет наиболее простые совпадения
// то есть положительный результат просто гарантирует что какой то минимальный линии для удаления
// есть а более сложные случаи надо разбирать отдельно
int Utilits4Maps::is_part_of_match(Match_Map *apply,  int i,   int k)
{
    #if defined  Test_serv
    //             printf( "\n is_part_of_match %d %d  \n", i,k);
    #endif
    int proto=apply->get_type(i,k);

    // k: строки
        // MAPX_Y == 8
    int max_x_y=MAPX_Y;

        //vert
        if(k+2 < max_x_y)             if(proto == apply->get_type(i,k+1)
                                         && proto == apply->get_type(i,k+2)) return 1;
        if(k+1 < max_x_y && k-1 > -1) if(proto == apply->get_type(i,k-1)
                                         && proto == apply->get_type(i,k+1)) return 1;
        if(k-2 > -1) 		if(proto == apply->get_type(i,k-2)
                                   && proto == apply->get_type(i,k-1)) return 1;

        //horizontal
        if(i+2 < max_x_y) 	     if(proto == apply->get_type(i+1,k)
                                        && proto == apply->get_type(i+2,k)) return 1;
        if(i+1 < max_x_y && i-1 > -1) if(proto == apply->get_type(i-1,k)
                                         && proto == apply->get_type(i+1,k)) return 1;
        if(i-2 > -1) 		     if(proto == apply->get_type(i-2,k)
                                        && proto == apply->get_type(i-1,k)) return 1;


#if defined  Test_serv

            // printf( "\n return 0 %d   \n",apply->get_type(i,k));
 #endif
        return 0;
}



int Utilits4Maps::has_vertical_match(Match_Map *apply,int i, int k,bool make_str)
{
    if((k)>MAPY-3) {
        return 0; //три не получиться ибо ниже нет трех фишек
    }
    // значит k k+1 k+2    <=7  ;  то есть  k<=5

int max_z=MAPY-k; // 8-k
int z;
int count_z=0;
int proto=apply->get_type(i,k);
int segment_id = apply->get_sector_id(i,k);


#if defined  Test_serv
//printf( "\n vert [i k] [%d,%d]: %d " , i,k,apply->get_type(i,k));
//printf( "\n max_z %d " ,max_z);
#endif
    for(z=0;z<max_z;z++) {
          int z_add_k=k+z;

            if(proto==apply->get_type(i,z_add_k)) {
                count_z++;

                if( apply->get_sector_id(i,z_add_k)){
                   segment_id = apply->get_sector_id(i,z_add_k);
                }

            } else {
            break; // прерываем цикл
            }

    #if defined  Test_serv
    //       printf( "\n z %d %d %d ",z_add_k, count_z,segment_id);
    #endif
        }

        if((count_z >= 3)){
            // вертикальное совпадение больше 3х
        if (make_str){
            if( segment_id==0) segment_id=next_id++;

            for(z=0;z<count_z;z++) {
                  int z_add_k=k+z;
                  // пометили все сектора на уничтожение
                  apply->set_sector_id(i,z_add_k, segment_id);
        #if defined  Test_serv
       // printf( "\n v set %d %d %d ",i, z_add_k,segment_id);
        #endif
            }
            }
    #if defined  Test_serv
    //    printf( "\n end %d %d  ", count_z,z);
    #endif
               return count_z;
        }


        return 0;


}

int Utilits4Maps::has_horizontal_match(Match_Map *apply,int i, int k,bool make_str)
{
    if(i>MAPX-3) {
      return 0; //три не получиться ибо ПРАВЕЕ нет трех фишек
    }
    // значит i i+1 i+2    <=7  ;  то есть  i<=5

  int max_z=MAPX-i;
  int z;
  int count_z=0;
  int proto=apply->get_type(i,k);
  int segment_id = apply->get_sector_id(i,k);

  for(z=0;z<max_z;z++) {
       int z_add_i=i+z;

       if(proto ==apply->get_type(z_add_i,k) ) {
              count_z++;

              if( apply->get_sector_id(z_add_i,k)){
                 segment_id = apply->get_sector_id(z_add_i,k);
              }

          } else {
          break; // прерываем цикл
          }

     #if defined  Test_serv
        //  printf( "\n z %d %d %d %d",z_add_k, count_z,z,z_add_k);
    #endif
      }

      if((count_z >= 3)){
          if (make_str){
          if( segment_id==0) segment_id=next_id++;
          for(z=0;z<count_z;z++) {
                int z_add_i=i+z;
                // пометили все сектора на уничтожение
                apply->set_sector_id(z_add_i,k, segment_id);
            #if defined  Test_serv
          //  printf( "\n h set %d %d %d ",z_add_i, k,segment_id);
                #endif
          }
            }

#if defined  Test_serv
//    printf( "\n end %d %d  ", count_z,z);
#endif
             return count_z;
      }


      return 0;

}
// генерируем
int Utilits4Maps::get_chois(Match_Map *apply,int i, int k){
/*
вероятность выпадения определенной фишки 1/6    face_choice_max=6;
вероятность выпадения двух определенных фишек 1/36 то есть 2.7%

четырех 1/216           0.077 %

вероятность выпадания любых одинаковых фишек 4 раза подряд 0,462 %
6/216 = 1/36

задача состояит в том чтобы уменьшить вероятность выпадения вдвое
1/72

тогда вероятность выпадения 4 фишек определенного вида должна быть
1/432

или свести вероятность выпадения фишки такой же как соседней до
2/15     13%

в начале было 1/6     16.6%

Итого получаем
требуется с вероятностью  13% выдавать совпадающее с низом или правой стороной
в остальных случаях выдавать не равным

*/
   std::vector< int > face_choice; // набор фишек который мы сгенерируем
   int res = (unsigned int) rand() % 100; // 0 ... 99  [1%]

    if(
      (res>=0)&&(res<28)
    )
    { // совпадающий либо с правым либо с левым либо с низом
        if(i-1 >= 0){
        //слева
        if(apply->board[i-1][k].choice_type<face_choice_max)face_choice.push_back(  apply->board[i-1][k].choice_type);
        }
        if((i+1 < MAPX)){
        // справа
        if(apply->board[i+1][k].choice_type<face_choice_max)
            face_choice.push_back(apply->board[i+1][k].choice_type);
        }
        if(k+1 < MAPY){
        // снизу
        if(apply->board[i][k+1].choice_type<face_choice_max)
            face_choice.push_back(apply->board[i][k+1].choice_type);
        }


    }

    if(face_choice.empty()) {
        std::set < int > myset;
        std::set < int >::iterator it;
        // все кроме левого правого или низа
        if(i-1 >= 0){
        //слева
        myset.insert(  apply->board[i-1][k].choice_type);
        }
        if((i+1 < MAPX)){
        // справа
        myset.insert(apply->board[i+1][k].choice_type);
        }
        if(k+1 < MAPY){
        // снизу
            myset.insert(apply->board[i][k+1].choice_type);
        }
        for(int t=0;t<face_choice_max;t++){
        it=myset.find(t);
        if(it==myset.end()){
        face_choice.push_back(t);
        }
        }


    }
    int result = (unsigned int) rand() % face_choice.size(); //
    return face_choice[result];

}
// ходим по всей карте и уничтожаем все клетки помеченные на  уничтожение
void Utilits4Maps::drop_all(Match_Map *apply,int drop_cicle)
{
/*
{"destroy":[ {"line10":{"f_5_0":3,"f_5_1":3,"f_5_2":3,"f_5_3":3,"f_5_4":3}},{"line15":{"f_3_1":1,"f_4_1":1,"f_5_1":1}}]},
{"new_f":{"f_5_4":5,"f_5_3":5,"f_5_2":4,"f_5_1":1,"f_5_0":3}},
...
*/
#if defined  Test_serv
     //        printf( "drop all begin \n" );
 #endif
    CSegment all_segment[65]; // 1 .. 64

    for(int k=0;k<8;k++)
        for(int i=0;i<8;i++) // сверху вниз слева на право
    {
        int seck=apply->get_sector_id(i,k);
        if((seck>0)&&(seck<=64)){
            // определяем насколько она глубока
            int amount=has_vertical_match(apply, i, k,false);
            if(amount==0)amount=1;
           // точки на уничтожение
            for(int z=0;z<amount;z++) {
                  int z_add_k=k+z;
                    //  запомнили фишку и сектор уничтожаемых фишек
                #if defined  Test_serv
            //    printf( "\n v sector set %d %d %d ",i, z_add_k,amount);
                #endif

                 int seck0=apply->get_sector_id(i, z_add_k);
                  all_segment[seck0].set_segment(apply->get_type(i,z_add_k) , i, z_add_k);
                  apply->set_sector_id(i,z_add_k, 0);
            }

            drop_row(apply,i,k+amount-1,amount,1);

        }

        if(seck>64){
        printf( "\n New [i z] [%d,%d]: %d %d " , i,k,apply->board[i][k].choice_type,seck);
        }

    }
    // на текущую точку собрали создали все новые фишки и посчитали все уничтоженные фишки
    // собираем в кучу результаты
    if(all_destroy_f.tellp()!=0)all_destroy_f<<',';

    all_destroy_f<<"{\"destroy\":[ ";
    int pr_moreline=0;

    for (int j=1;j<65;j++){
        // я так понимаю пробегаем все ячейки с целью поиска уничтожаемых сегментов
        if(all_segment[j].is_free==0){
            if(pr_moreline==1){
                 all_destroy_f<<",";
               }
            pr_moreline=1;
            int Perk_undo=apply->get_val_choice(all_segment[j].choice_type);
             int demm=abs(apply->apply_effect_choice(all_segment[j].choice_type,all_segment[j].count_choice,drop_cicle));
         all_destroy_f<<" {\"line"<<Perk_undo<<"_"<<demm<<"\":{" <<all_segment[j].in_line_str.str()<<"}}";
#if defined  Test_serv
         std::ostringstream d[6];
         d[0]<<"Пехот @";
         d[1]<<"Танки #";
         d[2]<<"Авиац $";
         d[3]<<"Помощ +";
         d[4]<<"Криту &";
         d[5]<<"Щит.. *";

       printf( " %s [  %d -> %d ] \t",d[all_segment[j].choice_type].str().c_str()
               ,all_segment[j].count_choice, demm);

 #endif
         if(all_segment[j].count_choice>3)flag_match3_more=1;
        }
    }
    all_destroy_f<< "]},";
    // Проверяем  сменились ли состояние панелей игрока
    // ТО есть послесписка фишек на дестроя выдаем панели игрока


    int m_1=apply->se_pan_b % 3 + 1;  // 1  3

    bool Panels=false;

    if(apply->se_pan_b!=0) {
           if(drop_cicle>0){
               m_1=3;
               Panels = false;
             } else {
               apply->se_pan_b++;
            }
    } else {
     m_1=0; apply->se_pan_b++;
    }


    if((apply->user_perk_change[0]>0)
            ||(apply->user_perk_change[1]>0)||(Panels)){


        all_destroy_f<<"{\"panels\":{ ";

        if((apply->user_perk_change[0]>0)||(m_1==1)||(m_1==0))
        {
         int pr=apply->user_perk_change[0];
         apply->user_perk_change[0]=0;
         int i=0;
         all_destroy_f<<"\"panel0\":{";
         for (int j=0; j<6; j++){
            if((j==3)&&(pr==2)) continue; // игнор панельки с аптечкой при срабатывании аптечки
        if(j>0) all_destroy_f<<",";
        all_destroy_f<<"\"tx"<<j<<"\":\""<<apply->user_perk[i][j]<<"/"<<apply->max_user_perk[i][j]<<"\"";
        }
        all_destroy_f<<",\"fc"<<6<<"\":"<<apply->count_alarm[i]<<"";
        all_destroy_f<<"}";
         if((apply->user_perk_change[1]>0)||(m_1==2)||(m_1==0)){ all_destroy_f<<" , "; }
    }

  if((apply->user_perk_change[1]>0)||(m_1==2)||(m_1==0)){
         int pr=apply->user_perk_change[1];
           apply->user_perk_change[1]=0;
   int i=1;
   all_destroy_f<<"\"panel1\":{";
   for (int j=0; j<6; j++){
       if((j==3)&&(pr==2)) continue; // игнор панельки с аптечкой при срабатывании аптечки
       if(j>0) all_destroy_f<<",";
       all_destroy_f<<"\"tx"<<j<<"\":\""<<apply->user_perk[i][j]<<"/"<<apply->max_user_perk[i][j]<<"\"";
                      }
        all_destroy_f<<",\"fc"<<6<<"\":"<<apply->count_alarm[i]<<"";
          all_destroy_f<<"}";
          }
    all_destroy_f<< "}},";
    } else {
// оповещение о числе пропущенных ходов через одну попеременно с одним пропуском
        if((m_1!=3)){
        all_destroy_f<<"{\"panels\":{ ";
        int i=0;
        if((m_1==1)||(m_1==0))
        {
        all_destroy_f<<"\"panel0\":{";
        all_destroy_f<<"\"fc"<<6<<"\":"<<apply->count_alarm[i]<<"";
        all_destroy_f<<"}";
        if(m_1==0){ all_destroy_f<<" , "; }
        }

  if((m_1==2)||(m_1==0)){
       int i=1;
       all_destroy_f<<"\"panel1\":{";
       all_destroy_f<<"\"fc"<<6<<"\":"<<apply->count_alarm[i]<<"";
       all_destroy_f<<"}";
          }

    all_destroy_f<< "}},";
        }
    }


    //создаем новые фишки
    for(int k=0;k<8;k++)
        for(int i=0;i<8;i++) // сверху вниз слева на право
    {
        if(apply->board[i][k].choice_type>=face_choice_max){
            // точка анализа карты для генерации фишки во время боя
            int new_ch=get_chois(apply,i, k);
            apply->board[i][k].choice_type =new_ch;
           if(new_f_after_drop.tellp()!=0)new_f_after_drop<<',';
            new_f_after_drop<<"\"f_"<<(i)<<"_"<<(k)<<"\":"<< new_ch;

            }
    }
    all_destroy_f <<"{\"new_f\":{"<<new_f_after_drop.str()<<"}}";

    new_f_after_drop.str("");

}

void Utilits4Maps::drop_row(Match_Map *apply,int i, int to, int ammount,int make_str )
{
    /*
Суть функции начиная со строчки в которой находиться уничтожаемая фишка поднятся до самого верха
смещаяя фишки со смещением ammount (кол-во уничтожаемых фишек в ряду)
после того как до верха меньше чем ammount  строк начать генерировать новые фишки
*/

        for(int z=to;z>=0;z--)
        {
             if(z - ammount >= 0) //we got something to take in?
                    // стащили вниз фишки выше списка на удаление
                    apply->board[i][z].choice_type=apply->board[i][z - ammount].choice_type;
                else
                     {


                         if(make_str==1) {
                            //специальный режим при битвах
                         apply->board[i][z].choice_type= face_choice_max+i+z*8;
                         // при этом режиме ячейки для генерации новой фишки помечаются
                         // а затем  в конце режима drop all или random вместо них генерируются
                         // нужные фишки сделано для исправления бага с с несколькими вертикальными рядами фишек
                           } else {
                             int new_ch=get_chois(apply,i, z);
                              apply->board[i][z].choice_type =new_ch;
                         if(make_str==2) {
                         if(new_f_after_drop.tellp()!=0)new_f_after_drop<<',';
                          new_f_after_drop<<"\"f_"<<(i)<<"_"<<(z)<<"\":"<< new_ch;

                          }
                         }
                     }

        }
      //  printf( "\n " );
}

void Utilits4Maps::setup_random_map(Match_Map *apply)
{
        int i,k;

        for(i=0;i<8;i++)
                for(k=0;k<8;k++){
                        apply->board[i][k].choice_type = rand() % face_choice_max;
                        apply->board[i][k].for_drop_id=0;
                }

        //now...

        clear_out_matches(apply);

        int i_hint,k_hint;

        while(!find_hint(apply,&i_hint,&k_hint)) {
            for(i=0;i<8;i++)
                    for(k=0;k<8;k++){
                        // не эффективно конечно но лень думать как отпимальнее

                            apply->board[i][k].choice_type = rand() % face_choice_max;
                    }


            clear_out_matches(apply);
        }

}
void Utilits4Maps::set_win_status(Match_Map *apply){
    // тип перехода хода  после осуществления хода
    now_move_user=apply->user_active_turn;

    type_move_user=apply->send_active_turn_type;
    //send_interval =  сделано ручками при ходе так как в остальных случаях ожидание не нужно

    // статус победы
    win_user=apply->win_user;

    if(apply->count_alarm[0] > 4){
    win_user=1;
    }

    if(apply->count_alarm[1] > 4){
    win_user=0;
    }
  //  apply->se_pan_b++;
}

void Utilits4Maps::get_perk_status(Match_Map *apply){
//"panel":{"tx0":"0/100","tx1":"0/100","tx2":"0/100","tx3":"0/100","tx4":"0/100","tx5":"0/100"}

    set_win_status(apply); //разнесено ибо в некоторых случаях параметр перков не нужен

    user_perk_change[0]=apply->user_perk_change[0];
    user_perk_change[1]=apply->user_perk_change[1];
    int pr=user_perk_change[0];
    int pr1=user_perk_change[1];

    apply->user_perk_change[1]=0;
    apply->user_perk_change[0]=0;




    int i=0;
    for (int j=0; j<6; j++){

        user_panel[i][j].str("");
        if((j==3)&&(pr==2)) continue;
        user_panel[i][j]<<apply->user_perk[i][j]<<"/"<<apply->max_user_perk[i][j];

    }
   i=1;
   for (int j=0; j<6; j++){
       user_panel[i][j].str("");
         if((j==3)&&(pr1==2)) continue;
        user_panel[i][j]<<apply->user_perk[i][j]<<"/"<<apply->max_user_perk[i][j];
                      }
  return;

}


void Utilits4Maps::Print(Match_Map *apply){
    int i,k;


#if defined  Test_serv
             // распечатать карту
     printf( "\n num " );
             for(i=0;i<8;i++) {
                  printf( "  %d",i );
             } printf( "\n" );

#endif






        //std::string out_destr;
            std::ostringstream out_destr;
            for(k=0;k<8;k++) { // k строки
                #if defined  Test_serv
                printf( "\nk:%d ",k );
                #endif
                    for(i=0;i<8;i++) { // i строки но для моей задачи это столбцы!

            #if defined  Test_serv
                        printf("  ");
                        if(apply->get_type(i,k)==0) printf( "@");
                        if(apply->get_type(i,k)==1) printf( "#");
                        if(apply->get_type(i,k)==2) printf( "$");
                        if(apply->get_type(i,k)==3) printf( "+");
                        if(apply->get_type(i,k)==4) printf( "&");
                        if(apply->get_type(i,k)==5) printf( "*");
                        if(apply->get_type(i,k)==6) printf( "X");
                #endif

                 if(out_destr.tellp()!=0)out_destr<<',';
                 out_destr<<"\"f_"<<i<<"_"<<(k)<<"\":"<<(apply->get_type(i,k));
                    }
            }
#if defined  Test_serv
           //  printf( "\n" );
           //   get_perk_status(apply);
          //    return;
 #endif

             if(all_destroy_f.tellp()!=0)all_destroy_f<<',';

             all_destroy_f<<"{\"map\":{"<<out_destr.str()<<"}}";
     // это вызывается в начале битвы и обеспечивает игрока данными о панелях противника
              get_perk_status(apply);

}
void Utilits4Maps::boom(Match_Map *apply, int i1, int k1){
/*

  0 крест
  1 запустить поиск совпадений
  2 отдать результаты (в качестве дестроя {"destroy":[  {"line0":{"f_3_5":1,"f_3_6":1,"f_3_7":1}}]},)
*/



    if((i1 < 0) ||(i1 > MAPX-1)||(k1 < 0)||(k1 > MAPY-1)){
        // ошибка
        error_number(1,"boom() coordinate error");
        return;
    }
    ready_for_turn();
    std::ostringstream out_destr;


    for(int z=0;z<MAPX;z++)
    {

          if(i1==z){

                 // фактически загоняем в уничтоженные все фишки из вертикали
                  for(int i=0;i<MAPY;i++)  {
                      if(out_destr.tellp()!=0)out_destr<<',';
                      out_destr<<"\"f_"<<z<<"_"<<(i)<<"\":"<<apply->board[z][i].choice_type;
                  }

                  // 2 в drop_row позволяет набирать новые фишки в строку новых фишек
                   drop_row(apply,z,7,8,2); // заменили их



          }
          else {
              // 2 в drop_row позволяет набирать новые фишки в строку новых фишек
            drop_row(apply,z,k1,1,2);
            if(out_destr.tellp()!=0)out_destr<<',';
            out_destr<<"\"f_"<<z<<"_"<<(k1)<<"\":"<<apply->board[z][k1].choice_type;
          }
    }




    all_destroy_f<<"{\"destroy\":[ ";
    all_destroy_f<<" {\"slot\":{" ;
    all_destroy_f<<out_destr.str()<<"}}]},";
    all_destroy_f<<"{\"new_f\":{"<<new_f_after_drop.str()<<"}}";
    new_f_after_drop.str("");
    find_all_matches(apply);

    int i_hint,k_hint;
    if(!find_hint(apply,&i_hint,&k_hint))
    {
     re_init_map(apply);
    }
 get_perk_status(apply);
 //так как мы бонус требуется неким костылем поправить результат перехода хода
 // на 6
 type_move_user=6;
 return;
}
 Utilits4Maps::Utilits4Maps(){
 //str_new_destr.reserve(64*3);
// new_after_drop.reserve(64*3);
    flag_match3_more=0;
    next_id=1;
    error_flag=0;
    struct timeval val;
    gettimeofday(&val, NULL);

    // 3 случайно выбрать из доступных ячеек и двигатся туда
    srand((int64_t) val.tv_sec*(int64_t)1000000+(int64_t)val.tv_usec);
 }
void Utilits4Maps::random(Match_Map *apply){
//  выбрать случайные координаты

     int i, j;
    ready_for_turn();

    std::multiset < int > sort_set;

    std::ostringstream out_destr;
    srand(time(NULL));

    // point_id=

    int dup_flag;
    int rand_val;
    static int MAX_RCOUNT=15;
    int parr[MAX_RCOUNT]; // набор случайных 12 координат k*8+i;


    for ( i= 0; i<MAX_RCOUNT; i++ ) {

            for ( ; ; ) {
                    rand_val= rand()%64;
                    dup_flag=0;
                    for ( j= 0; j<i; j++ ) {
                            if ( rand_val == parr[j] ) { dup_flag= 1; break; }
                    }
                    if ( !dup_flag ) { break; }
            }
            parr[i]= rand_val;
#if defined  Test_serv
             printf( "+1+ %d \n" , rand_val );
 #endif
            sort_set.insert(rand_val);
    }
    // имеем случайные координаты по логике они в sort_set упорядочаны по возрастанию
    std::multiset < int >::iterator it= sort_set.begin();
    for(it= sort_set.begin();it!= sort_set.end();it++){
         int i, j;
         i=(*it)%8;
         j=(*it)/8;
         if(out_destr.tellp()!=0)out_destr<<',';
         out_destr<<"\"f_"<<i<<"_"<<(j)<<"\":"<<apply->board[i][j].choice_type;
         drop_row(apply,i,j,1,1); // столбец упал на одну ячейку

    }


    //создаем новые фишки
    for(int k=0;k<8;k++)
        for(int i=0;i<8;i++) // сверху вниз слева на право
    {
        if(apply->board[i][k].choice_type>=face_choice_max){
            // точка анализа карты для генерации фишки во время боя
            //int new_ch=rand() % face_choice_max;
            int new_ch=get_chois(apply,i, k);
            apply->board[i][k].choice_type =new_ch;
           if(new_f_after_drop.tellp()!=0)new_f_after_drop<<',';
            new_f_after_drop<<"\"f_"<<(i)<<"_"<<(k)<<"\":"<< new_ch;

            }
    }

    all_destroy_f<<"{\"destroy\":[ ";

    all_destroy_f<<" {\"slot\":{" ;
    all_destroy_f<<out_destr.str()<<"}}]},";
    all_destroy_f <<"{\"new_f\":{"<<new_f_after_drop.str()<<"}}";
    new_f_after_drop.str("");

    find_all_matches(apply);

    int i_hint,k_hint;
    if(!find_hint(apply,&i_hint,&k_hint))
    {
     re_init_map(apply);
    }


    get_perk_status(apply);

    //так как мы бонус требуется неким костылем поправить результат перехода хода
    // на 6
    //now_move_user=6;
    type_move_user=6;
   return;
}
void Utilits4Maps::end_timeout(Match_Map *apply){
    ready_for_turn();

   // кто ходит  now_move_user=apply->user_active_turn;
    //type_move_user=1; // стандартный переход хода
      int sum1=apply->user_perk[apply->user_active_turn][0]+
              apply->user_perk[apply->user_active_turn][1]+
             apply->user_perk[apply->user_active_turn][2];
    int object=apply->get_noactive_user();
    int sum2=apply->user_perk[object][0]+
             apply->user_perk[object][1]+
             apply->user_perk[object][2];

    if((sum1>sum2)||(sum2==sum1)) {
    win_user=apply->user_active_turn+7;
    } else {
    win_user=object+7;
    }


}
void Utilits4Maps::refresh(Match_Map *apply, int k1){

/*
  0 поменять всю карту
*/

    ready_for_turn();
    std::ostringstream out_destr;


    for(int z=0;z<MAPX;z++)
    {
       // фактически загоняем в уничтоженные все фишки из карты
        for(int i=0;i<MAPY;i++)  {  if(out_destr.tellp()!=0)out_destr<<',';
            out_destr<<"\"f_"<<z<<"_"<<(i)<<"\":"<<apply->board[z][i].choice_type;
        }


    }
   setup_random_map(apply);
   for(int z=0;z<MAPX;z++)
   {
      // фактически загоняем в уничтоженные все фишки из карты
       for(int i=0;i<MAPY;i++)  {
           if(new_f_after_drop.tellp()!=0)new_f_after_drop<<',';
            new_f_after_drop<<"\"f_"<<(z)<<"_"<<(i)<<"\":"<< apply->board[z][i].choice_type;

       }


   }

    all_destroy_f<<"{\"destroy\":[ ";
    all_destroy_f<<" {\"slot\":{" ;
    all_destroy_f<<out_destr.str()<<"}}]},";
    all_destroy_f<<"{\"new_f\":{"<<new_f_after_drop.str()<<"}}";



   /*
     // это не надо ибо   setup_random_map(apply); гарантирует что ходы есть
    new_f_after_drop.str("");
 find_all_matches(apply);
int i_hint,k_hint;
    if(!find_hint(apply,&i_hint,&k_hint))
    {
     re_init_map(apply);
    }*/
 get_perk_status(apply);
 //так как мы бонус требуется неким костылем поправить результат перехода хода
 // на 6
 type_move_user=6;
 return;
}

void Utilits4Maps::error_number(int number,const std::string &info)
{
     all_destroy_f<<"{\"error\":{\"val\":"<<number<<", \"type\":\""<<info<<"\"}}";
//{"error":{"type":"unknown request","val":4}}
    error_flag=number;
     return;
}
void Utilits4Maps::re_init_map(Match_Map *apply){
 setup_random_map(apply);
 Print(apply);
}
void Utilits4Maps::add_turn(Match_Map *apply){
   apply->not_change_turn_bonus=1;
}
void Utilits4Maps::change( Match_Map *apply, int i1, int k1, int i2, int k2){

    int i_hint,k_hint;
    ready_for_turn();


    //ходит чел
    // флаг
    int good=0;
    // 1  успешен ли ход
    // 2  ход успешен но после хода нет возможности сходить и надо перерисовать карту
    // 3  косячный ход или ход не правильный или ход не заставляет исчезнуть фишки

    if(cap_swap(i1,k1,i2,k2))
                    {
                        // меняем местами фишки
                        swap_board(apply,i1,k1,i2,k2);

                            //есть ли совпадения?
                           if((is_part_of_match(apply,i1,k1)+is_part_of_match(apply,i2,k2))>0)
                           {
                            //check if we won the round
                            if(find_all_matches(apply))
                            {
                                    //ход успешен что то исчезло

                                // в массиве turn_destroy_count[i] валяется все что ебнуло по игроку
                                    good=1;

                                    //check if we lose
                                    if(!find_hint(apply,&i_hint,&k_hint))
                                    {
                                     //display the end game screen
                                    //       printf( "... ходов нет надо бы пеерисовать экран полностью :)" );
                                        good=2;
                                    }
                                    //тут кстати можно отправить хинт челу :)
                            }
                           }
                            else
                            {   //good=3;
                                    //no matches found so swap back
                                    swap_board(apply,i1,k1,i2,k2);
                                     send_interval = apply->alarm_active_turn_user(3);
                                    get_perk_status(apply);
                                    error_number(2," no matches found ");
                                      return;

                            }

                            //replace top row


                } else {
    //  переданные параметры не корректные наприсовать ошибку
       good=3;
    }
    //  теперь по идее уже можно формировать строку для отправки
    // в строке str_new_destr список новых ячеек и уничтожений
    if(good==3)
    {

    //  отдать ошибку
         send_interval = apply->alarm_active_turn_user();
        get_perk_status(apply);
        error_number(1,"change() incorrect coordinate ");
        return;
    }
    // отдать новые и уничтоженные фишки


    if(good==2)
    {  // сформировать новую карту
   re_init_map(apply);
    }




     //тут эффект нового хода
     if(flag_match3_more==1) apply->not_change_turn++;
     send_interval = apply->change_active_turn_user();

     set_win_status(apply);
     flag_match3_more=0;
 // на текущий момент победа или проигрый уже выставлено
 // так же по идее выставлены результаты хода
 // функция которая вызывает эту функцию сама это все разрулит(победа результаты ход итд)
        return;

}

//// <<<<<<<<<<<<  END >>>>>>>>>>>>>>>>>>>>>>>
