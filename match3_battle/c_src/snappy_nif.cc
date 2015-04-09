/**

 **/

#include <iostream>
#include <cstring>

#include "erl_nif_compat.h"


#ifdef OTP_R13B03
#error OTP R13B03 not supported. Upgrade to R13B04 or later.
#endif

#ifdef __cplusplus
#define BEGIN_C extern "C" {
#define END_C }
#else
#define BEGIN_C
#define END_C
#endif

#define SC_PTR(c) reinterpret_cast<char *>(c)



#include "poligmain.h"

std::vector < Match_Map > all_maps;
int free_maps_count;
Utilits4Maps tools;
int query_count;

void unlock_map(uint16_t id){
    free_maps_count++;
    all_maps[id].is_free=1;
 }


 void lock_map(uint16_t id){

     if(id==0) {
         id++;
     while((id<MAX_MAP_COUNT)&&(all_maps[id].is_free!=1)){
     id++;
     }

     if(id==MAX_MAP_COUNT) {
     // error свободных карт нет :( TODO может  наростить хранилище?
         tools.error_number(6,"no free map, please wait.");
         tools.query_map_id.allbyte=0;
         return;
     }

     if(all_maps[id].is_free==1){all_maps[id].is_free=0; free_maps_count--;}
      else {
         tools.error_number(6,"no free(2) map, please wait .");
         tools.query_map_id.allbyte=0;
         return;
     }
     // заняли карту
     all_maps[id].is_free=0;
     }
     tools.query_map_id.allbyte=id;
     tools.Print(&(all_maps[id]));
 }

  void new_battle(int user0x0,    int user0x1,    int user0x2,
                  int user1x0,    int user1x1,    int user1x2){
  // забиваем карту

  lock_map(0);
    if(tools.query_map_id.allbyte!=0){

      tools.new_battle(&(all_maps[tools.query_map_id.allbyte]), user0x0,  user0x1,  user0x2, user1x0,    user1x1,    user1x2);

    }
  }

int query( int num, unsigned long arg0,unsigned long arg1,unsigned long arg2,unsigned long arg3,unsigned long arg4,unsigned long arg5){

/*
encode({free,  I, D}) -> [1,  I, D]; %% Освободить карту
encode({get_map, I, D}) -> [2, I, D]; %% Дай свободную карту/дай карту такую то
encode({change, I, D,X1,Y1,X2,Y2}) -> [3, I, D,X1,Y1,X2,Y2]; %%  сходит на такой то карте так то
encode({destroy_point, I, D,X1,Y1}) -> [4, I, D,X1,Y1]; %%  Уничтожить квадрат с центром по координтатам
encode({destroy_line, I, D,Y1}) -> [5, I, D,Y1]. %%  Уничтожить линию
encode({count,  I, D}) -> [1,  I, D]; %% Сколько свободных карт?
*/


     //std::cout <<"num: "<<num<<" "<<query_map_id.allbyte<<"\n";

    switch (num) {

            case 1:{  unlock_map(tools.query_map_id.allbyte);  } break; // Освободить карту(например по таймауту игрока)
            case 2:{  lock_map(tools.query_map_id.allbyte);   } break;  //получить карту
            case 3:{   // ход
                tools.change(&(all_maps[tools.query_map_id.allbyte]),arg1,arg2,arg3,arg4);   } break;
            case 4:{ //унистожить вертикальную и горизонтальную линию +
                tools.boom(&(all_maps[tools.query_map_id.allbyte]),arg1,arg2);   } break;
            case 5:{ // рефреш карты
                tools.refresh(&(all_maps[tools.query_map_id.allbyte]),arg1);   } break;
            case 6:{ // новая карта и битва
                 new_battle(arg0,arg1,arg2,arg3,arg4,arg5);   } break;
           default:
            { // todo log it
         //std::cout <<"default: "<<num<<"\n";
              }

} // end switch (number)


    return 2;

}

static inline ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom_compat(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}
static inline ERL_NIF_TERM
make_error(ErlNifEnv* env, const char* mesg)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, mesg));
}



static inline ERL_NIF_TERM
make_ok(ErlNifEnv* env, ERL_NIF_TERM mesg)
{
    ERL_NIF_TERM ok = make_atom(env, "ok");
    return enif_make_tuple2(env, ok, mesg);
}
static inline ERL_NIF_TERM
make_panel_user(ErlNifEnv* env, uint32_t user0)
{ // apply->user_perk[i][j]<<"/"<<apply->max_user_perk[i][j]
    ERL_NIF_TERM ChangeFl = enif_make_uint_compat(env,tools.user_perk_change[user0]);

    ErlNifBinary term[6];
    for (int i=0;i<6;i++)       {
        if(!enif_alloc_binary(tools.user_panel[user0][i].str().length(), &(term[i]))) {

            for (int j=0;j<i;j++)       {   enif_free(&(term[i])); }
            return  make_error(env, "insufficient_memory");
            }
        memcpy(term[i].data, tools.user_panel[user0][i].str().c_str(), tools.user_panel[user0][i].str().length());

    }


    return   enif_make_tuple7(env, ChangeFl,
                              enif_make_binary(env, &(term[0])),
                               enif_make_binary(env, &(term[1])),
                               enif_make_binary(env, &(term[2])),
                               enif_make_binary(env, &(term[3])),
                               enif_make_binary(env, &(term[4])),
                               enif_make_binary(env, &(term[5])));
}

static inline ERL_NIF_TERM
make_move_type(ErlNifEnv* env)
{
 // создает нужный тупл для кодирования конца хода
    ERL_NIF_TERM ChangeFl = enif_make_uint_compat(env,tools.now_move_user);
    ERL_NIF_TERM ChangeFl2 = enif_make_uint_compat(env,tools.type_move_user);
    ERL_NIF_TERM ChangeFl3 = enif_make_uint_compat(env,tools.send_interval);

    return   enif_make_tuple3(env, ChangeFl,ChangeFl2,ChangeFl3);
}




static inline ERL_NIF_TERM
make_reply(ErlNifEnv* env)
{
    // подготавливаем бинарные наборы
    ErlNifBinary b_map;

    if(!enif_alloc_binary(tools.all_destroy_f.str().length(), &b_map))
      return  make_error(env, "insufficient_memory");



    memcpy(b_map.data, tools.all_destroy_f.str().c_str(), tools.all_destroy_f.str().length());

    //              id   next_turn_user  win_user panel_user0 panel_user1
    // {match_res,100500,0|1,            3|0|1,   <<...>>,<<...>>,map,<<...>> }
    ERL_NIF_TERM term0 = make_atom(env, "match_res");
    ERL_NIF_TERM term1 = enif_make_uint_compat(env,tools.query_map_id.allbyte);

    if(tools.error_flag>0){
        // победа окончательна ?
        if(tools.win_user==3) tools.win_user=4; //<то есть ошибка
        else { // победа окончательна т.е. (в результате ошибки мы победили)
        tools.win_user=tools.win_user+5;
         }
    }
/*
Итого по победам
0 1 нормальная победа/проигрыш
3 4 ошибка какая то
5 6 победа/проигрыш изза ошибки
7 8 победа/проигрыш по времени

*/

    ERL_NIF_TERM term2 = make_move_type(env);
    ERL_NIF_TERM term3 = enif_make_uint_compat(env,tools.win_user);

    ERL_NIF_TERM term4 = make_panel_user(env, 0);
    ERL_NIF_TERM term5 =  make_panel_user(env, 1);
    ERL_NIF_TERM term6 = make_atom(env, "map");
    ERL_NIF_TERM term7 = enif_make_binary(env, &b_map);
    tools. all_destroy_f.str("");

    return   enif_make_tuple8(env, term0,term1, term2,term3,term4, term5,term6,term7);
}


BEGIN_C


ERL_NIF_TERM
change_health(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // {"change_health", 3, }, // добавление и снятие "здовровья"
// параметры id карты
    // прибавить или отнять
    //  сколько

    if(argc!=3){
   return make_error(env, "query err");
    }
    unsigned long arg_int[6];


 for (int num_run=0;num_run<argc;num_run++){
     if (!enif_get_ulong(env, argv[num_run], &arg_int[num_run]))
         return make_error(env, "no id");
 }
      // первое число это id карты

 if(all_maps[arg_int[0]].is_free!=0)
    return make_error(env, "this map free");
int user;

tools.query_map_id.allbyte=arg_int[0];
int dp_health=0;
tools.ready_for_turn();

if(arg_int[1]==0) { // наносим урон
user=all_maps[tools.query_map_id.allbyte].get_noactive_user(); //тот на кого применяют негативный эффект
dp_health=(-1)*arg_int[2];
} else {
user=all_maps[tools.query_map_id.allbyte].user_active_turn;  // тот на кого применяют позитивный эффект
dp_health=arg_int[2];
}




all_maps[tools.query_map_id.allbyte].user_perk_change[user]=1;
all_maps[tools.query_map_id.allbyte].change_perk(user,0,dp_health);
all_maps[tools.query_map_id.allbyte].change_perk(user,1,dp_health);
all_maps[tools.query_map_id.allbyte].change_perk(user,2,dp_health);

Match_Map *apply=&(all_maps[tools.query_map_id.allbyte]);
if(arg_int[1]==0){
// проверка нет ли завершения битвы
    int sum=apply->user_perk[user][0]+apply->user_perk[user][1]+apply->user_perk[user][2];

    if(sum<=0){
    apply->win_user=apply->user_active_turn;
    }
}

// собираем сообщение нужным образом
{

        tools.all_destroy_f<<"{\"panels\":{ ";
        if(dp_health<0) {
        tools.all_destroy_f<<" \"param0\":1, ";
        } else {
        tools.all_destroy_f<<" \"param0\":2, ";
        }
        if((apply->user_perk_change[0]>0))
        {

         apply->user_perk_change[0]=0;
         tools.all_destroy_f<<"\"panel0\":{";
         int i=0;
        for (int j=0; j<6; j++){
        if(j>0) tools.all_destroy_f<<",";
       tools. all_destroy_f<<"\"tx"<<j<<"\":\""<<apply->user_perk[i][j]<<"/"<<apply->max_user_perk[i][j]<<"\"";

        }
        tools.all_destroy_f<<"}";
      //   if((apply->user_perk_change[1]>0)){ tools.all_destroy_f<<" , "; }
    }

  if((apply->user_perk_change[1]>0)){
              apply->user_perk_change[1]=0;
   int i=1;
   tools.all_destroy_f<<"\"panel1\":{";
   for (int j=0; j<6; j++){

       if(j>0) tools.all_destroy_f<<",";
       tools.all_destroy_f<<"\"tx"<<j<<"\":\""<<apply->user_perk[i][j]<<"/"<<apply->max_user_perk[i][j]<<"\"";

                      }
          tools.all_destroy_f<<"}";
          }

tools.all_destroy_f<<"}} ";
    }
tools.get_perk_status(&(all_maps[tools.query_map_id.allbyte]));
//так как мы бонус требуется неким костылем поправить результат перехода хода
// на 6
tools.type_move_user=6;
return make_reply(env);

}

ERL_NIF_TERM
all_query(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // {"change", 5, all_query}, 3
     //{"boom", 3, all_query},   4
     //{"line", 2, all_query}    5
    //{"new", 6, new_battle },   6

    unsigned long arg_int[6];
    int num=3;

 switch (argc) {
         case 2:{num=5; } break;
         case 3:{num=4; } break;
         case 5:{num=3; } break;
         case 6:{num=6; } break;
 default:
   { return make_error(env, "query err");
   }

} // end switch (argc)

 for (int num_run=0;num_run<argc;num_run++){
     if (!enif_get_ulong(env, argv[num_run], &arg_int[num_run])) return make_error(env, "no id");
    // std::cout<<" ["<<num_run<<"] "<<arg_int[num_run]<<" \n";
     if((num!=6)  // запрос не new
             && (num_run!=0) // не идентификатор карты
             &&(arg_int[num_run]>=MAPX_Y)){
         arg_int[num_run]=MAPX_Y-1;
         // координаты в заданных пределах
     }
 }
 if(num!=6) {
     // тогда первое число это id карты
 if(all_maps[arg_int[0]].is_free!=0)
    return make_error(env, "this map free");
 }

tools.query_map_id.allbyte=arg_int[0];
query(num,arg_int[0],arg_int[1],arg_int[2],arg_int[3],arg_int[4],arg_int[5]);

return make_reply(env);

}
// end all_query(


ERL_NIF_TERM
unlock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{



    unsigned long arg_int0;
if (!enif_get_ulong(env, argv[0], &arg_int0))
    return make_error(env, "no id");

 tools.query_map_id.allbyte=arg_int0;
 query(1,arg_int0, 0,0,0,0,0);
 return make_atom(env, "true");

}
// завершение битвы надо победить тому у кого больше здоровья
ERL_NIF_TERM end_timeout(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned long arg_int0;
    if (!enif_get_ulong(env, argv[0], &arg_int0))
        return make_error(env, "no id");
    tools.query_map_id.allbyte=arg_int0;
    Match_Map *apply=&(all_maps[tools.query_map_id.allbyte]);
    tools.end_timeout(apply);
    return make_reply(env);
}

ERL_NIF_TERM
no_change(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{  //переход хода в битве
    if(argc!=1){
   return make_error(env, "query err");
    }
unsigned long arg_int0;
if (!enif_get_ulong(env, argv[0], &arg_int0))
    return make_error(env, "no id");
 tools.query_map_id.allbyte=arg_int0;

 Match_Map *apply=&(all_maps[tools.query_map_id.allbyte]);


 tools.add_turn(apply);
 // так как не выдаем стандартный ответ за тип хода отвечает вышестоящий код
 return make_ok(env,make_atom(env, "apply"));

}

ERL_NIF_TERM
random_cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{  //уничтожение случайных 12 ячеек {"random", 1, },
    if(argc!=1){
       return make_error(env, "query err");
    }
  unsigned long arg_int0;
if (!enif_get_ulong(env, argv[0], &arg_int0))
    return make_error(env, "no id");

tools.query_map_id.allbyte=arg_int0;

 Match_Map *apply=&(all_maps[tools.query_map_id.allbyte]);


 tools.random(apply);
 return make_reply(env);

}

ERL_NIF_TERM
next_user(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{  //переход хода в битве
    if(argc!=1){
   return make_error(env, "query err");
    }
unsigned long arg_int0;
if (!enif_get_ulong(env, argv[0], &arg_int0))
    return make_error(env, "no id");
 tools.query_map_id.allbyte=arg_int0;

 Match_Map *apply=&(all_maps[tools.query_map_id.allbyte]);

 //tools.query_map_id.allbyte
 apply->not_change_turn=0;
 apply->not_change_turn_bonus=0; // игнорируем бонус для надежности обнулим их
 tools.send_interval = apply->alarm_active_turn_user(4);
 tools.get_perk_status(apply);

 return make_reply(env);

}


ERL_NIF_TERM
get_map(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
if(argc!=1){
   return make_error(env, "query err");
}
unsigned long arg_int0;
if (!enif_get_ulong(env, argv[0], &arg_int0))
    return make_error(env, "no id");

 tools.query_map_id.allbyte=arg_int0;

 query(2,arg_int0, 0,0,0,0,0);
 // отдать карту + состояние
 // возвращаем ответ в виде кортежа

 //              id   next_turn_user
 // {match_res,100500,0|1,panel_user0,<<...>>,panel_user1,<<...>>,mboard,<<...>> }
tools.send_interval = 0;
return make_reply(env);

}




int
on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    all_maps.reserve(MAX_MAP_COUNT);
    struct timeval val;
    gettimeofday(&val, NULL);
        srand((int64_t)val.tv_sec*(int64_t)1000000+(int64_t)val.tv_usec);
        query_count=0;
        Match_Map new_map;
        new_map.is_free=1;

        for (int i=0; i<MAX_MAP_COUNT;i++){
            all_maps.push_back(new_map);
        }
        free_maps_count=MAX_MAP_COUNT-1;
        std::vector < Match_Map >::iterator tt;
        for (tt=all_maps.begin();tt!=all_maps.end();tt++){
        tools.setup_random_map((&(*tt)));
        }
       //  std::cout<<" "<<"load ok \n";
    return 0;
}


/**

TODO
new (tx0,tx1,tx2, tx0,tx1,tx2) -> id, << map >>
get (id) ->  << map >>
change(id,x1,y1,x2,y2) -> battle_result, << map_change >>
line(id,x1) -> battle_result, << map_change >>
boom(id,x1,y1) -> battle_result, << map_change >>
Карта -> id, bool free
битва -> id, id_map, turn, user_status

*/



static ErlNifFunc nif_functions[] = {
    {"free", 1, unlock},
    {"get_map", 1,get_map },
    {"next_user", 1,next_user }, // +переход хода
    {"new", 6, all_query },      // +новая битва
    {"change", 5, all_query},    // +ход
    {"no_change", 1,no_change }, // +доп ход
    {"change_health", 3, change_health}, // добавление и снятие "здовровья"
    {"random_cell", 1, random_cell},    // +12 ячеек
    {"boom", 3, all_query},   //+ квадрат 3 на 3  /->/  Вертикаль и горизонталь
    {"end_timeout", 1, end_timeout},   // завершение битвы надо победить тому у кого больше здоровья
    {"line", 2, all_query}    //+ рефреш карты
};


ERL_NIF_INIT(snappy, nif_functions, &on_load, NULL, NULL, NULL);


END_C
