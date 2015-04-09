<?

function buyCredits($TId, $user_id, $item_id, $item_price) {

//global $id_world;
//global $mcpx;

//global $memcache_world_url;
//global $memcache_world_port;



$fp = fopen('/tmp/vk_poligon_log.txt', 'a+');
fwrite($fp, "\n\n".date('Y-m-d H:i:s'."\n"));




fwrite($fp, "подключаюсь к редису $redis_host[0], $redis_port[0] \n");


$redis = new Redis();
//$redis->pconnect('localhost', 6379);
$redis->pconnect('192.168.1.4', 6379);
//$redis->pconnect('192.168.1.20', 6379);
$redis->select(0);

$error_out = 0;

$item_num = $item_id;// сколько начислить


fwrite($fp, "входные параметры: ".$TId.'|'.$user_id.'|'.$item_id.'|'.$item_price."\n");


fwrite($fp, "пишу транзакцию \n");
//$add_res =  $redis->set('t_'.$TId, $user_id.'|'.$item_id.'|'.$item_price, false, 86400);

$add_res =  add_redis($redis, 't_'.$TId, $user_id.'|'.$item_id.'|'.$item_price, 1, 86400);

fwrite($fp, "результат: ".intval($add_res)."\n");

if ($add_res) {
				// $item_num - сколько начислить
                // и тут начисление
				

                        	fwrite($fp, "Зачисляю кредиты.\n");

							$user_name = 'vk_'.$user_id;

                            $abt = $redis->get('abt_'.$user_id);

						    $set_in = $redis->publish('paymant.vk', '{'.$user_name.', '.$item_num.', '.intval($abt).'}');
							fwrite($fp, '$set_in = '.$set_in."\n");

							if (intval($set_in)>0) 
							{

										fwrite($fp, "Кредиты зачислены. Отдаем успешный статус\n");


										//отдаем успешный статус
                                        $error_out = $TId+time();


										//flush();

										//fwrite($fp, "отдали успешный статус и беснуемся дальше.");	

										//$error_out = $TId+time();
							} else {
									fwrite($fp, "В редис не записалось ( отмена. \n"); 
									$error_out = -22;
							}
                                        
                                        
                                        

                            //        } else $error_out = -22;
                            //}  else $error_out = -103;

   


} else $error_out = -113;
fwrite($fp, "error_out на момент окончания = ".$error_out." \n");

fwrite($fp, "Закончили упражнение \n");
fclose($fp);
return $error_out;
}



function add_redis($redis, $key, $val, $time) {

 // Get key value
 $old_val = $redis->getSet($key, $val);
            $redis->expire($key, $time);
 if ($old_val===false) {
    return true;
 } else return false;


}

?>
