<?

$private_key = '';
$buy_in_val = 800;

$redis = new Redis();
//$redis->pconnect('localhost', 6379);
//$redis->pconnect('192.168.1.4', 6379);
$redis->pconnect('192.168.1.20', 6379);
$redis->select(0);

$fp = fopen('/tmp/polig_ml_log.txt', 'a+');
fwrite($fp, "\n\n".date('Y-m-d H:i:s'."\n"));

//$memcache_world = new Memcache();
//$memcache_world->pconnect($memcache_world_url, $memcache_world_port);

//var_dump($_GET);

    foreach ($_GET as $key => $value) {
	if ($key!='sig')
		$parramz[$key]=$value;
	}

$uid = $_GET[uid];
$sig = $_GET[sig];
$debug = $_GET[debug];
$service_id = intval($_GET[service_id]);

$transaction_id = $_GET[transaction_id];

fwrite($fp, "uid=".$uid." \n");
fwrite($fp, "transaction_id=".$transaction_id." \n");
fwrite($fp, "service_id=".$service_id." \n");
fwrite($fp, "sig=".$sig." \n");

$other_price = intval($_GET['other_price']);
$sms_price = intval($_GET['sms_price']);

$mailiki_price = intval($_GET['mailiki_price']);

$profit = intval($_GET['profit']);

fwrite($fp, "other_price=".$other_price." \n");
fwrite($fp, "sms_price=".$sms_price." \n");
fwrite($fp, "mailiki_price=".$mailiki_price." \n");
fwrite($fp, "PROFIT!!!1=".$profit." \n");


if (intval($mailiki_price)!=0)  { $price = $mailiki_price*100; $add_sid=$mailiki_price*100; }
if (intval($other_price)!=0) { $price = $other_price; $add_sid=$other_price; }
if (intval($sms_price)!=0) { $price = $sms_price; }

$my_sig = sign_client_server($parramz, $uid, $private_key);

$status = 0;
$error_code = 700;

/*
    * 701 User not found — если приложение не смогло найти пользователя для оказания услуги
    * 702 Service not found — если услуга с данный идентификатором не существуем в вашем приложении
    * 703 Incorrect price for given uid and service_id — если данная услуга для данного пользователя не могла быть оказана за указанную цену
    * 700 Other error — другая ошибка

*/

$redis->lPush('paymant_'.$uid, $transaction_id);

$t_id = $redis->get('paymant_tid'.$uid);


//$t_id = $memcache_world->get('paymant_'.$transaction_id.'_'.$uid);
if ((intval($t_id)!=intval($transaction_id)) && (intval($transaction_id)!=0))
{

fwrite($fp, "my_sig=".$my_sig." \n");

if ($my_sig==$sig)
{
fwrite($fp, "Транзакция новая и sig верный \n");

	if ($add_sid>=$buy_in_val)
		{
			if ($price!=0)	
				{
					$user_name = 'ml_'.$uid;

					$add_summ = floor($add_sid/$buy_in_val);
                    // тут начисляем бабло
                            if ($service_id<10) $service_id = 0;
                            $set_in = $redis->publish('paymant.ml', '{'.$user_name.', '.$add_summ.',  '.$service_id.'}');
                            fwrite($fp, '$set_in = '.$set_in."\n");
                    // 
                        if (intval($set_in)>0) {
										fwrite($fp, "Записали ".$add_summ."кредитов пользователю ".$tank_id." uid=".$uid." prefix=ml \n");

										$status=1;
										$error_code = 0;

										echo '{"status":"'.$status.'"}';

										flush();

										fwrite($fp, "Отдал положительный статус  ".'{"status":"'.$status.'"}'."\n");
										//$memcache_world->set('paymant_'.$uid, $transaction_id, 0, 600);
										@$redis->set('paymant_tid'.$uid, $transaction_id.'', 3000);
										fwrite($fp, "Записал транзакцию в редис \n");
                        } else { $error_code = 703; fwrite($fp, "В редис не записалось ( отмена. \n"); }
				}  else $error_code = 703;
		} else $error_code = 702;
} else $error_code = 701;
} else {

//	$status = 2;
	fwrite($fp, "Транзакция уже была, возвращаю статус 1 \n");

	$status=1;
	$error_code = 0;

	echo '{"status":"'.$status.'"}';
	flush();
}

if ($error_code!=0)
{
 echo '{"status":"'.$status.'", "error_code":"'.$error_code.'"}';
fwrite($fp, "Вернул ошибку: ".'{"status":"'.$status.'", "error_code":"'.$error_code.'"}'." \n");
}

fwrite($fp, "Закончили уражнение. \n");
fclose($fp);

function sign_client_server(array $request_params, $uid, $private_key) {
        ksort($request_params);
        $params = '';
        foreach ($request_params as $key => $value) {
          $params .= "$key=$value";
        }

	$my_sig = md5( $params . $private_key);
        return $my_sig;
}
?>
