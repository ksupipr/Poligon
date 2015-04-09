<?

//параметры приложения

$appId=11;
$appKey="";
$application_secret_key = "";

$buy_in_val = 800;

$redis = new Redis();
//$redis->pconnect('localhost', 6379);
//$redis->pconnect('192.168.1.4', 6379);
$redis->pconnect('192.168.1.20', 6379);
$redis->select(0);

$fp = fopen('/tmp/okp_log.txt', 'a+');
fwrite($fp, "\n\n".date('Y-m-d H:i:s'."\n"));

header('Content-Type: application/xml; charset=utf-8');
//error_reporting(E_ERROR);



//var_dump($_REQUEST);

//читаем переданные параметры 
fwrite($fp, "читаем переданные параметры \n");
$method = $_REQUEST["method"];
$application_key = $_REQUEST["application_key"];
$call_id = $_REQUEST["call_id"];
$sig = $_REQUEST["sig"];
$uid = $_REQUEST["uid"];
$amount = $_REQUEST["amount"];
$transaction_time = $_REQUEST["transaction_time"];
$product_code = $_REQUEST["product_code"];
$transaction_id = $_REQUEST["transaction_id"];

fwrite($fp, "method=".$method."\n");
fwrite($fp, "application_key=".$application_key."\n");
fwrite($fp, "call_id=".$call_id."\n");
fwrite($fp, "sig=".$sig."\n");
fwrite($fp, "uid=".$uid."\n");
fwrite($fp, "amount=".$amount."\n");
fwrite($fp, "transaction_time=".$transaction_time."\n");
fwrite($fp, "product_code=".$product_code."\n");
fwrite($fp, "transaction_id=".$transaction_id."\n");



//проверяем метод
fwrite($fp, "\nпроверяем метод \n");
if($method != "callbacks.payment") {
header('invocation-error: 3');
print('<?xml version="1.0" encoding="UTF-8"?>');
echo "<ns2:error_response xmlns:ns2='http://api.forticom.com/1.0/'>
    <error_code>3</error_code>
    <error_msg>Method does not exist.</error_msg>
</ns2:error_response>";

fwrite($fp, 'неверный метод');
fclose($fp);
die();
}

//проверяем appId
fwrite($fp, "\nпроверяем appId \n");
if($appKey != $application_key) {
header('invocation-error: 101');
print('<?xml version="1.0" encoding="UTF-8"?>');

echo "<ns2:error_response xmlns:ns2='http://api.forticom.com/1.0/'>
    <error_code>101</error_code>
    <error_msg>Parameter application_key not specified or invalid</error_msg>
</ns2:error_response>";
fwrite($fp, 'неверный application_key');
fclose($fp);
die();
}

//собираем переданные параметры без учета sig
$i = 0;
$params = array();
foreach ($_GET as $key => $value) {
	if($key != "sig") {
		$params[$i] = "$key=$value";
		$i++;
	}
}
sort($params);
$params = join('', $params);
$mySig = md5($params . $application_secret_key);

//проверяем подпись
fwrite($fp, "\nпроверяем sig mySig=".$mySig."\n");
if($sig != $mySig) {
header('invocation-error: 104');
print('<?xml version="1.0" encoding="UTF-8"?>');
echo "<ns2:error_response xmlns:ns2='http://api.forticom.com/1.0/'>
    <error_code>104</error_code>
    <error_msg>Invalid signature.</error_msg>
</ns2:error_response>";
fwrite($fp, 'неверный sig');
fclose($fp);
die();
}


//-------------------------


fwrite($fp, "\nЗачисляем кредитов.\n");

$redis->lPush('paymant_'.$uid, $transaction_id);

$t_id = $redis->get('paymant_tid'.$uid);


//$t_id = $memcache_world->get('paymant_'.$transaction_id.'_'.$uid);
if ((intval($t_id)!=intval($transaction_id)) && (intval($transaction_id)!=0))
{
	fwrite($fp, "Записываю транзакцию\n");
	$redis->set('paymant_tid'.$uid, $transaction_id.'', 3000);

	if (($amount*100)>=$buy_in_val)
		{
//echo 'amount='.$amount.'&&';
//echo 'product_code='.$product_code;

		
					


					    $user_name = 'ok_'.$uid;
					
						//$add_summ = floor($product_code/$buy_in_val);
						$add_summ = floor(($amount*100)/$buy_in_val);

						    fwrite($fp, "Зачисляю кредиты.\n");

                            if ($product_code<10) $product_code = 0;
						    $set_in = $redis->publish('paymant.ok', '{'.$user_name.', '.$add_summ.', '.$product_code.'}');
							fwrite($fp, '$set_in = '.$set_in."\n");

							if (intval($set_in)>0) 
							{

										fwrite($fp, "Кредиты зачислены. Отдаем успешный статус\n");


										//отдаем успешный статус
										print('<?xml version="1.0" encoding="UTF-8"?>');
										echo "<callbacks_payment_response xmlns='http://api.forticom.com/1.0/'>true</callbacks_payment_response>";

										flush();

										fwrite($fp, "отдали успешный статус и беснуемся дальше.");	


							} else err_out_okp("В редис не записалось ( отмена. \n");
						
				
		} else err_out_okp('($amount*100)<$buy_in_val и равен '.($amount*100).'<'.$buy_in_val.'');




fwrite($fp, 'все гууд.');
fclose($fp);
} else err_out_okp('transaction_id уже был или =0');


function err_out_okp($log_text)
{
	global $fp;
	header('invocation-error: 104');
	print('<?xml version="1.0" encoding="UTF-8"?>');
echo "<ns2:error_response xmlns:ns2='http://api.forticom.com/1.0/'>
    <error_code>104</error_code>
    <error_msg>Invalid signature.</error_msg>
</ns2:error_response>";
fwrite($fp, $log_text);
fclose($fp);
die();
}

?>
