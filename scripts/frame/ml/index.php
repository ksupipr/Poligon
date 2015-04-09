<?php

require_once ('../conf.php');

$private_key = '';
define('CLIENT_DIR', 'http://res2.xlab.su/poligon/frame/ml/ml_client.swf');

$redis = new Redis();
$redis->pconnect($redis_host, $redis_port);
$redis->select(0);

$sf = $redis-> get('stop_flag');

if (!($sf===false)) { 

echo $sf;

} else { 


  $is_app_user        = $_GET['is_app_user'];
  $session_key        = $_GET['session_key'];
  $vid                = $_GET['vid'];
  $oid                = $_GET['oid'];
  $app_id             = $_GET['app_id'];
  $authentication_key = $_GET['authentication_key'];
  $session_expire     = $_GET['session_expire'];
  $ext_perm           = $_GET['ext_perm'];
  $sig                = $_GET['sig'];
  $window_id          = $_GET['window_id'];
  $referer_type       = $_GET['referer_type'];
  $referer_id         = $_GET['referer_id'];
 


$parramz1 = '';

foreach ($_GET as $key => $value) {
	if ($key!='sig'){
		$parramz[$key]=$value;
    if ($key=='authentication_key'){
      $parramz1.= 'auth_key='.$value.'&';
    }else{
      $parramz1.= $key.'='.$value.'&';
    }
  }
}

$parramz1.= 'load_error='.base64_encode('Произошла ошибка загрузки приложения...[br]Попробуйте зайти через несколько секунд. При повторении ошибки сообщите о ней разработчикам.').'&';
$parramz1.= 'data_error='.base64_encode('Неверный формат полученных данных.').'&';
$parramz1.= 'secure_error='.base64_encode('Для подключения не удалось получить файл кросс доменной политики безопасности.').'&';
$parramz1.= 'io_error='.base64_encode('Ошибка подключения к серверу.').'&';
$parramz1.= 'api_init='.base64_encode('Инициализация приложения...').'&';
$parramz1.= 'connecting='.base64_encode('Идёт подключение...').'&';
$parramz1.= 'update_player1='.base64_encode('Для работы приложения необходимо обновить флэш плеер.[br]Скачать его можно здесь [u][url').'&';
$parramz1.= 'update_player2='.base64_encode('"http://get.adobe.com/ru/flashplayer/" target').'&';
$parramz1.= 'update_player3='.base64_encode('"_blank"][/u]').'&';

if (is_array($parramz))
{
	$my_sig = sign_client_server($parramz, $uid, $private_key);
} else $my_sig='err';

//echo  $my_sig.'='.$sig;

if ($sig==$my_sig)
{
?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" dir="ltr" lang="en-US" xml:lang="en">

<head>
  <script type="text/javascript" src="http://connect.mail.ru/js/loader.js"></script>




</head>
<body>


<object id="tanks2" name="tanks2"  width="756" height="600" quality="best" allowfullscreen="true" wmode="Window" type="application/x-shockwave-flash" 
 data="<? echo CLIENT_DIR; ?>"  >
<param name="movie" value="<? echo CLIENT_DIR; ?>"  />
	<param name="AllowScriptAccess" value="always">
  	<param name="allowNetworking" value="all">

<?php

echo '<param name="flashVars" value="'.$parramz1.'" />';
?>
 </object>
<a href="http://loc.xlab.su/poligon/io.php?game=tanks2ml&vid=<? echo $uid; ?>" target="_blank" style="display:block; margin-top:11px; text-align:center;" title="Танчики 2" ><img src="http://tanks.github.com/images/tank.jpg" style="border:none;" alt="Танчики 2" /></a>

<!-- Yandex.Metrika counter -->
<script type="text/javascript">
(function (d, w, c) {
    (w[c] = w[c] || []).push(function() {
        try {
            w.yaCounter14753050 = new Ya.Metrika({id:14753050, enableAll: true, ut:"noindex"});
        } catch(e) {}
    });
    
    var n = d.getElementsByTagName("script")[0],
        s = d.createElement("script"),
        f = function () { n.parentNode.insertBefore(s, n); };
    s.type = "text/javascript";
    s.async = true;
    s.src = (d.location.protocol == "https:" ? "https:" : "http:") + "//mc.yandex.ru/metrika/watch.js";

    if (w.opera == "[object Opera]") {
        d.addEventListener("DOMContentLoaded", f);
    } else { f(); }
})(document, window, "yandex_metrika_callbacks");
</script>
<noscript><div><img src="//mc.yandex.ru/watch/14753050?ut=noindex" style="position:absolute; left:-9999px;" alt="" /></div></noscript>
<!-- /Yandex.Metrika counter -->

</body>
</html>
<? } else echo 'Попытка подмены передаваемых значений.'; 
}

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
