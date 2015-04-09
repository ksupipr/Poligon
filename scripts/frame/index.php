<?php

require_once ('conf.php');

$redis = new Redis();
$redis->pconnect($redis_host, $redis_port);
$redis->select(0);

$sf = $redis-> get('stop_flag');

if (!($sf===false)) { 

echo $sf;

} else { 



$api_id['vk'] = 111;
$api_secret['vk'] = "";

$client_dir = 'http://res2.xlab.su/poligon/frame/vk_client.swf?00002';

$parramz = '';


$private_key = $api_secret['vk'];
$api_id_out = $api_id['vk'];

if (is_array($_GET))
    foreach ($_GET as $key => $value) 
		$parramz.= $key.'='.$value.'&';

$parramz.= 'load_error='.base64_encode('Произошла ошибка загрузки приложения...[br]Попробуйте зайти через несколько секунд. При повторении ошибки сообщите о ней разработчикам.').'&';
$parramz.= 'data_error='.base64_encode('Неверный формат полученных данных.').'&';
$parramz.= 'secure_error='.base64_encode('Для подключения не удалось получить файл кросс доменной политики безопасности.').'&';
$parramz.= 'io_error='.base64_encode('Ошибка подключения к серверу.').'&';
$parramz.= 'api_init='.base64_encode('Инициализация приложения...').'&';
$parramz.= 'connecting='.base64_encode('Идёт подключение...').'&';
$parramz.= 'update_player1='.base64_encode('Для работы приложения необходимо обновить флэш плеер.[br]Скачать его можно здесь [u][url').'&';
$parramz.= 'update_player2='.base64_encode('"http://get.adobe.com/ru/flashplayer/" target').'&';
$parramz.= 'update_player3='.base64_encode('"_blank"][/u]').'&';

//$parramz=mb_substr($parramz, 0, strlen($parramz), 'UTF-8');
//$parramz=mb_substr($parramz, 0, -1, 'UTF-8');
$parramz=iconv('windows-1251', 'UTF-8', $parramz);

$get_av  = md5($api_id_out.'_'.intval($_GET["viewer_id"]).'_'.$private_key);

//if($_GET["viewer_id"]==6180194){
//  exit("Отвали");
//}
//echo $parramz;

if ($_GET["auth_key"]==$get_av)
{

?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" dir="ltr" lang="ru-RU" xml:lang="ru">

<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <script src="http://vkontakte.ru/js/api/xd_connection.js?2" type="text/javascript"></script>

<script type="text/javascript">
  VK.init(function() {
     // API initialization succeeded
     // Your code here
  });


function thisMovie(movieName)
{
 if (navigator.appName.indexOf("Microsoft") != -1)
 {
 return window[movieName];
 }
 else
 {
 return document[movieName];
 }
} 


function hideMovie(movieName)
{
 var movie = thisMovie(movieName);

 if (movie != null)
 {
	movie.style.visibility = 'hidden';
 }
}

function showMovie(movieName)
{
 var movie = thisMovie(movieName);

 if (movie != null)
 {
	 movie.style.visibility = 'visible';
 }
} 


function getXmlHttp()
{
  var xmlhttp;
  try {
    xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
  } catch (e) {
    try {
      xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    } catch (E) {
      xmlhttp = false;
    }
  }
  if (!xmlhttp && typeof XMLHttpRequest!='undefined') {
    xmlhttp = new XMLHttpRequest();
  }
  return xmlhttp;
}



function uploadSend(url_img)
{
 VK.api('wall.getPhotoUploadServer', {}, function(data) {
     if (data.response) {
         upload_url = data.response.upload_url;
         alert( upload_url );
         


     }
 }); 
}




VK.addCallback("onWindowBlur", function ()
{
 hideMovie("tanks2");
});

 VK.addCallback("onWindowFocus", function ()
 {
 showMovie("tanks2");
}); 





</script>

</head>

<style>
html, body
{
    margin:0px;
    padding:0px;
    width:100%; height:100%;
    font-family:Arial;
    font-size:9pt;
}
a {
    color:#2B587A;
    text-decoration:none;
    }

a:hover {
    text-decoration:underline;
    }
</style>
<body>


<object id="tanks2" name="tanks2" width="756" height="600" quality="best" allowfullscreen="true" wmode="Window" type="application/x-shockwave-flash" 


data="<? echo $client_dir; ?>"  >

<param name="movie" value="<? echo $client_dir; ?>"  />
	<param name="AllowScriptAccess" value="always">
  	<param name="allowNetworking" value="all">
  	<param name="allowfullscreen" value="true">


<?php

echo '<param name="flashVars" value="'.$parramz.'" />';
?>
 </object>

<a href="http://loc.xlab.su/poligon/io.php?game=tanks2vk&vid=<? echo $_GET["viewer_id"]; ?>" target="_blank" style="display:block; margin-top:11px; text-align:center;" title="Танчики 2" ><img src="http://tanks.github.com/images/tank.jpg" style="border:none;" alt="Танчики 2" /></a>

<div style="display:block; margin-top:5px; text-align:center; "><a target="_blank" href="http://vk.com/page-1_43770896">Пользовательское соглашение</a> | <a target="_blank" href="http://vk.com/page-1_43770893">Политика конфиденциальности</a></div>

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

<?


 } else echo 'Попытка подмены передаваемых значений.'; 

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
