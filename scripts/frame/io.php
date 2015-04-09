<?php

require_once ('conf.php');

$redis = new Redis();
$redis->pconnect($redis_host, $redis_port);
$redis->select(0);


$game = $_GET['game'];


switch ($game) {

    case 'tanks2vk':  
                    $redis-> incr($game, 1);
                    header("Location: http://vk.com/app1888415");
                    break;
    case 'tanks2ok':  
                    $redis-> incr($game, 1);
                    header("Location: http://www.odnoklassniki.ru/game/tanks2");
                    break;
    case 'tanks2ml':  
                    $redis-> incr($game, 1);
                    header("Location: http://my.mail.ru/apps/625881");
                    break;
    case 'poligon':  
                    $redis-> incr($game, 1);
                    header("Location: http://vk.com/app2955611");
                    break;
    case 'poligonok':  
                    $redis-> incr($game, 1);
                    header("Location: http://www.odnoklassniki.ru/games/polygon-match-3");
                    break;
    case 'poligonml':  
                    $redis-> incr($game, 1);
                    header("Location: http://my.mail.ru/apps/683880");
                    break;
    default :       header("Location: http://xlab.su/");
}


if (isset($_GET['vid'])) {
$vid = preg_replace('/\D/','', $_GET['vid']);

$fp = fopen('/tmp/redirect_log.txt', 'a+');
fwrite($fp, "".date('Y-m-d H:i:s')." | ".$vid." | ".$game."\n");
fclose($fp);
}


?>
