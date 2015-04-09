<?php


if (isset($_POST['query']))
{
    $json_in = stripslashes($_POST['query']);
}

$input_q = json_decode ( $json_in , true) or die('{"error": {"type":"1", "val":"1"}}');



$sn_prefix   = $input_q["auth"]["sn_prefix"];
$sn_id       = $input_q["auth"]["sn_id"];
$auth_key    = $input_q["auth"]["auth_key"];


$version = 'a0000020';

$host_world[1] = '"http://res2.xlab.su/poligon"';
$host_world[2] = '"http://res2.xlab.su/poligon"';
$host_world[3] = '"http://res2.xlab.su/poligon"';
$host_world_res[0] = '{"url":"res/fly.away", "vers":"'.$version.'"}';
$host_world_res[1] = '{"url":"res/main.swf", "vers":"'.$version.'"}';
$host_world_res[2] = '{"url":"res/utils.xlab", "vers":"'.$version.'"}';
$host_world_res[3] = '{"url":"res/locale/ru_RU/fonts.xlab", "vers":"'.$version.'"}';
$host_world_res[4] = '{"url":"res/locale/ru_RU/lang.xinfo", "vers":"'.$version.'"}';
$host_world_res[5] = '{"url":"res/game_name.xart0", "vers":"'.$version.'"}';
$host_world_res[6] = '{"url":"res/enemy_turn.xart0", "vers":"'.$version.'"}';
$host_world_res[7] = '{"url":"res/your_turn.xart0", "vers":"'.$version.'"}';
$host_world_res[8] = '{"url":"res/locale/ru_RU/merits.txt", "vers":"'.$version.'"}';
$host_world_res[9] = '{"url":"res/locale/ru_RU/orders.txt", "vers":"'.$version.'"}';
$host_world_res[10] = '{"url":"res/pr.xart0", "vers":"'.$version.'"}';
$host_world_res[11] = '{"url":"res/sound/match3.mp3", "vers":"'.$version.'"}';
$host_world_res[12] = '{"url":"res/sound/match4.mp3", "vers":"'.$version.'"}';
$host_world_res[13] = '{"url":"res/sound/match5.mp3", "vers":"'.$version.'"}';
$host_world_res[14] = '{"url":"res/sound/care1.mp3", "vers":"'.$version.'"}';
$host_world_res[15] = '{"url":"res/sound/care2.mp3", "vers":"'.$version.'"}';
$host_world_res[16] = '{"url":"res/sound/napalm.mp3", "vers":"'.$version.'"}';
$host_world_res[17] = '{"url":"res/sound/krest.mp3", "vers":"'.$version.'"}';
$host_world_res[18] = '{"url":"res/sound/rikochet.mp3", "vers":"'.$version.'"}';
$host_world_res[19] = '{"url":"res/sound/attack.mp3", "vers":"'.$version.'"}';
$host_world_res[20] = '{"url":"res/sound/health_bon.mp3", "vers":"'.$version.'"}';
$host_world_res[21] = '{"url":"res/sound/free_turn.mp3", "vers":"'.$version.'"}';
$host_world_res[22] = '{"url":"res/sound/new_turn.mp3", "vers":"'.$version.'"}';
$host_world_res[23] = '{"url":"res/sound/select1.mp3", "vers":"'.$version.'"}';
$host_world_res[24] = '{"url":"res/sound/move1.mp3", "vers":"'.$version.'"}';
$host_world_res[25] = '{"url":"res/sound/wrong_turn.mp3", "vers":"'.$version.'"}';
$host_world_res[26] = '{"url":"res/sound/tick.mp3", "vers":"'.$version.'"}';
$host_world_res[27] = '{"url":"res/sound/training1.mp3", "vers":"'.$version.'"}';
$host_world_res[28] = '{"url":"res/sound/tourney1.mp3", "vers":"'.$version.'"}';
$host_world_res[29] = '{"url":"res/sound/buy1.mp3", "vers":"'.$version.'"}';
$host_world_res[30] = '{"url":"res/sound/buy2.mp3", "vers":"'.$version.'"}';
$host_world_res[31] = '{"url":"res/sound/click1.mp3", "vers":"'.$version.'"}';
$host_world_res[32] = '{"url":"res/sound/error1.mp3", "vers":"'.$version.'"}';
$host_world_res[33] = '{"url":"res/sound/battle1.mp3", "vers":"'.$version.'"}';
$host_world_res[34] = '{"url":"res/sound/click2.mp3", "vers":"'.$version.'"}';
$host_world_res[35] = '{"url":"res/sound/duel1.mp3", "vers":"'.$version.'"}';
$host_world_res[36] = '{"url":"res/sound/exit.mp3", "vers":"'.$version.'"}';
$host_world_res[37] = '{"url":"res/sound/loss1.mp3", "vers":"'.$version.'"}';
$host_world_res[38] = '{"url":"res/sound/merit.mp3", "vers":"'.$version.'"}';
$host_world_res[39] = '{"url":"res/sound/win1.mp3", "vers":"'.$version.'"}';
$host_world_res[40] = '{"url":"res/sound/win2.mp3", "vers":"'.$version.'"}';
$host_world_res[41] = '{"url":"res/sound/win3.mp3", "vers":"'.$version.'"}';
$host_world_res[42] = '{"url":"res/sound/care3.mp3", "vers":"'.$version.'"}';
$host_world_res[43] = '{"url":"res/sound/care3_1.mp3", "vers":"'.$version.'"}';
$host_world_res[44] = '{"url":"res/sound/drop1.mp3", "vers":"'.$version.'"}';
$host_world_res[45] = '{"url":"res/sound/drop2.mp3", "vers":"'.$version.'"}';
$host_world_res[46] = '{"url":"res/sound/main_mus.mp3", "vers":"'.$version.'"}';
$host_world_res[47] = '{"url":"res/sound/game_mus.mp3", "vers":"'.$version.'"}';
$host_world_res[48] = '{"url":"res/sound/game_mus1.mp3", "vers":"'.$version.'"}';
$host_world_res[49] = '{"url":"res/sound/game_mus2.mp3", "vers":"'.$version.'"}';
$host_world_res[50] = '{"url":"res/sound/ambient1.mp3", "vers":"'.$version.'"}';
$host_world_res[51] = '{"url":"res/sound/ambient2.mp3", "vers":"'.$version.'"}';
$host_world_res[52] = '{"url":"res/sound/ambient3.mp3", "vers":"'.$version.'"}';
$host_world_res[53] = '{"url":"res/sound/ambient4.mp3", "vers":"'.$version.'"}';
$host_world_res[54] = '{"url":"res/sound/ambient5.mp3", "vers":"'.$version.'"}';
$host_world_res[55] = '{"url":"res/sound/day_prize.mp3", "vers":"'.$version.'"}';
$host_world_res[56] = '{"url":"res/sound/game_mus3.mp3", "vers":"'.$version.'"}';



$world_name[1] = 'Центр';
$world_name[2] = 'Центр';
$world_name[3] = 'Центр';


if ((intval($sn_id)!=0) ) {
    $user_auth = getWorldId($sn_prefix, $auth_key, $sn_id);
} else {
    exit('{"error": {"type":"1", "val":"2"}}');
}

$world_id = intval($user_auth['world_id']);

if (intval($world_id)==0) {
    exit('{"error": {"type":"1", "val":"3"}}');
}



$toSend = '{"activate":{  '
    .             '"data0": {   "val1": "'.$user_auth['username'].'",   "val0": "'.$user_auth['password'].'" }, '
    .          '"reshosts": [  '.$host_world[1].'  ], '
    .'"res": [';

foreach ($host_world_res as $key => $value) {
	if ($key==0) {
    	$toSend .=$value;
	}

	$toSend .=','.$value;
    }

$toSend .=   '], '
    .       ' "gamemaster": "game@echo.poligon.xlab.su",   "connecttohost":"poligon.xlab.su",  "connecttoport":"443"  }}';
echo $toSend ;
//echo '<result><err code="0" link1="'.$host_world[$world_id].'" link2="'.$host_world_res[$world_id].'" world_num="'.$world_id.'" world_name="'.$world_name[$world_id].'" '.$in_val[$world_id].'  reff="'.$version.'" /></result>';


function getWorldId($sn_prefix, $auth_key, $sn_id)
{
	global $conn;
	global $memcache_world;

	$out['world_id'] = 0;

	switch ($sn_prefix)
			{
				case 'vk':
						$api_id = 111;
						$api_secret = "";

						$get_av  = md5($api_id.'_'.$sn_id.'_'.$api_secret);
						break;
				case 'ml':
						$api_id = 111;
						$api_secret = "";

						$get_av  = md5($api_id.'_'.$sn_id.'_'.$api_secret);

						break;

				case 'ok':
						$api_id = 111;
						$api_secret = "";

						$auth_key_out = explode('|', $auth_key);
						$auth_key = $auth_key_out[0];
						$session_key = $auth_key_out[1];

						$get_av =  md5($sn_id.$session_key.$api_secret);
						break;

				default:  $get_av=''; $auth_key = '';
			}


	if ($get_av==$auth_key)
	{


		$user_name = $sn_prefix.'_'.$sn_id;



		if (intval($out['world_id'])==0) {
			$out['world_id']=getNewWorldId($sn_prefix);
			$out['username'] = $user_name;
			$out['password'] =  md5($user_name);
			//md5($user_name.'_'.time());

		}
	}



	return $out;
}

function getNewWorldId($sn_prefix)
{


if (($sn_prefix=='vk') || ($sn_prefix=='ml') || ($sn_prefix=='ok'))
	{
	$world_id = 1;

	} else $world_id=0;
	return $world_id;
}
