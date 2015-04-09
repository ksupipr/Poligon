<?

function getBonusName($bid) {
$BonusName[0] = 'Напалм';
$BonusName[1] = 'Крест';
$BonusName[2] = 'Рикошет';
$BonusName[3] = 'Атака';
$BonusName[4] = 'Защита';
$BonusName[5] = 'Свобода';

if (!(is_integer($bid))) $bid = intval($bid);

if (isset($BonusName[$bid])) return $BonusName[$bid];
else return '';

}

function make_add_descr($add_params) {
    //{[],0,[]}
    $result = '';
    



require_once('EPP/lib/erlang_term_parser.php');


$term = erl_parse_term($add_params);



    $TopList = $term[data][0][data];

    for ($i=0; $i<count($TopList); $i++) {
        $result .= $TopList[$i][data][1].' к рейтингу, '; 
    }

    if (intval($term[data][1])>0) {
           $result .= 'монеты войны ('.intval($term[data][1]).' шт.), '; 
    }

    $BonusList = $term[data][2][data];

    for ($i=0; $i<count($BonusList); $i++) {
        if (!is_array($BonusList[$i][data][0])) {
            $result .= 'бонус «'.getBonusName(intval($BonusList[$i][data][0])).'» ('.$BonusList[$i][data][1].' шт.), '; 
        } else $result .= 'случайный бонус ('.$BonusList[$i][data][1].' шт.), '; 
    }



    return mb_substr($result,0, -2, 'utf-8').'.';
}



if (isset($_FILES['datafile'])) {

$new_json = '';

    $fh = fopen($_FILES['datafile']['tmp_name'], 'r');
    $data = fread($fh, filesize($_FILES['datafile']['tmp_name']));
    $json_data = json_decode($data);


    if (isset($json_data->merits)) {
        foreach ($json_data->merits as $l_now => $l_now_val) {
                $addt = make_add_descr($l_now_val->add_params);
                if ($addt!='.') $addt = '[color=\"#003300\" size=\"10\" font=\"Verdana\" leading=\"-1.5\"]'.$addt;
                else $addt = '';
                $l_now_val->add_descr=$addt;
                $json_data->$l_now = $l_now_val;
        }

require_once('EPP/JSON.php');

$json = new Services_JSON();


echo($json->encode($json_data));



    } else {
        echo 'WTF?';
    }   
} else {
?>

<form enctype="multipart/form-data" method="post">
<p>
<input type="file" name="datafile" size="40">
</p>
<div>
<input type="submit" value="Send">
</div>
</form>

<?
}
?>