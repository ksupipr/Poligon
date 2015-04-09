<form enctype="multipart/form-data" method="post">
<p>
<input type="file" name="datafile" size="40">
</p>
<div>
<input type="submit" value="Send">
</div>
</form>

<?

if (isset($_FILES['datafile'])) {

$new_json = '';

    $fh = fopen($_FILES['datafile']['tmp_name'], 'r');
    $data = fread($fh, filesize($_FILES['datafile']['tmp_name']));
    $json_data = json_decode($data);


    if (isset($json_data->merits)) {
        foreach ($json_data->merits as $l_now) {
            if ($l_now->type == 2) {

                $descr = $l_now->descr;

                $descr_to_br = explode('[br]', $descr);
                unset($descr_to_br[0]);
                $descr = '';
                foreach ($descr_to_br as $descr_to_br_now) {

                if (mb_strlen($descr_to_br_now, 'utf-8')>72) {
                    $descr_to_line = explode(' ', $descr_to_br_now);
                    $descr_to_br_now = '';
                    $count_ch = 0;
                    for ($i=0; $i<count($descr_to_line); $i++) {
                        $count_world = mb_strlen($descr_to_line[$i], 'utf-8')+1;
                        if (($count_world+$count_ch)>72) {
                            $count_ch = $count_world;
                            $descr_to_br_now .='[br]'.$descr_to_line[$i];
                        } else {
                            $count_ch = $count_world+$count_ch;
                            $descr_to_br_now .=' '.$descr_to_line[$i];
                        }
                        
                    }
                }
                    $descr .=$descr_to_br_now.'[br]';
                }

                $descr = mb_substr($descr, 0, -4, 'utf-8');

                $new_json .=
                '{
                  "id": '.$l_now->id.',
                  "name": "'.$l_now->name.'",
                  "img": "'.$l_now->img.'",
                  "descr": "'.addslashes($l_now->add_descr).'",
                  "need": [
                      "'.$descr.'"
                   ]
                },'."\n";
            }
        }

$final_json =
'{
    "orders": [
        '.mb_substr($new_json,0,-2, 'utf-8').'
    ]
}';

var_dump($final_json);

    } else {
        echo 'WTF?';
    }   
}

?>