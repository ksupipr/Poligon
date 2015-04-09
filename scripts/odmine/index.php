<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <title>Достижения</title>
        <link type="text/css" href="css/ui-lightness/jquery-ui-1.8.21.custom.css" rel="stylesheet" />
        <script type="text/javascript" src="js/jquery-1.7.2.min.js"></script>
        <script type="text/javascript" src="js/jquery-ui-1.8.21.custom.min.js"></script>
        <script type="text/javascript">
            $(function(){

                // Accordion
                $(".accordion").accordion({ autoHeight: false, header: "h3", collapsible: true, active: false });

                $( ".accordion" ).sortable({
                    placeholder: "ui-state-highlight"
                });
                //$( ".accordion" ).disableSelection();

                $('.icon_b').hover(
                    function() { $(this).addClass('ui-state-hover'); },
                    function() { $(this).removeClass('ui-state-hover'); }
                );

                $( "#edit_text_form" ).dialog({
                    autoOpen: false,
                    height: 300,
                    width: 350,
                    modal: true
                   
                });

                $( "#edit_img_form" ).dialog({
                    autoOpen: false,
                    height: 600,
                    width: 800,
                    modal: true
                   
                });



                $('.icon_b').click(
                    function() { iconClick($(this)); }
                );

                });

        function iconClick(obj_in) {
            var type = $(obj_in).attr('type');
                switch (type) {
                    case 'edit':  editClick(obj_in);
                                break;
                    default: break;
                }
        }


        function editClick(obj_in) {
            var target = $(obj_in).attr('target');
            var etype = $(obj_in).attr('etype');
            var parent_obj = $(obj_in).parent().find('.'+target);
            
            switch (etype) {
                    case 'text':  editText(parent_obj);
                                break;
                    case 'img':  editImg(parent_obj, $(obj_in).attr('dir'));
                                break;
                    default: break;
                }
        }
  
        function editText(text_obj) {
            var text = $(text_obj).html();
            $( "#edit_text_form" ).find('textarea').val(text);
            $( "#edit_text_form" ).dialog({
                close : function () { $(text_obj).html($(this).find('textarea').val()); }
            });
            $( "#edit_text_form" ).dialog( "open" );
            
        }

        function editImg(img_obj, dir) {
            var src = $(img_obj).attr('src');

               $( "#edit_img_form" ).dialog({
                open : function () { 
                        $.post('img_list.php', { fnow: src, dir: dir }, function(data) {
                            $( "#edit_img_form" ).html(data);
                        });
                 },
                close : function () {
                    var newsrc = $("#edit_img_form #imgs_list .select_img img").attr('src');
                        $(img_obj).attr({'src': newsrc});
                }
            });


            $( "#edit_img_form" ).dialog( "open" );
        }

        </script>
        <style>
            body {
                padding:40px; padding-bottom:60px;
            }
            .accordion {
                margin-left:40px; margin-right:60px;
            }
            .icon_b {margin: 2px; position: relative; padding: 2px 0; cursor: pointer;  list-style: none; display:inline-block;}
            .icon_b span.ui-icon {margin: 0 2px;}

            #edit_text_form textarea, #edit_text_form form { width:100%; height:100%; }

            .img_on td {
                margin:3px;
                text-align:center;
                background:#fff;
                border:#cfcfcf;
                width:150px;
                height:150px;
                vertical-align:top;                
            }

            .img_on td .icon_b {
                float:left;
                margin-right:-15px;
                margin-bottom:-15px;
                top:0px;
                left:0px;
            }
        </style>
    </head>
    <body>
        
        
<?
function ttypeOut($ttype) {
    switch (intval($ttype)) {
        case 1: $out = 'после боя';
                break;
        case 2: $out = 'раз в сутки';
                break;
        case 3: $out = 'раз в неделю';
                break;
        case 4: $out = 'раз в месяц';
                break;
        case 5: $out = 'особый ежедневный';
                break;
        default: $out = 'неизвестный';

    }
    return $out;
}

function stackedOut($stacked) {
    switch (intval($stacked)) {
        case 0: $out = 'не копятся';
                break;
        case 1: $out = 'копятся';
                break;
        default: $out = 'неизвестно';

    }
    return $out;
}

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

function make_need_achivs($need_achivs) {
//[{4,1,1},{15,1,0}]
global $all_merits;

    $term = erl_parse_term($need_achivs);

    $achivList = $term[data];

    $out = '<ul>';

    for ($i=0; $i<count($achivList); $i++) {
        $achivNow = $all_merits[intval($achivList[$i][data][0])];
        $name = $achivNow->name;
        $count = intval($achivList[$i][data][1]);
        $dell = intval($achivList[$i][data][2]);

        $out .= '<li>'.$name.' ('.$count.'шт.) удалить после получения: '.$dell.';</li> ';
    }
    $out .= '</ul>';

return $out;
}

function getSign($sign) {
    switch ($sign) {
        case 'equal': $out = '=';
                      break;
        case 'more': $out = '>=';
                      break;
        case 'less': $out = '=<';
                      break;
        case 'sumequal': $out = '=';
                      break;
        case 'summore': $out = '>=';
                      break;
        case 'sumless': $out = '=<';
                      break;
        case 'firstmore': $out = '>=';
                      break;
        case 'firstless': $out = '=<';
                      break;
        case 'sumfirstmore': $out = '>=';
                      break;
        case 'sumfirstless': $out = '=<';
                      break;
        default: $out = 'NaN';
    }

    return $out;
}

function getParams($params) {
        $out[0] = 0;
        $out[1] = 0;

        if (is_array($params)) {
            $param_data = $params['data'];
            $out[0] = '';
            if ($params['type']=='list') {
                
                for ($i=0; $i<count($param_data); $i++) {
                    if ($param_data[$i]>0)
                        $out[0] .= '+'.$param_data[$i];
                    else 
                        $out[0] .= $param_data[$i];

                }
            } else {
                for ($i=0; $i<count($param_data); $i++) {
                     if (is_array($param_data[$i])) {
                        $inn = getParams($param_data[$i]);
                        $out[$i] = $inn[0];
                     } else {
                        $out[$i] = $param_data[$i];
                     }
                }
            }
        } else {
            $out[0] = $params;
        }
    return $out;
}

function make_state_params($state_params) {


    $term = erl_parse_term($state_params);

    $paramsList = $term['data'];

    $out = '<ul>';


    for ($i=0; $i<count($paramsList); $i++) {
        $paramsNow = $paramsList[$i]['data'][0];
        $count = intval($paramsList[$i]['data'][1]);
        $sign = $paramsList[$i]['data'][2];
        $sign = $sign['data'];



        $params = getParams($paramsNow);

        if (($sign=='firstmore') || ($sign=='firstless') || ($sign=='sumfirstmore') || ($sign=='sumfirstless')) {
            $out .= '<li>'.$params[0].' '.getSign($sign).' '.$params[1].';</li> ';
        } else {
            $out .= '<li>'.$params[0].' '.getSign($sign).' '.$count.';</li> ';
        }
    }
    $out .= '</ul>';

return $out;
}

require_once('EPP/lib/erlang_term_parser.php');

    $fh = fopen('db/merits.txt', 'r');
    $data = fread($fh, filesize('db/merits.txt'));
    $json_data = json_decode($data);

// $term = make_state_params('[{{[67, 68], [69, 70]}, 0, sumfirstmore}]');


    if (isset($json_data->merits)) {
        foreach ($json_data->merits as $l_now) {
            $all_merits[intval($l_now->id)] = $l_now;
        }

        foreach ($json_data->merits as $l_now) {
    
            
    
            $out_type[intval($l_now->type)] .= '<div class="achiv_block"  id="achiv_'.$l_now->id.'" aid="'.$l_now->id.'">
                <h3><a><span class="name">'.$l_now->name.'</span></a></h3>
                <div>
                    <p>Название: <b class="name">'.$l_now->name.'</b><span type="edit" target="name" etype="text" class="icon_b ui-state-default ui-corner-all" title=".ui-icon-pencil"><span class="ui-icon ui-icon-pencil"></span></span></p>
                    <p>Краткое название: <b class="sname">'.$l_now->sname.'</b><span type="edit" target="sname" etype="text" class="icon_b ui-state-default ui-corner-all" title=".ui-icon-pencil"><span class="ui-icon ui-icon-pencil"></span></span></p>
                    <p>Описание: <span type="edit" target="descr" etype="text" class="icon_b ui-state-default ui-corner-all" title=".ui-icon-pencil"><span class="ui-icon ui-icon-pencil"></span></span><br/><span class="descr">'.$l_now->descr.'</span></p>

                    <table class="img_on">
                    <tr>
                        <td><span type="edit" target="img" etype="img" dir="res/images/achivs/" class="icon_b ui-state-default ui-corner-all" title=".ui-icon-pencil"><span class="ui-icon ui-icon-pencil"></span></span>
                            <img class="img" src="'.$l_now->img.'"/>
                        </td>
                        <td><span type="edit" target="icon" etype="img" dir="res/images/achivs/" class="icon_b ui-state-default ui-corner-all" title=".ui-icon-pencil"><span class="ui-icon ui-icon-pencil"></span></span>
                            <img class="icon" src="'.$l_now->icon.'"/>
                        </td>
                        <td><span type="edit" target="icon_off" etype="img"  dir="res/images/achivs/" class="icon_b ui-state-default ui-corner-all" title=".ui-icon-pencil"><span class="ui-icon ui-icon-pencil"></span></span>
                            <img class="icon_off" src="'.$l_now->icon_off.'"/>
                        </td>
                    </tr>
                    </table>
                    <div>Проверка: <b class="t_type" val="'.$l_now->t_type.'">'.ttypeOut($l_now->t_type).'</b></div>
                    <div>Хранение: <b class="stacked">'.stackedOut($l_now->stacked).'</b></div>
                    <div>Качество: <b class="level">'.$l_now->level.'</b></div>
                    <div>Условия по параметрам статистики: <div class="state_params" val="'.$l_now->state_params.'">'.make_state_params($l_now->state_params).'</div></div>
                    <div>Необходимы достижения: <b class="need_achivs" val='.$l_now->need_achivs.'>'.make_need_achivs($l_now->need_achivs).'</b></div>
                    <div>Вознаграждение: <b class="add_params" val="'.$l_now->add_params.'">'.make_add_descr($l_now->add_params).'</b></div>
                </div>
            </div>';
        }
    }

    $raz_now = 0;

    foreach ($out_type as $out_type_key => $out_type_now) {
        switch ($out_type_key) {
              case 1: $name_r = 'достижения';
                      break;
              case 2: $name_r = 'награды';
                      break;
              case 3: $name_r = 'звания';
                      break;
              case 4: $name_r = 'нефиксируемые достижения';
                      break;
        }
        if ($raz_now!=$out_type_key) {
            if ($raz_now!=0) echo '</div></div>';
            $raz_now=$out_type_key;
            echo '<div id="razdel_'.$out_type_key.'">
                <h3>'.$name_r.'</h3><div class="accordion" id="accordion'.$out_type_key.'">';
        }
        echo  $out_type_now;
    }

    echo '</div></div>';
?>

           
<div id="edit_text_form">
    <form>
        <textarea></textarea>
    </form>
</div>

<div id="edit_img_form">

</div>

    </body>
</html>