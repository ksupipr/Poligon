<?

if (isset($_POST["dir"])) {

$fnow = $_POST["fnow"];
$dir = $_POST["dir"];

$d = dir($dir);

echo '<style>

.img_dir {
    float:left;
    width:140px;
    height:140px;
    text-align:center;
    background:#fff;
    border:1px solid #888;
    margin:2px;
    overflow:hidden;
    padding:2px;
}

.select_img {
    border:1px solid #ff0000;
    outline:2px solid #ff0000;
}
</style>

<script>
$(function(){
    $("#imgs_list .img_dir").click(function () {
        $("#imgs_list .img_dir").removeClass("select_img");
        $(this).addClass("select_img");
    });
});
</script>

';


echo '<div id="imgs_list">';

echo '<dir class="img_dir"><img title="" src=""/></dir>';

while (false !== ($entry = $d->read())) {
    if (($entry!='.') and ($entry!='..') and (!is_dir($dir.$entry))) {
        if ($fnow == $dir.$entry) $class_s = 'select_img'; else $class_s = '';
        echo '<dir class="img_dir '.$class_s.'"><img title="'.$dir.$entry.'" src="'.$dir.''.$entry.'"/></dir>';
    }
}
$d->close();
}

echo '</div>';
    
?>