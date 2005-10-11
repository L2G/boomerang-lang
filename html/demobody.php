<?php

############################
# Configuration parameters #
############################
# $enabledebug = TRUE;
# $enablelogging = TRUE;

$defaultdemogroup = "basics";

# relative to the individual demo directory
$logfile_locations = 
  array("../../../harmonywebdemo.log",  # pub/cgi on fling-l
        "../../log.tmp"                 # harmony on localhost
        );

##################
# Load Form Data #
##################

function get_post_data ($s) { return str_replace("\r","",stripslashes($_REQUEST[$s])); }

$reset = $_REQUEST['reset'];
$choosenewgroup = $_REQUEST['choosenewgroup'];
$choosenewnumber = $_REQUEST['choosenewnumber'];
$nextpart = $_REQUEST['nextpart'];
$prevpart = $_REQUEST['prevpart'];
$r1 = get_post_data('r1_text');
$r2 = get_post_data('r2_text');
$ar = get_post_data('ar_text');
$l1 = get_post_data('l1_text');
$l2 = get_post_data('l2_text');
$schema = get_post_data('schema_text');
$demogroup = $_REQUEST['demogroup'];
$demonumber = $_REQUEST['demonumber'];

debug ('$_REQUEST', $_REQUEST);

$text_height_diff = 21; 
$icon_width = 24;

##################
# Load Demo Data #
##################

chdir("../examples");

$alldemos = array();

function savedemo () {
  global $demo, $demos, $lastdemo;
  $temp = $demo;
  $demos[] = $temp;
}

function get_demos_from ($subdir) {
  global $demo, $demos, $alldemos;
  $demo = array();
  $demos = array(0 => "");  # so that the real contents are 1-indexed
  $f = $subdir . "/demos.php";
  if (file_exists($f)) {
    include($f);
    $demos["demogroupname"] = $demogroupname;
    $alldemos[$subdir] = $demos;
  }
}

get_demos_from("basics");
$alldemos[] = array("header" => "Tutorials");
get_demos_from("addresses");
get_demos_from("structuredtext");
get_demos_from("lenses");
$alldemos[] = array("header" => "Expert");
get_demos_from("bookmarks");
get_demos_from("calendars");
get_demos_from("relational");
get_demos_from("xmi");
get_demos_from("rawsync");

# print_r ($alldemos);

#####################
# Set Up Parameters #
#####################

if (empty($demogroup)) {
  $demogroup = $defaultdemogroup;
  $demonumber = 1;
  $reset = "YES";
  $changegroup = TRUE;
}

if (!empty($choosenewgroup)) {
  $reset = "YES";
  $demonumber = "1";
  $choosenewnumber = "";
  $nextpart = "";
  $prevpart = "";
  $changegroup = TRUE;
}

if (!empty($choosenewnumber)) {
  $reset = "YES";
  $nextpart = "";
  $prevpart = "";
}

if (!empty($nextpart)) {  
  $reset = "YES";
  $prevpart = "";
  if (!empty($alldemos[$demogroup][$demonumber+1])) {
    $demonumber = $demonumber+1;
  } else {
    reset($alldemos);
    while (current($alldemos) and key($alldemos) !== $demogroup) {
      next($alldemos);
    }
    next($alldemos);
    $temp = current($alldemos); $temp2 = $temp["header"];
    while (!empty($temp2)) { $temp = next($alldemos); $temp2 = $temp["header"]; }
    if (current($alldemos)) {
      $demogroup = key($alldemos);
      $demonumber = 1;
      $changegroup = TRUE;
    } else {
      $shownextparterror = "<br><b>No more demos</b>";
    }
  }
}

if (!empty($prevpart)) {
  $reset = "YES";
  if (!empty($alldemos[$demogroup][$demonumber-1])) {
    $demonumber--;
  } else {
    $shownextparterror = "<br><b>No Previous demos</b>";
  }
}

chdir($demogroup);

##################
# Read Demo Data #
##################

function demoparam($n) {
  global $demogroup, $demonumber, $alldemos;
  return $alldemos[$demogroup][$demonumber][$n];
}

$instr = demoparam("instr");
$r1format = demoparam("r1format");
$forcer1 = demoparam("forcer1");
$r2format = demoparam("r2format");
$arformat = demoparam("arformat");
$flags = demoparam("flags");
$democmd = demoparam("democmd");

$d_instr_w = demoparam("instr_w");
$d_instr_h = demoparam("instr_h");
$d_instr_d = demoparam("instr_d");

$d_r1 = demoparam("r1");
$d_r2 = demoparam("r2");
$d_ar = demoparam("ar");
$d_l1 = demoparam("l1");
$d_l2 = demoparam("l2");
$d_la = demoparam("la");
$d_schema = demoparam("schema");
$d_default_w = demoparam("default_w");
$d_default_wide_w = demoparam("default_wide_w");
$d_default_h = demoparam("default_h");

#####################
# Set Up Parameters #
#####################

$default_w = $d_default_w ? $d_default_w : 450;
$default_wide_w = $d_default_wide_w ? $d_default_wide_w : ($default_w * 2 + 10);
$default_h = $d_default_h ? $d_default_h : 175;

$state_hash = 
  array("instr"  => array("title" => "Instructions", "w" => $default_wide_w, "d" => "block", "icon" => "instr", "text" => false),
        "r1"     => array("title" => "Replica #1",   "h" => $default_h, "w" => $default_w, "d" => "block", "icon" => "replica",  "text" => true,  "ro" => false,),
        "r2"     => array("title" => "Replica #2",   "h" => $default_h, "w" => $default_w, "d" => "block", "icon" => "replica",  "text" => true,  "ro" => false,),
        "l1"     => array("title" => "Lens #1",      "h" => $default_h, "w" => $default_w, "d" => "none", "icon" => "lens",     "text" => true,  "ro" => false,),
        "l2"     => array("title" => "Lens #2",      "h" => $default_h, "w" => $default_w, "d" => "none", "icon" => "lens",     "text" => true,  "ro" => false,), 
        "a1"     => array("title" => "Abstract #1",  "h" => $default_h, "w" => $default_w, "d" => "none", "icon" => "abstract", "text" => true,  "ro" => true,),
        "a2"     => array("title" => "Abstract #2",  "h" => $default_h, "w" => $default_w, "d" => "none", "icon" => "abstract", "text" => true,  "ro" => true,),
        "ar"     => array("title" => "Archive",      "h" => $default_h, "w" => $default_w, "d" => "none", "icon" => "archive",  "text" => true,  "ro" => true,),
        "schema" => array("title" => "Schema",       "h" => $default_h, "w" => $default_w, "d" => "none", "icon" => "schema",   "text" => true,  "ro" => false,),
        "output" => array("title" => "Output",       "h" => $default_h, "w" => $default_wide_w, "d" => "none", "icon" => "output",   "text" => true,  "ro" => true,)
      );

$icons = 
  array("statusok" => array("dummy" => true, "desc" => "OK"            ),
        "spacer1"   => array("dummy" => true, "file" => "clear"),
        "sync"     => array("desc" => "Synchronize" ),
        "prev"     => array("desc" => "Previous Demo"        ),
        "reset"    => array("desc" => "Reset"           ),
        "next"     => array("desc" => "Next"            ),
        "spacer2"   => array("dummy" => true, "file" => "clear"),
        "instr"    => array("desc" => "Show Instructions",   "shows" => array("instr")),
        "replica"  => array("desc" => "Show Replicas",       "shows" => array("r1", "r2")),
        "abstract" => array("desc" => "Show Abstract Trees", "shows" => array("a1", "a2")),  
        "schema"   => array("desc" => "Show Schema",         "shows" => array("schema")),
        "archive"  => array("desc" => "Show Archive",        "shows" => array("ar")),
        "lens"     => array("desc" => "Show Lens",           "shows" => array("l1","l2")),
        "output"   => array("desc" => "Show Output",         "shows" => array("output"))
      );
if ($demonumber == 1) { array_splice($icons, 3, 1); }

//"experton" => array("desc" => "Expert Mode ON"       ),
//"prevset"  => array("desc" => "Previous Demo Set"    ),
//"nextset"  => array("desc" => "Next Demo Set"        ),
$expert = false;
function pick($expert, $f, $d, $s) { 
  global $reset;
  if($reset) { return $d ? $d : $s; } //ignore form data, it's bogus
  else {
    //   if($expert) { return $f ? $f : ($d ? $d : $s); }
    // else { return $d ? $d : ($f ? $f : $s); }
    return $f ? $f : ($d ? $d : $s);
  }
}

foreach ($state_hash as $k=>$v) {
  $state_hash[$k]['h'] = pick($expert, get_post_data($k . "_h"), demoparam($k . "_h"), $v['h']);
  $state_hash[$k]['w'] = pick($expert, get_post_data($k . "_w"), demoparam($k . "_w"), $v['w']);
  $state_hash[$k]['d'] = pick($expert, get_post_data($k . "_d"), demoparam($k . "_d"), $v['d']);
}

foreach($icons as $i=>$v) {
  $icons[$i]['active'] = "false";
  if(! $v['shows']) { 
    $icons[$i]['active'] = "true";
  } else {
    foreach($v['shows'] as $d) {      
      if($state_hash[$d]['d'] == "none") { $icons[$i]['active'] = "true"; break; }
    }
  }
}

if ($changegroup) { $reset = "YES"; }

if (!empty($reset)) {
  $r1 = $d_r1;
  $r2 = $d_r2;
  $ar = $d_ar;
  $l1 = $d_l1;
  $l2 = $d_l2;
  $la = $d_la;
  $schema = $d_schema;
}
$l1 = $l1 ? $l1 : "Prelude.id";
$l2 = $l2 ? $l2 : "Prelude.id";
$la = $la ? $la : "Prelude.id";
$schema = $schema ? $schema : "Prelude.Any";

###########
# Logging #
###########

if($enablelogging) {
  /* NB: gethost will hang a few seconds if offline */
  $remote = trim(gethost($GLOBALS["HTTP_SERVER_VARS"]["REMOTE_ADDR"]));
  $date = date("Y/m/j G:i:s T");
  $logmsg = "$date  $remote  ($demogroup / $demonumber)\n";

  foreach ($logfile_locations as $name) {
    $handle = @fopen($name, 'a');
    if ($handle) {
      echodebug ("Log message written to $name");
      fwrite($handle, $logmsg);
      fclose($handle);
    } else {
      echodebug ("Could not open $name");
    }
  }
}

###############
# Run Harmony #
###############
if (empty($democmd)) $democmd = "harmonize-" . $demogroup;

$tempbasename = "h" . posix_getpid() . str_replace(array(" ","."),"",microtime());
$tempdir = "/tmp";
$tempbase = "$tempdir/$tempbasename";

if (empty($arformat)) $arformat = "meta";

$r1file = $tempbase . "r1." . $r1format;
$r2file = $tempbase . "r2." . $r2format;
$arfile = $tempbase . "ar." . $arformat;
$newr1file = $tempbase . "newr1." . $r1format;
$newr2file = $tempbase . "newr2." . $r2format;
$newarfile = $tempbase . "newar." . $arformat;

put_file($r1file, $r1);
put_file($r2file, $r2);
put_file($arfile, $ar);

$lensmodule = $tempbasename . "lens";
$lensModule = ucfirst($lensmodule);
$lensfile = "$tempdir/$lensmodule.fcl";
$lensfilecontents = 
  "module $lensModule =\n"
  . "let l1 : lens = \n"
  . "# 0 \"NOFILEHERE\"\n"
  . $l1 . "\n\n"
  . "let l2 : lens = \n"
  . "# 0 \"NOFILEHERE\"\n"
  . $l2 . "\n\n"
  . "let la : lens = \n"
  . "# 0 \"NOFILEHERE\"\n"
  . $l1 . "\n\n"
  . "schema S = \n"
  . "# 0 \"<schema>\"\n"
  . $schema . "\n";
put_file($lensfile, $lensfilecontents);

if (!file_exists($democmd)) {
  abort("Executable " . $democmd . " not found in " . getcwd(),"");
}

$cmdbase = 
    "export HOME=../../../..; "
  . "export FOCALPATH=.:../../lenses:/$tempdir;"
  . "./$democmd $flags";

$cmd = 
    $cmdbase 
  . "-r1 $r1file "
  . "-r2 $r2file "
  . "-ar $arfile "
  . "-lensr1 $lensModule.l1 "
  . "-lensr2 $lensModule.l2 "
  . "-lensar $lensModule.la "
  . "-newar $newarfile " 
  . "-newr1 $newr1file " 
  . "-newr2 $newr2file " 
  . "-schema $lensModule.S "
  . ($forcer1 ? "-forcer1 " : "")
  . "2>&1";
debug ('$cmd',$cmd);
$output = shell_exec($cmd);

if (file_exists($newarfile) && file_exists($newr1file) && file_exists($newr2file)) {
  $ar = filecontents($newarfile);
  $r1 = filecontents($newr1file);
  $r2 = filecontents($newr2file);
} else {
  array_shift($icons);
  array_insert($icons, 0, array("statusnotok" => array("dummy" => "true", "desc" => "ERROR")));
  $state_hash['output']['d'] = "block";
  $icons['output']['active'] = "false";  
  $icons['statusnotok']['active'] = "true";  
}

# generate abstract versions of the two (new) replicas
  $getcmd = $cmdbase 
  . (file_exists($newr1file) ? "$newr1file " : "$r1file ")
  . (!empty($l1) ? "-lensr1 $lensModule.l1 " : "")
  . "2>&1";
  $a1 = shell_exec($getcmd);
  $getcmd = 
      $cmdbase 
    . (file_exists($newr2file) ? "$newr2file " : "$r2file ")
    . (!empty($l2) ? "-lensr1 $lensModule.l2 " : "")
    . "2>&1";
  $a2 = shell_exec($getcmd);

############
# Response #
############

print <<<EOF
<html>
<head>
<title>Harmony</title>

<!------------ css ------------>
<style type="text/css">
body {
  background: #eeeeee; 
  font-family: arial, sans-serif;
  font-size: small;
}

#surtitle { 
  font-size:xx-small; 
  font-weight:bold;
}

#header {
  position:relative;
  margin: 3px 3px 5px 3px;
  width:100%;
  min-width:500px;
  height:60px;
}

#logo { 
  position:absolute;
  top:0px; left:0px
  margin:0px;
  width:164px;
  height:52px;
  background-image:url("images/harmony-logo-small.png");
}

#control { 
  min-width:280px;
  margin-left:175px;
  margin-right:200px;
}

#selector { 
  position:absolute;
  top:18px;
  right:0px;
  width:200px;
  height:100%;
}

.title { 
  position:relative;
  left:0px;
  top:0px;
  background-color:#99ccff;  
  border:1px solid #bbbbbb;
  margin-bottom:3px;
  font-weight:bold;
  text-align:center;
}

.min { 
  position:absolute;
  height:11px; width:11px;
  top:1px; right:1px;
  padding:0px;
  margin:0px;
  cursor:default;
  background-image:url("images/close.png")
}


.grip { 
  position:absolute;
  bottom:1px;
  right:1px;
  padding:0; 
  margin:0;
  height:11px;
  width:11px;
  cursor:move;
  background-image:url("images/grip.png");
}

.resize { 
  position:absolute;
  bottom:0px;
  right:0px;
  cursor:move;
}

.bot { vertical-align:bottom; }
.center { margin-left:auto; margin-right:auto; }

.group { float:left; }
.hide { width:24px; height:24px; display:none; }

.box { 
  position:relative;
  float:left;
  border:1px solid #bbbbbb;
  background-color:#ffffcc;
  padding:3px;
  margin-right:3px;
  margin-bottom:3px;
}
      
textarea { background:#ffffee; }
.spacer { clear:both; }

</style>

<!------------ javascript ------------>
<script type="text/javascript">

/* globals */
var x = 0;
var y = 0;
var clicked = false;
var clicked_ids = [];

var state = {
EOF;
$sep = "\n\t";
$text = "_text";
foreach($state_hash as $k=>$v) {    
  $h = $v['h'] ? ("h:" . $v['h'] . ",") : "";   
  $w = $v['w'] ? ("w:" . $v['w'] . ",") : "";   
  $d = $v['d'];
  print($sep . $k . ':{' . $w . $h . 'd:"' . $d . '"}');
  $sep = ",\n\t";
  if($v['text']) {
    $h = $v['h'] ? ("h:" . ($v['h'] - $text_height_diff ). ",") : "";
    $w = $v['w'] ? ("w:" . $v['w'] . ",") : "";
    $d = $v['d'];
    print($sep . $k . '_text:{' . $w . $h . 'd:"' . $d . '"}');
  }
}
print("\n};\n");

print "var icons = {";
$sep = "";
foreach($icons as $i=>$v) {
  print ($sep . '"' . $i . '"' . ':' . $v['active']);
  $sep = ", ";
}
print "};\n";
print "function active_icons() { var n = 0; for(i in icons) { if(icons[i]) { n++; } }; return n; }\n";
print "function icon_index(i) { var n = 0; for(j in icons) { if(i == j) { return n; } if (icons[j]) { n++; } } return -1; }\n";

foreach($state_hash as $k=>$v) {
  $resize = $k . "_resize";
  $hide = $k . "_hide";
  $text = $v['text'] ? ('", "' . $k . '_text') : "";    
  $icon_name = $v['icon'];
  $icon = $icon_name . "_icon";

print <<<EOF
function $resize(e) { return setup_resize(e, ["$k$text"]); }
function $hide(e) { 
  state.$k.d = "none"; 
  document.getElementById("$k").style.display = "none"; 
  if(! icons["$icon_name"]) { 
    icons["$icon_name"] = true;
    document.getElementById("$icon").style.display = "inline"; 
  } 
}

EOF;
}

print <<<EOF
function setup_resize(e, ids) {
  var style = document.getElementById(ids[0]).style;  
  x = e.clientX;
  y = e.clientY;
  clicked = true;
  clicked_ids = ids;
  style.borderStyle = "dashed";
  style.borderColor = "#000000";
  style.backgroundColor = "#ffffee";
  return;
}
 
function mouse_up(e) {
  if (clicked) {
    clicked = false;
    var style = document.getElementById(clicked_ids[0]).style;
    style.borderStyle = "solid";
    style.borderColor = "#bbbbbb";
    style.backgroundColor = "#ffffcc";
    clicked_ids = [];
  }
  return;
}

function mouse_move(e) {
  if(clicked) {
    var dx = e.clientX - x;
    var dy = e.clientY - y;
    x = x + dx;
    y = y + dy;
    for (i=0; i < clicked_ids.length; i++) {
      var style = document.getElementById(clicked_ids[i]).style;
      var clicked_data = state[clicked_ids[i]];
      clicked_data.w += dx;
      clicked_data.h += dy;
      style.width = clicked_data.w + "px";
      style.height = clicked_data.h + "px";
    }
  }
  return;
}

function show_st(icon) {
   var surtitle = document.getElementById("surtitle");
   var desc = document.getElementById(icon + "_icon").alt;
   var total_icons = active_icons();
   var n = icon_index(icon);
   if (n < (total_icons / 2)) {
     surtitle.style.textAlign = "left";
     var offset = n * $icon_width;
     var spacer = '<img src="images/clear.gif" alt="spacer" height="1" width="'+offset+'" >';
     surtitle.innerHTML = spacer + desc;
   } else {
     surtitle.style.textAlign = "right";
     var offset = (total_icons - n - 1) * $icon_width;
     var spacer = '<img src="images/clear.gif" alt="spacer" height="1" width="'+offset+'">';
     surtitle.innerHTML = desc + spacer;
  }
}

function hide_st() { document.getElementById("surtitle").innerHTML="&nbsp;"; }

function sync_click() { go_submit(); }
function reset_click()    { document.theform.reset.value = true; go_submit(); }
function next_click()     { document.theform.nextpart.value = true; go_submit(); }
function prev_click()     { document.theform.prevpart.value = true; go_submit(); }


EOF;

print "function go_submit()     {\n";
foreach($state_hash as $k=>$v) {
  print("  document.theform." . $k . "_h.value = state." . $k . ".h;\n");
  print("  document.theform." . $k . "_w.value = state." . $k . ".w;\n");
  print("  document.theform." . $k . "_d.value = state." . $k . ".d;\n");
}
print "  document.theform.submit();\n}\n";

foreach($icons as $k=>$v) {
  $n = $v['n'];
  $over = $k . "_over";
  $icon = $k . "_icon";
  $click = $k . "_click";
  if($v['shows']) {
    print "function $click() {\n";
    print "  hide_st()\n";
    print "  icons[\"$k\"] = false;\n";
    print "  document.getElementById(\"$icon\").style.display = \"none\";\n"; 
    foreach($v['shows'] as $d) { 
      print "  state.$d.d = \"block\";\n";
      print "  document.getElementById(\"$d\").style.display = \"block\";\n";
    }
    echo "}\n";
  }
  echo "function $over(e) { show_st(\"$k\"); }\n";
}
    

print "function load() {\n";
print "  var s;\n";
print "  for(i in icons) { if(icons[i]) { document.getElementById(i + '_icon').style.display = 'inline'; } }\n"; 
foreach($state_hash as $k=>$v) {
  print "  s = document.getElementById(\"$k\").style;\n";
  print "  s.display = state.$k.d;\n";
  print "  if(state.$k.w) { s.width = state.$k.w + 'px'; }\n";
  print "  if(state.$k.h) { s.height = state.$k.h + 'px'; }\n";
  if($v['text']) {
    $text = $k . "_text";
    print "  s = document.getElementById(\"$text\").style;\n";
    print "  if(state.$k.w) { s.width = (state.$k.w) + 'px'; }\n";
    print "  if(state.$k.h) { s.height = (state.$k.h - $text_height_diff) + 'px'; }\n";
  }
}
print "  document.addEventListener(\"mouseup\", mouse_up, true);\n";
print "  document.addEventListener(\"mousemove\", mouse_move, true);\n";

foreach($state_hash as $k=>$v) {
  $grip = $k . "_grip";
  $resize = $k . "_resize";
  $min = $k . "_min";
  $hide = $k . "_hide";
  print <<<EOF
  document.getElementById("$grip").addEventListener("mousedown", $resize, true);
  document.getElementById("$min").addEventListener("click", $hide, true);

EOF;
}

foreach($icons as $k=>$v) {
  $icon = $k . "_icon";
  $over = $k. "_over";
  
  print <<<EOF
  document.getElementById("$icon").addEventListener("mouseover", $over, true);
  document.getElementById("$icon").addEventListener("mouseout", hide_st, true);

EOF;

  if(! $v['dummy']) { 
    $click = $k. "_click";
    print <<<EOF
  document.getElementById("$icon").addEventListener("click", $click, true);

EOF;
  }
}
print "}\n";

print <<<EOF

function select_demo_group() {
   document.theform.choosenewgroup.value="YES";
   go_submit();
   return true;
}
function select_demo_number() {
   document.theform.choosenewnumber.value="YES";
   go_submit();
   return true;
}

</script>
</head>

<body onload="load()">
  <form method="post" name="theform">
    <input type="hidden" value="" name="choosenewgroup"/>  
    <input type="hidden" value="" name="choosenewnumber"/>  
    <input type="hidden" value="" name="prevpart"/>      
    <input type="hidden" value="" name="nextpart"/>  
    <input type="hidden" value="" name="reset"/>
    <div id="header">
      <div id="logo"></div>
      <div id="control">
        <table class="center">
          <tr><td><div id="surtitle">&nbsp;</div></td></tr>
          <tr><td>

EOF;
foreach($icons as $k=>$v) {
  $id = $k . "_icon";
  $src = "images/" . ($v['file'] ? $v['file'] : $k) . ".png";
  $desc = $v['desc'] ? $v['desc'] : "&nbsp;";
 print "                <img id=\"$id\" class=\"hide\" src=\"$src\" alt=\"$desc\">\n"; 
}

print <<<EOF
           </td>
          </tr>
        </table>
      </div>
      <div id="selector">
      <!-- selector stuff -->
    <select name="demogroup" onchange="select_demo_group()">
    <optgroup label="Overview">

EOF;

foreach ($alldemos as $k=>$v) {
  if(!empty($v["header"])) {
    $header = $v["header"];
    print "</optgroup> <optgroup label=\"$header\">\n";
  } else {
    $name = $v["demogroupname"];
    $selected = "";
    if($k == $demogroup) $selected="selected";
    echo "<option $selected value=\"$k\">$name</option>";
  }
}
echo '</optgroup></select>';
echo '<select name="demonumber" onchange="select_demo_number()">';
$i = 1;
while (!empty($alldemos[$demogroup][$i])) {
 $selected = "";
 if ($i == $demonumber) { $selected="selected"; }
 print "<option $selected value=\"$i\">$i</option>\n";
 $i = $i + 1;
}
echo '</select>';
echo '</div>';
echo '</div>';
echo '<!-- main content -->';
echo '<div>';

/* generate the divs, titles, etc. */
foreach ($state_hash as $k=>$v) {
  $title = $v['title'];
  $grip = $k . "_grip";
  $min = $k . "_min";
  eval("\$k_val = \$$k;");
  $ro = $v['ro'] ? " readonly" : "";
  $text = $v['text'] ? "<textarea name=\"" . $k . "_text\" id=\"" . $k . "_text\"$ro>$k_val</textarea>" : "<p>$k_val</p>";
  $h = $k . "_h";
  $w = $k . "_w";
  $d = $k . "_d";
  print <<<EOF
  <div class="box" id="$k">
  <div class="title">$title<div class="min" id="$min"></div></div>
  <div>$text</div>
  <div class="grip" id="$grip"></div>
  </div>
  <input type="hidden" name="$w" value="0">
  <input type="hidden" name="$h" value="0">
  <input type="hidden" name="$d" value="none">


EOF;
}

$debugout = $enabledebug ? join("\n",$debuglog) . "\n" : "\n";
print <<<EOF
</form>$debugout
</body>
</html>
EOF;

###################################
# Misc Functions #
###################################

function array_insert (&$array, $position, $insert_array) {
  $first_array = array_splice ($array, 0, $position);
  $array = array_merge ($first_array, $insert_array, $array);
}

function put_file ($name, $contents) {
  $handle = fopen($name, 'w');
  fwrite($handle, $contents);
  fclose($handle);
}

function append_file ($name, $contents) {
  $handle = fopen($name, 'a');
  fwrite($handle, $contents);
  fclose($handle);
}

function listdir($dirname=".") {
   $files = array();
   if($handle = opendir($dirname)) {
       while(false !== ($file = readdir($handle)))
         $files[] = $file;
       closedir($handle);
   }
   return($files);
}

function filecontents($filename) {
  $handle = fopen($filename, "r");
  $contents = fread($handle, filesize($filename));
  fclose($handle);
  return $contents;
}

function abort($mesg, $more) {
  echo "<h2>Oops: " . $mesg . "</h2>\n";
  echo $more;
  exit(0);
}

function show($data, $func = "print_r", $return_str = false){
   ob_start();
   $func($data);
   $output = ''.htmlspecialchars(ob_get_contents()).'';
   ob_end_clean();
   if($return_str) return $output; else echo $output;
}

function echodebug($s) {
  global $debuglog;
  $debuglog[] = $s . "<br>";
}

function debug ($s,$v) {
  global $debuglog;
  $debuglog[] = '<hr width="50%"><p>';
  $debuglog[] = $s . " = ";
  $debuglog[] = show ($v, "print_r", TRUE);
  $debuglog[] = "</p>";
}

function gethost ($ip) {
 $host = `host $ip`;
 return (($host ? end ( explode (' ', $host)) : $ip));
}

?>
