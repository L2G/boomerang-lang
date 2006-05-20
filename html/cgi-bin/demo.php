<?php
#################################################################
# The Harmony Project
# harmony@lists.seas.upenn.edu
#
# demo.php - Live web demo
################################################################
# $Id: demo.php 1188 2006-01-18 23:50:17Z jnfoster $

$home = getcwd();

############################
# Configuration parameters #
############################
#$enabledebug = TRUE;
$enablelogging = TRUE;

$defaultdemogroup = "basics";

$logfile_locations = array("$home/webdemo.log");

##################
# Load Form Data #
##################

function get_post_data ($s) { return str_replace("\r","",stripslashes($_REQUEST[$s])); }

$submitter = $_REQUEST['submitter'];
$r1 = get_post_data('r1_text');
$r2 = get_post_data('r2_text');
$ar = get_post_data('ar_text');
$l1 = get_post_data('l1_text');
$l2 = get_post_data('l2_text');
$schema = get_post_data('schema_text');
$demogroup = $_REQUEST['demogroup'];
$demonumber = $_REQUEST['demonumber'];
$expert = defined($_REQUEST['expert']) ? ($_REQUEST['expert'] == "true") : false;

debug ('$_REQUEST', $_REQUEST);

$text_height_diff = 21; 
$icon_width = 36;
$min_width = 100;
$min_height = 75;

$reset = false;
$error = false;
$body_background = "#eeeeee";
$surtitle_text = "&nbsp;";

##################
# Load Demo Data #
##################

chdir("examples");

$alldemos = array();

function savedemo () {
  global $demo, $demos, $lastdemo;
  $temp = $demo;
  $demos[] = $temp;
}

function get_demos_from ($subdir) {
  global $demo, $demos, $alldemos;
  $demo = array();
  $demos = array(0 => "");      # make contents 1-indexed 
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
#get_demos_from("xmi");
get_demos_from("rawsync");
get_demos_from("talkdemos");

#####################
# Set Up Parameters #
#####################

if (empty($demogroup)) {
  $demogroup = $defaultdemogroup;
  $demonumber = 1;
  $reset = true;
}

if ($submitter == "sync") {
} else if ($submitter == "reset") { 
  $reset = true;
} else if ($submitter == "select_group") {
  $reset = true;
  $demonumber = "1";
} else if ($submitter == "select_number") {
  $reset = true;
} else if ($submitter == "next") {
  $reset = true;
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
    } else {
      $shownextparterror = "<br><b>No more demos</b>";
    }
  }
} else if ($submitter == "prev") {
  $reset = true;
  if (!empty($alldemos[$demogroup][$demonumber-1])) {
    $demonumber--;
  } else {
    $shownextparterror = "<br><b>No Previous demos</b>";
  }
} else if ($submitter == "expert") {
  #toggle expert mode
  $expert = $expert ? false : true;
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
$splash = demoparam("splash");
$r1format = demoparam("r1format");
$r2format = demoparam("r2format");
$arformat = demoparam("arformat");
$la = demoparam("la");
$flags = demoparam("flags");
$democmd = demoparam("democmd");

$d_forcer1 = demoparam("forcer1");
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

$elements = 
  array("instr"  => array("title" => "Instructions", 
                          "w" => $default_wide_w, "d" => "block", "text" => false),
        "r1"     => array("title" => "Replica #1",   
                          "h" => $default_h, "w" => $default_w, "d" => "block", "text" => true, "ro" => false),
        "r2"     => array("title" => "Replica #2",   
                          "h" => $default_h, "w" => $default_w, "d" => "block", "text" => true, "ro" => false),
        "l1"     => array("title" => "Lens #1",      
                          "h" => $default_h, "w" => $default_w, "d" => "none", "text" => true,  "ro" => false),
        "l2"     => array("title" => "Lens #2",      
                          "h" => $default_h, "w" => $default_w, "d" => "none", "text" => true,  "ro" => false),
        "a1"     => array("title" => "Abstract #1",  
                           "h" => $default_h, "w" => $default_w, "d" => "none", "text" => true,  "ro" => true),
        "a2"     => array("title" => "Abstract #2",  
                          "h" => $default_h, "w" => $default_w, "d" => "none", "text" => true,  "ro" => true),
        "ar"     => array("title" => "Archive",      
                          "h" => $default_h, "w" => $default_w, "d" => "none", "text" => true,  "ro" => true,),
        "schema" => array("title" => "Schema",       
                           "h" => $default_h, "w" => $default_w, "d" => "none", "text" => true,  "ro" => false,),
        "output" => array("title" => "Output",       
                          "h" => $default_h, "w" => $default_wide_w, "d" => "none", "text" => true,  "ro" => true,)
      );

$expert_icon = $expert ? "expertoff" : "experton";
$expert_desc = $expert ? "Turn Expert Mode Off" : "Turn Expert Mode On";
             
$icons = 
  array("error"    => array("desc" => "Error", "dummy" => true),
        $expert_icon => array("desc" => $expert_desc,   
                              "key" => "e"),
        "clear"    => array("desc" => "&nbsp;", "dummy" => true),
        "sync"     => array("desc" => "Synchronize Replicas",    
                            "key" => "s"),
        "reset"    => array("desc" => "Reset Demo",                   
                            "key" => "r"),
        "prev"     => array("desc" => "Previous Demo",                                    
                            "key" => "p"),
        "next"     => array("desc" => "Next Demo",  
                            "key" => "n"),
        "instr"    => array("desc" => "Show Instructions",   "shows" => array("instr"),    
                            "key" => "i"),
        "replica"  => array("desc" => "Show Replicas",       "shows" => array("r1", "r2"),
                            "key" => "c"),
        "abstract" => array("desc" => "Show Abstract Trees", "shows" => array("a1", "a2"), 
                            "key" => "a"),  
        "schema"   => array("desc" => "Show Schema",         "shows" => array("schema"),
                            "key" => "t"),
        "archive"  => array("desc" => "Show Archive",        "shows" => array("ar"), 
                            "key" => "o"),
        "lens"     => array("desc" => "Show Lenses",         "shows" => array("l1","l2"), 
                            "key" => "l"),
        "output"   => array("desc" => "Show Harmony Output", "shows" => array("output"), 
                            "key" => "h")
      );

if ($demonumber == 1) { 
  if ($splash) { 
    $icons = array_splice($icons, 6,2); 
  } else {
    array_splice($icons, 5, 1); 
  } 
} else if ($splash) { 
  $icons = array_splice($icons, 5, 3); 
}
if($splash) { $elements = array_slice($elements, 0, 1); }

if ($reset) {
  $r1 = $d_r1;
  $r2 = $d_r2;
  $ar = $d_ar;
  $l1 = $d_l1;
  $l2 = $d_l2;
  $schema = $d_schema;
  $forcer1 = $d_forcer1;
}
$l1 = $l1 ? $l1 : "Prelude.id";
$l2 = $l2 ? $l2 : "Prelude.id";
$la = $la ? $la : "Prelude.id";
$schema = $schema ? $schema : "Prelude.Any";

debug("REALR1", $r1);

###########
# Logging #
###########

if($enablelogging) {
  /* BEWARE: gethost will hang a few seconds if offline */
  $remote = 
    ($GLOBALS["HTTP_SERVER_VARS"]["REMOTE_ADDR"] == "127.0.0.1")
    ? "localhost"
    : trim(gethost($GLOBALS["HTTP_SERVER_VARS"]["REMOTE_ADDR"]));
  $browser = $_SERVER['HTTP_USER_AGENT'];
  $date = date("Y/m/j G:i:s T");
  $logmsg = "$date  $remote  ($demogroup / $demonumber) $browser\n";
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
if(!$splash) {
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
    . $la . "\n\n"
    . "schema S = \n"
    . "# 0 \"<schema>\"\n"
    . $schema . "\n";
  put_file($lensfile, $lensfilecontents);

  if (!file_exists($democmd)) {
    abort("Executable " . $democmd . " not found in " . getcwd(),"");
  }

  $cmdbase = 
    "export HOME=./; "
    . "export FOCALPATH=.:../../lenses:/$tempdir;"
    . "./$democmd $flags ";

  debug("R1", $r1);
  debug("R1FILE", $r1file);

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

  debug("OUTPUT", $output);

  if (file_exists($newarfile) && file_exists($newr1file) && file_exists($newr2file)) {
    $ar = filecontents($newarfile);
    $r1 = filecontents($newr1file);
    $r2 = filecontents($newr2file);
  } else {
    $error = true;
  }
  
  if ($error || !preg_match('/Conflict/', $output)) { #HACK!! check exit code instead
    array_splice($icons, 0, 1);
  } else {
    $error = true;
    $body_background = "#ffdddd";
    $surtitle_text = "Error";
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
}
#TODO here: remove temporary files!!

##
# ELEMENTS loop
##
function pick($f, $d, $s) { 
  global $expert, $reset;
  if($expert) { return $f ? $f : ($d ? $d : $s); }
  else {
    if($reset) { return $d ? $d : $s; } //ignore form data, it's bogus
    return $f ? $f : ($d ? $d : $s);
  }
}

$ELEMENT_functions = "";
$ELEMENT_init = "";
$ELEMENT_html = "";
foreach ($elements as $k=>$v) {
  global $ELEMENT_functions, $ELEMENT_init, $ELEMENT_html;
  # pick data from form, demo, and static array
  $elements[$k]['h'] = $v['h'] = pick(get_post_data($k . "_h"), demoparam($k . "_h"), $v['h']);
  $elements[$k]['w'] = $v['w'] = pick(get_post_data($k . "_w"), demoparam($k . "_w"), $v['w']);
  $elements[$k]['d'] = $v['d'] = pick(get_post_data($k . "_d"), demoparam($k . "_d"), $v['d']);

  $icon = "";
  foreach($icons as $i=>$w) { if($w["shows"] && in_array($k, $w["shows"])) { $icon = $i; break; } }

  if($k == "output" && $error) { $elements[$k]['d'] = $v['d'] = "block"; }

  $ELEMENT_functions = $ELEMENT_functions
    #resize function
    . " function $k" . "_resize(e){" 
    . " return resize(e, {s:document.getElementById(\"$k\").style," 
    . " h:document.theform.$k" . "_h," 
    . " w:document.theform.$k" . "_w" 
    . ($v["text"] ? ", t:document.getElementById(\"$k" . "_text\").style" : "") 
    . " });}"
    #hide function
    . " function $k" . "_hide(e){"
    . " document.theform.$k" . "_d.value = \"none\";"
    . " document.getElementById(\"$k\").style.display = \"none\";"
    . " if(!i[\"$icon\"]){i[\"$icon\"] = true;"
    . " document.getElementById(\"{$icon}_icon\").style.display = \"inline\";}}"
    ;

  $ELEMENT_init = $ELEMENT_init
    . "document.getElementById(\"{$k}_grip\").addEventListener(\"mousedown\", $k" . "_resize, true);"
    . "document.getElementById(\"{$k}_min\").addEventListener(\"click\", $k" . "_hide, true);"
    . "s=document.getElementById(\"$k\").style;"
    . "s.display=document.theform.{$k}_d.value;"
    . "if(document.theform.{$k}_w.value){s.width=document.theform.$k" . "_w.value + 'px';}"
    . "if(document.theform.{$k}_h.value){s.height=document.theform.$k" . "_h.value + 'px';}"
    . ($v['text'] ?
       ("s=document.getElementById(\"{$k}_text\").style;"
        . "if(document.theform.{$k}_w.value){s.width=document.theform.$k" . "_w.value + 'px';}"
        . "if(document.theform.{$k}_h.value){s.height=(document.theform.$k" . "_h.value - $text_height_diff)+ 'px';}")
       : "")
    ;

  eval("\$k_val=\$$k;");
  debug("$k", $k_val);
  $ELEMENT_html = $ELEMENT_html
    . "<div class=\"box\" id=\"$k\">"
    . "<div class=\"title\">{$v['title']}<div class=\"min\" id=\"$k" . "_min\"></div></div>"
    . "<div>" . ($v["text"] 
                  ? "<textarea name=\"{$k}_text\" id=\"{$k}_text\"" . ($v["ro"] ? " readonly" : "") . ">$k_val</textarea>" 
                  : "<p>$k_val</p>") 
             . " </div>"
    . "<div class=\"grip\" id=\"{$k}_grip\"></div></div>"
    . "<input type=\"hidden\" name=\"$k" . "_w\" value=\"{$v['w']}\">"
    . "<input type=\"hidden\" name=\"$k" . "_h\" value=\"{$v['h']}\">"
    . "<input type=\"hidden\" name=\"$k" . "_d\" value=\"{$v['d']}\">";
}

##
# ICON loop
##
$sep = $sep_actions = "";
$ICON_array = "";
$ICON_functions = "";
$ICON_html = "";
$ICON_init = "";
$ICON_actions = "";
foreach($icons as $i=>$v) {
  global $sep, $sep_actions, $ICON_array, $ICON_functions, $ICON_html, $ICON_init, $ICON_actions;

  # calculate active icons
  $icons[$i]['active'] = $v['active'] = "false";
  if(! $v['shows']) { $icons[$i]['active'] = $v['active'] = "true"; } else {
    foreach($v['shows'] as $d) {      
      if($elements[$d]['d'] == "none") { $icons[$i]['active'] = $v['active'] = "true"; break; }
    }
  }

  if($i == "outut" && $error) { $icons['output']['active'] = $v['active'] = "false"; }

  # javascript array
  $ICON_array = $ICON_array . "{$sep}\"$i\":{$v['active']}";
  $sep = ",";

  # javascript initialization
  $ICON_init = $ICON_init 
    . "document.getElementById(\"{$i}_icon\").addEventListener(\"mouseover\", {$i}_over, true);"
    . "document.getElementById(\"{$i}_icon\").addEventListener(\"mouseout\", hide_st, true);"
    . ($v["dummy"] ? "" : "document.getElementById(\"{$i}_icon\").addEventListener(\"click\", {$i}_click, true);");

  # javascript functions
  $show_code = "";
  $hide_code = "";
  if($v['shows']) {
    foreach($v["shows"] as $d) {
      $show_code = $show_code 
        . "document.theform.{$d}_d.value = \"block\";"
        . "document.getElementById(\"{$d}\").style.display = \"block\";";
      $hide_code = $hide_code . "{$d}_hide();";
    }
  }
  $ICON_functions = $ICON_functions
    . ($v["shows"] ? 
       " function ${i}_click() {"
       . " hide_st();"
       . " i[\"$i\"]=false;"
       . " document.getElementById(\"{$i}_icon\").style.display=\"none\";"
       . $show_code
       . " } "
       . " function {$i}_toggle() { "
       . " if(i[\"$i\"]) { {$i}_click(); }"
       . " else { "
       . $hide_code
       . " }"
       . " }"
       : "")
    . "function {$i}_over(e){show_st(\"$i\");}";
   
  #Keystroke Actions 
  if($v['key']) { 
    $i_code = $v['shows'] ? "{$i}_toggle" : "{$i}_click";
    $ICON_actions = $ICON_actions . "{$sep_actions} '{$v['key']}':{$i_code}";
    $sep_actions = ",";
  }
  # HTML
  $src = "images/icons/" . ($v['file'] ? $v['file'] : $i) . ".png";
  $desc = $v['desc'] ? $v['desc'] : "&nbsp;";
  $ICON_html = $ICON_html . "<img id=\"$i" . "_icon\" class=\"hide\" src=\"$src\" alt=\"$desc\">"; 
}

##
# SELECTOR loop
##
$SELECT_html = 
  "<select name=\"demogroup\" onchange=\"select_demo_group()\">"
  . "<optgroup label=\"Overview\">";
foreach ($alldemos as $k=>$v) {
  global $SELECT_html;
  if(!empty($v["header"])) {
    $SELECT_html = $SELECT_html 
      . "</optgroup>"
      . "<optgroup label=\"" . $v["header"] . "\">";
  } else {     
    $SELECT_html = $SELECT_html
      . "<option " 
      . (($k == $demogroup) ? "selected " : "") 
      . "value=\"$k\">" 
      . $v["demogroupname"] . "</option>";
  }
}
$SELECT_html = $SELECT_html
  . "</optgroup>"
  . "</select>"
  . "<select name=\"demonumber\" onchange=\"select_demo_number()\">'";
for($i = 1; !empty($alldemos[$demogroup][$i]); $i++) {
 $SELECT_html = $SELECT_html 
   . "<option "
   . (($i == $demonumber) ? "selected " : "")
   . "value=\"$i\">$i</option>";
}
$SELECT_html = $SELECT_html . "</select>";

############
# Response #
############
$debugout = $enabledebug ? join("<br>\n",$debuglog) . "\n" : "\n";

$expert_str = $expert ? "true" : "false";

##
# Style Sheet
##
$css = <<<CSS
body {
  background: $body_background;
  font-family: arial, sans-serif;
  font-size: small;
}
#header {
  position:relative;
  margin: 3px 3px 5px 3px;
  width:100%;
  min-width:700px;
  height:85px;
}
#logo { 
  position:absolute;
  top:0px; left:0px
  margin:0px;
  height:49px;
  width:190px;
  cursor:pointer;
  background-image:url("images/harmony-logo.png");
}
#selector {  
  position:absolute;
  bottom:0px;
  left:0px;
  width:210px;
}
#control { 
  position:absolute;
  left:240px;
  bottom:0px;
}
#surtitle { 
  font-size:small; 
  font-weight:bold;
  min-width:150px;
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
.group { float:left; }
.hide { width:{$icon_width}px; height:{$icon_width}px; display:none; }
.box { 
  position:relative;
  float:left;
  border:1px solid #bbbbbb;
  background-color:#ffffcc;
  padding:3px;
  margin-right:3px;
  margin-bottom:3px;
}
select { font-size:x-small; }
textarea { background:#ffffee; }
.spacer { clear:both; }
.red { color:#cc0000; }
CSS;

##
# Javascript
##
$js = <<<ENDJAVASCRIPT
var x=0;
var y=0;
var c=null;
var i={ $ICON_array };
var actions={ $ICON_actions };

function is(){ var n=0; for(j in i){ if(i[j]) n++ } return n; }
function idx(j){ var n=0; for(k in i){ if(k==j) return n; if(i[j]) n++; } return -1; }
function resize(e, r) {  
  c = r;
  c.s.borderStyle = "dashed";
  c.s.borderColor = "#000000";
  c.s.backgroundColor = "#ffffee";
  x = e.clientX;
  y = e.clientY;
  return true;
}
function m_up(e) {
  if (c) {
    c.s.borderStyle = "solid";
    c.s.borderColor = "#bbbbbb";
    c.s.backgroundColor = "#ffffcc";
    c = null;
  }
  return true;
}
function m_move(e) {
  var dx, dy;
  if(c) {
    dx = e.clientX - x;
    dy = e.clientY - y;
    x = e.clientX;
    y = e.clientY;
    if(c.h.value) { 
      c.h.value = Math.max(parseInt(c.h.value) + dy, $min_height);
      c.s.height = c.h.value + "px"; 
      if(c.t) { c.t.height = (c.h.value - $text_height_diff) + "px"; }
    }
    if(c.w.value) {       
      c.w.value = Math.max(parseInt(c.w.value) + dx, $min_width);
      c.s.width = c.w.value + "px"; 
      if(c.t) { c.t.width = c.w.value + "px"; }
    }   
  }
  return true;
}
function show_st(j) {
  var o, p, s, d, t, n;
   s = document.getElementById("surtitle");
   d = document.getElementById(j + "_icon").alt;
   t = is();
   n = idx(j);
   if (n < (t / 2)) {
     s.style.textAlign = "left";
     o = n * $icon_width;
     p = '<img src="images/clear.png" alt="spacer" height="1" width="'+o+'" >';
     s.innerHTML = p + d;
   } else {
     s.style.textAlign = "right";
     o = (t - n - 1) * $icon_width;
     p = '<img src="images/clear.png" alt="spacer" height="1" width="'+o+'">';
     s.innerHTML = d + p;
  }
}
function hide_st() { var s = document.getElementById("surtitle"); s.style.textAlign = "left"; s.innerHTML="$surtitle_text"; }
function sync_click() { sync_over(); go_submit("sync"); }
function reset_click() { reset_over(); go_submit("reset"); }
function next_click() { next_over(); go_submit("next"); }
function prev_click() { prev_over(); go_submit("prev"); }
function experton_click() { experton_over(); go_submit("expert"); }
function expertoff_click() { expertoff_over(); go_submit("expert"); }
function select_demo_group() { go_submit("select_group"); }
function select_demo_number() { go_submit("select_number"); }
function go_submit(s) { 
  document.theform.submitter.value = s; 
  document.theform.submit(); 
}

var ctrl_hot = false;
function go_keystroke(k) { 
  var c = String.fromCharCode(k.keyCode).toLowerCase();
  if(ctrl_hot) {
    var a = actions[c];
    if (a) { a(); }  
    ctrl_hot = false;
  } else if(k.ctrlKey) {
    if(c == 'h') {
      ctrl_hot = true; 
    } else if (c == 's') {      
      sync_click(); 
      ctrl_hot = false; 
    } 
  }
}

function load() {
  var s;
  document.addEventListener("mouseup", m_up, true);
  document.addEventListener("mousemove", m_move, true);
  document.onkeydown = go_keystroke;
  for(j in i) {if(i[j]){document.getElementById(j+'_icon').style.display='inline';}}
  hide_st();
  $ICON_init 
  $ELEMENT_init
}
$ELEMENT_functions
$ICON_functions
ENDJAVASCRIPT;

# strip redundant whitespace from CSS and javascript
$css = compress($css);
#$js = compress($js);
$demogroupname = $alldemos[$demogroup]['demogroupname'];

##
# HTML OUTPUT
##
print <<<ENDHTML
<html>
<head>
<title>Harmony - $demogroupname</title>
  <style type="text/css">
    $css
  </style>
  <script language="javascript">
    $js
  </script>
</head>
<body onload="load()">
  <form method="post" name="theform">
    <input type="hidden" value="" name="submitter"/>
    <input type ="hidden" value="$expert_str" name="expert"/>
    <div id="header">
      <div id="logo" onclick="location.href='..'" onmouseover="window.status='Return to Harmony homepage'; return true;"></div>
      <div id="control">
        <table><tr><td>
        <div id="surtitle">$surtitle_text</div>
        $ICON_html
        </td></tr></table>
      </div>
      <div id="selector">$SELECT_html</div>
    </div>
    <div>$ELEMENT_html</div>
</form>$debugout
<div class="spacer"></div>
</body>
</html>
ENDHTML;

###################################
# Misc Functions #
###################################

function compress($t) { return preg_replace('/\s+/', ' ', $t);}

function array_insert (&$array, $position, $insert_array) {
  $first_array = array_splice ($array, 0, $position);
  $array = array_merge ($first_array, $insert_array, $array);
}

function put_file ($name, $contents) {
  $handle = fopen($name, 'w');
  if ($handle) {
    fwrite($handle, $contents);
    fclose($handle);
  } else {
    echodebug ("Could not open $name");
  }  
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
  $size = filesize($filename);
  $contents = "";
  if ($size != 0) {
    $handle = fopen($filename, "r");
    $contents = fread($handle, $size);
    fclose($handle);
  }
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
