<?

# Things to do:
#   when the lens changes, do a RESET
#   add a simple sync demo
#   make "next" go on to the next demo group
#   put the manual on the web
#   number demos from 1


##############################################################################
# Configuration parameters

$defaultdemogroup = "basics";


##############################################################################
# Grab the post data

function get_post_data ($s) {
  return str_replace("\r","",stripslashes($_REQUEST[$s]));
}

$reset = $_REQUEST['RESET'];
$choosenew = $_REQUEST['CHOOSENEW'];
$nextpart = $_REQUEST['NEXTPART'];
$r1 = get_post_data('R1');
$lensr1 = get_post_data('LENSR1');
$prevlensr1hex = get_post_data('PREVLENSR1HEX');
$r2 = get_post_data('R2');
$arhex = $_REQUEST['ARHEX'];
$ar = hex2asc($arhex);
$demogroup = $_REQUEST['DEMOGROUP'];
$demonumber = $_REQUEST['DEMONUMBER'];

$elidelens = $_REQUEST['ELIDELENS'];
$elidearchive = $_REQUEST['ELIDEARCHIVE'];
$elideoutput = $_REQUEST['ELIDEOUTPUT'];
$optimizespace = $_REQUEST['OPTIMIZESPACE'];

# print_r ($_REQUEST);

##############################################################################
# Load the demos

chdir("../examples");

$alldemos = array();

function savedemo () {
  global $demo;
  global $demos;
  $demos[] = $demo;
  $demo = array();
}

function get_demos_from ($subdir) {
  global $demo, $demos, $alldemos;
  $demos = array();
  $f = $subdir . "/demos.php";
  if (file_exists($f)) {
    include($f);
    $demos["demogroupname"] = $demogroupname;
    $alldemos[$subdir] = $demos;
  }
}

get_demos_from("basics");
get_demos_from("addresses");
get_demos_from("structuredtext");
get_demos_from("calendars");
# get_demos_from("lenses");
get_demos_from("relational");

# print_r ($alldemos);


##############################################################################
# Set up parameters

if (empty($demogroup)) {
  $demogroup = $defaultdemogroup;
  $demonumber = 0;
  $reset = "YES";
}

if (!empty($choosenew)) {
  $reset = "YES";
  $demonumber = "0";
  $nextpart = "";
}

if (!empty($nextpart)) {
  if (!empty($alldemos[$demogroup][$demonumber+1])) {
    $demonumber = $demonumber+1;
    $reset = "YES";
  }
  else {
    $shownextparterror = "<br><b>No more parts in this demo</b><br/>(Choose a new demo from the menu below)";
  }
}

chdir($demogroup);

function demoparam($n) {
  global $demogroup, $demonumber, $alldemos;
  return $alldemos[$demogroup][$demonumber][$n];
}

$r1orig = demoparam("r1");
$r1format = demoparam("r1format");
$r2format = demoparam("r2format");
$lensr1orig = demoparam("lensr1");
$lensr2same = demoparam("lensr2same");
$harmonyflags = demoparam("flags");
$instructions = demoparam("instructions");

if (!empty($reset)) {
  $r1 = $r1orig;
  $lensr1 = $lensr1orig;
  # Allow the demo itself to override any settings that it wants to
  eval (demoparam("extras"));
}


##############################################################################
# Run Harmony

$democmd = "harmonize-" . $demogroup;

$tempbasename = "h" . posix_getpid() . str_replace(array(" ","."),"",microtime());
$tempdir = "/tmp";
$tempbase = "$tempdir/$tempbasename";

$r1file = $tempbase . "r1." . $r1format;
$r2file = $tempbase . "r2." . $r2format;
$arfile = $tempbase . "ar.meta";
$newr1file = $tempbase . "newr1." . $r1format;
$newr2file = $tempbase . "newr2." . $r2format;
$newarfile = $tempbase . "newar.meta";

put_file($r1file, $r1);
if (empty($reset)) {
  put_file($arfile, $ar);
  put_file($r2file, $r2);
}

if (!empty($lensr1)) {
  $lensmodule = $tempbasename . "lens";
  $lensModule = ucfirst($lensmodule);
  $lensfile = "$tempdir/$lensmodule.fcl";
  $lensfilecontents = 
    "module $lensModule = let l : lens = \n"
    . "# 0 \"NOFILEHERE\"\n"
    . $lensr1;
  put_file($lensfile, $lensfilecontents);
  if (asc2hex($lensr1) != $prevlensr1hex) {
    # if the lens has been edited by the user, smash the archive so that
    # this SYNC becomes just a GET
    $ar = $r2;
  }
}

if (!file_exists($democmd)) {
  abort("Executable " . $democmd . " not found in " . getcwd(),"");
}

$cmd = 
    "export HOME=../../../..; "
  . "export FOCALPATH=.:../../lenses:/$tempdir;"
  . "./$democmd $harmonyflags "
  . "-ar $arfile " 
  . "-r1 $r1file " 
  . (!empty($lensr1) ? "-lensr1 $lensModule.l " : "")
  . "-r2 $r2file " 
  . (!empty($lensr2same) ? "-lensr2 $lensModule.l " : "")
  . "-newar $newarfile " 
  . "-newr1 $newr1file " 
  . "-newr2 $newr2file " 
  . "2>&1";
# echo "cmd = " . $cmd . "<p>";

if (!empty($reset)) {
  # If this is a reset, run harmony twice so that the output should read EQUAL
  $cmdagain = str_replace("new","",$cmd);
  $output = shell_exec($cmdagain);
}  

$output = shell_exec($cmd);

if (file_exists($newarfile) && file_exists($newr1file) && file_exists($newr2file)) {
  $ar = filecontents($newarfile);
  $r1 = filecontents($newr1file);
  $r2 = filecontents($newr2file);
} else {
  $r2 = "<Harmony failed>";
  $ar = "";
}

# $diag = 
#      "Harmony command: <br> $cmd <p>" 
#    . "Output from Harmony:</br>" . htmlentities($output) . "<p>"
#    . "File " . $r1file . " contains:<br><pre>" . (htmlentities($r1)) ."</pre><p>"
#    . "File " . $r2file . " contains:<br><pre>" . (htmlentities($r2)) ."</pre><p>"
#    . "File " . $arfile . " contains:<br><pre>" . (htmlentities($ar)) ."</pre><p>"
#    . (!empty($lensr1) ? 
#         "File " . $lensfile . " contains:<br><pre>" 
#         . (htmlentities($lensfilecontents)) 
#         . "</pre><p>"
#       : "")
#   ;
# abort("Harmony run did not create all three files", $diag);

$arhex = asc2hex($ar);

##############################################################################
# Format the response page

if ($elidelens || empty($lensr1orig)) $lensstyle = "display:none";

echo <<<HTML
  <html>

  <head>
  <STYLE TYPE="text/css">
    body { margin-left: 15; margin-right: 15; margin-top: 15; background: #dddddd; color:black }
    h1 { text-align: center; padding: 10; background: #ffffaa;  
         border-width:medium; border-color:#888888; border-style:solid }
    table { width:95%; }
    textarea { background: #FFFFdd; width:100%; height:230; }
    textarea.lenstextarea { height:60; }
    td { align:top; }
    .lens { $lensstyle }
    .label { color:#990000; align:left; }
    .instructions { background:#f2f2f2; padding:6; 
                    margin-left:50; margin-right:50; margin-top:0; margin-botom:0; border-width:thin; 
                    border-color:#888888; border-style:solid }
    .controls { }
  /*  
    .buttonbox { background:#e9e9e9; padding:10; margin-left:30%; margin-right:30%; border-width:thin; 
                 border-color:#888888; border-style:solid }
    .demochoice { margin-left: 50; margin-right: 50; text-align:right}
    .controls { background:#e9e9e9; padding:10; border-width:thin; border-color:#888888; border-style:solid }
  */
  </STYLE>
  </head>

  <body>
HTML;

if (empty($optimizespace)) {
echo "<center><h1>Harmony Sandbox</h1></center>";
}

echo '<form name="theform" method="post">';

echo <<<HTML
<script language="JavaScript">
function jsChooseNew() {
   document.theform.CHOOSENEW.value="YES";
   document.theform.submit();
}
</script>
HTML;


####### Instructions 

function emit_instructions () {
  global $instructions;
  echo <<<HTML
      <p>
      <div class=instructions>$instructions</div>
      <br>
HTML;
}

if (empty($optimizespace)) emit_instructions();


####### Control buttons

echo " <table class=controls> <tr> ";

echo <<<HTML
    <td align=left>
    <div class=buttonbox>
    <input type="submit" value="Synchronize"/>  
    <input type="submit" value="Reset" name="RESET"/>  
    <input type="submit" value="Next part" name="NEXTPART"/>  
    $shownextparterror
    </td>
HTML;


####### Demo selection controls

echo "<td align=right>";

echo <<<HTML
    <div class="demochoice">
    <select name="DEMOGROUP" onchange="jsChooseNew()">
HTML;

foreach ($alldemos as $k => $v) {
  $name = $v["demogroupname"];
  $selected = "";
  if ($k == $demogroup) $selected="selected";
  echo "<option $selected value=\"$k\">$name</option>";
}

echo <<<HTML
    </select>
    <input type="hidden" value="" name="CHOOSENEW"/>  
    <select name="DEMONUMBER" onchange="document.theform.RESET.click()">
HTML;

$i = 0;
while (!empty($alldemos[$demogroup][$i])) {
  $selected = "";
  if ($i == $demonumber) $selected="selected";
  echo "<option $selected value=\"$i\">$i</option>";
  $i = $i + 1;
}

echo "</select> </div>";

echo "</td></tr></table>";

####### The boxes...

echo " <table>";

####### Replicas

$firstreplicatitle = "First replica";
$secondreplicatitle = "Second replica";

echo <<<HTML
      <tr> 
        <td valign=top>
          <div class=label>$firstreplicatitle:</div>
          <textarea name="R1" rows="23" cols="50">$r1</textarea>
        </td>
        <td valign=top>
          <div class=label>$secondreplicatitle:</div>
          <textarea name="R2" rows="23" cols="50">$r2</textarea>
        </td>
      </tr>
HTML;

####### Lens box

  $lensr1hex = asc2hex($lensr1);
  echo <<<HTML
      <tr>
        <td colspan=2>
           <div class=lens>
           <div class=label>Lens: </div>
           <textarea name="LENSR1" class=lenstextarea>$lensr1</textarea>
           </div>
        </td>
      </tr>
HTML;


####### Harmony output box

echo <<<HTML
      <tr>
        <td valign=top>
HTML;

if (empty($elideoutput)) {
  echo <<<HTML
    <div class=label>Harmony output: </div>
    <textarea name="DUMMY" rows="23" cols="50">$output</textarea>
HTML;
}

echo <<<HTML
        </td>
        <td valign="top">
HTML;

####### Archive box

if (!$elidearchive) {
echo <<<HTML
    <div class=label>Archive:</div>
    <textarea name="ARASC" readonly rows="23" cols="50">$ar</textarea><br />
HTML;
}

echo <<<HTML
        </td>
      </tr>
    </table>
    </center>
HTML;


### Checkboxes

if (!empty($optimizespace)) echo "<p>";

echo "<center>";

if (!empty($elidelens)) {
  $elidelenschecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="ELIDELENS" $elidelenschecked onchange="document.theform.submit()">Elide lens</input>
HTML;

echo "&nbsp;&nbsp";

if (!empty($elidearchive)) {
  $elidearchivechecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="ELIDEARCHIVE" $elidearchivechecked onchange="document.theform.submit()">Elide archive</input>
HTML;

echo "&nbsp;&nbsp";

if (!empty($elideoutput)) {
  $elideoutputchecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="ELIDEOUTPUT" $elideoutputchecked onchange="document.theform.submit()">Elide output</input>
HTML;

echo "&nbsp;&nbsp";

if (!empty($optimizespace)) {
  $optimizespacechecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="OPTIMIZESPACE" $optimizespacechecked onchange="document.theform.submit()">Compress</input>
HTML;

echo "</center>";

####### Instructions

# if (!empty($optimizespace)) emit_instructions();


####### Hidden fields for passing information along to the next invocation

echo <<<HTML
  <input name="ARHEX" type="hidden" value="$arhex"/>
  <input name="PREVLENSR1HEX" type="hidden" value="$lensr1hex"/>
HTML;


####### Footer

echo "</form>";


##############################################################################
#

##############################################################################
##############################################################################
# Miscellaneous support functions

function asc2hex ($temp) {
   $len = strlen($temp);
   for ($i=0; $i<$len; $i++) $data.=sprintf("%02x",ord(substr($temp,$i,1)));
   return $data;
}

function hex2asc($temp) {
   $len = strlen($temp);
   for ($i=0;$i<$len;$i+=2) $data.=chr(hexdec(substr($temp,$i,2)));
   return $data;
}

function put_file ($name, $contents) {
  $handle = fopen($name, 'w');
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


?>
