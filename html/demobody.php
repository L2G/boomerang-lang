<?

##############################################################################
# Configuration parameters

# $enabledebug = TRUE;
$enablelogging = TRUE;

$defaultdemogroup = "basics";

# relative to the individual demo directory
$logfile_locations = 
  array("../../../harmonywebdemo.log",  # pub/cgi on fling-l
        "../../log.tmp"                 # harmony on localhost
        );

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
$schema = get_post_data('SCHEMA');
$prevlensr1hex = get_post_data('PREVLENSR1HEX');
$r2 = get_post_data('R2');
$arhex = $_REQUEST['ARHEX'];
$ar = hex2asc($arhex);
$demogroup = $_REQUEST['DEMOGROUP'];
$demonumber = $_REQUEST['DEMONUMBER'];

$elidelens = $_REQUEST['ELIDELENS'];
$elideschema = $_REQUEST['ELIDESCHEMA'];
$elideabstract = $_REQUEST['ELIDEABSTRACT'];
$elidearchive = $_REQUEST['ELIDEARCHIVE'];
$elideoutput = $_REQUEST['ELIDEOUTPUT'];
$onecolumn = $_REQUEST['ONECOLUMN'];
$optimizespace = $_REQUEST['OPTIMIZESPACE'];
$raw = $_REQUEST['RAW'];

debug ('$_REQUEST', $_REQUEST);


##############################################################################
# Load the demos

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
$alldemos[] = array("header" => "Tutorial synchronization demos");
get_demos_from("addresses");
get_demos_from("structuredtext");
$alldemos[] = array("header" => "Lens Programming");
get_demos_from("lenses");
$alldemos[] = array("header" => "Demos for experts (minimally documented)");
get_demos_from("bookmarks");
get_demos_from("calendars");
get_demos_from("relational");
get_demos_from("xmi");

# print_r ($alldemos);


##############################################################################
# Set up parameters

if (empty($demogroup)) {
  $demogroup = $defaultdemogroup;
  $demonumber = 1;
  $reset = "YES";
  $changegroup = TRUE;
}

if (!empty($choosenew)) {
  $reset = "YES";
  $demonumber = "1";
  $nextpart = "";
  $changegroup = TRUE;
}

if (!empty($nextpart)) {
  $reset = "YES";
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

chdir($demogroup);

function demoparam($n) {
  global $demogroup, $demonumber, $alldemos;
  return $alldemos[$demogroup][$demonumber][$n];
}
$r1orig = demoparam("r1");
$r1format = demoparam("r1format");
$r2format = demoparam("r2format");
$arformat = demoparam("arformat");
$lensr1orig = demoparam("lensr1");
$lensr2same = demoparam("lensr2same");
$lensarsame = demoparam("lensarsame");
$harmonyflags = demoparam("flags");
$instructions = demoparam("instructions");
$democmd = demoparam("democmd");
$schemaorig = demoparam("schemaorig");

# A better way: Smash variables using key/value pairs from demo array
#   extract($alldemos[$demogroup][$demonumber]);
# (but some care is needed in places where the LHS and RHS of demoparam 
# calls above are not named the same...)

if ($changegroup) {
   $reset = "YES";
   $elideabstract = "YES";
   $elidelens = "YES";
   $elideschema = "YES";
   $onecolumn = "";
   $elideoutput = "YES";
   $elidearchive = "YES";
}

if (!empty($reset)) {
  $r1 = $r1orig;
  $schema = $schemaorig;
  $lensr1 = $lensr1orig;
  # Allow the demo itself to override any settings that it wants to
  eval (demoparam("extras"));
  # eval ($extras);
}

##############################################################################
# Keep a log of who is playing

if($enablelogging) {

  # NB: gethost will hang a few secs offline
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

##############################################################################
# Run Harmony

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
put_file($arfile, $ar);
put_file($r2file, $r2);

$lensmodule = $tempbasename . "lens";
$lensModule = ucfirst($lensmodule);
$lensfile = "$tempdir/$lensmodule.fcl";
$lensfilecontents = 
  "module $lensModule = let l : lens = \n"
  . "# 0 \"NOFILEHERE\"\n"
  . ($lensr1 ? $lensr1 : "id") . "\n\n"
  . "schema S = \n"
  . "# 0 \"<schema>\"\n"
  . ($schema ? $schema : "Any") . "\n";
put_file($lensfile, $lensfilecontents);

if (!file_exists($democmd)) {
  abort("Executable " . $democmd . " not found in " . getcwd(),"");
}

if (!empty($raw)) $rawflag = "-raw";

$cmdbase = 
    "export HOME=../../../..; "
  . "export FOCALPATH=.:../../lenses:/$tempdir;"
  . "./$democmd $harmonyflags $rawflag ";

# If we are doing a reset, overwrite r2 and archive with r1
if ($reset) { $forcer1 = TRUE; }

# If the lens has been edited, overwrite r2 and archive with r1
# (otherwise chaos ensues!)
if (asc2hex($lensr1) != $prevlensr1hex) { $forcer1 = TRUE; }

$cmd = 
    $cmdbase 
  . "-ar $arfile " 
  . (!empty($lensarsame) ? "-lensar $lensModule.l " : "")
  . "-r1 $r1file " 
  . (!empty($lensr1) ? "-lensr1 $lensModule.l " : "")
  . "-r2 $r2file " 
  . (!empty($lensr2same) ? "-lensr2 $lensModule.l " : "")
  . "-newar $newarfile " 
  . "-newr1 $newr1file " 
  . "-newr2 $newr2file " 
  . (!empty($schema) ? "-schema $lensModule.S " : "")
  . ($forcer1 ? "-forcer1 " : "")
  . "2>&1";
debug ('$cmd',$cmd);
$output = shell_exec($cmd);

if (file_exists($newarfile) && file_exists($newr1file) && file_exists($newr2file)) {
  $ar = filecontents($newarfile);
  $r1 = filecontents($newr1file);
  $r2 = filecontents($newr2file);
} else {
  # This turns out to mess things up because it leaves bad values in these fields
  # and that makes it hard to recover
  # $r2 = "<Harmony failed>";
  # $ar = "";
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

if (empty($elideabstract)) {
  # generate abstract versions of the two (new) replicas
  $getcmd = 
      $cmdbase 
    . (file_exists($newr1file) ? "$newr1file " : "$r1file ")
    . (!empty($lensr1) ? "-lensr1 $lensModule.l " : "")
    . "2>&1";
  $r1abstract = shell_exec($getcmd);
  $getcmd = 
      $cmdbase 
    . (file_exists($newr2file) ? "$newr2file " : "$r2file ")
    . (!empty($lensr2same) ? "-lensr1 $lensModule.l " : "")
    . "2>&1";
  $r2abstract = shell_exec($getcmd);
}

##############################################################################
# Format the response page

if ($elidelens || empty($lensr1orig)) $lensstyle = "display:none";

if ($elideschema || empty($schema)) $schemastyle = "display:none";

if ($elideabstract) $abstracttreesstyle = "display:none; ";

echo <<<HTML
  <html>

  <head>
  <STYLE TYPE="text/css">
    body { margin-left: 15; margin-right: 15; margin-top: 15; 
           background: #dddddd; color:black;
           font-family: arial, sans-serif;
           font-size: smaller;
         }
    table { width:100%; }
    textarea { background: #FFFFdd; width:100%; height:180; }
    textarea.lenstextarea { height:90; }
    td { align:top; }
    .titlebox { text-align: center; 
                padding-left:10; padding-right:10; padding-top:10;
                background: #ffffcc;  
                border-width:medium; border-color:#888888; border-style:solid }
    .titleimage { margin-bottom:10; }
    .lens { $lensstyle }
    .schema { $schemastyle }
    .abstracttrees { $abstracttreesstyle }
    .red { color:#990000; }
    .label { color:#990000; align:left; }
    .instructions { background:#f2f2f2; 
                    padding:6; padding-left:10; 
                    margin-left:50; margin-right:50; margin-top:0; margin-bottom:0; 
                    border-width:thin; border-color:#888888; border-style:solid;
                    overflow: auto; max-height:350; }
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
  $currentname = $alldemos[$demogroup]["demogroupname"];
  echo <<<HTML
    <table class=titlebox><tr>
      <td align=left valign=bottom width=30%>
        <h3>$currentname</h3>
      </td>
      <td align=center valign=top>
        <img class="titleimage" src="images/harmonYY-header-trans.gif" width="350" height="100" alt="Harmony">
      </td>
      <td align=right valign=bottom width=30%>
        <h3>Part $demonumber</h3>
      </td>
    </tr></table>
HTML;
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

echo "<optgroup label=\"Overview\">";
foreach ($alldemos as $k => $v) {
  if (!empty($v["header"])) {
    $header = $v["header"];
    echo "</optgroup> <optgroup label=\"$header\">";
  } else {
    $name = $v["demogroupname"];
    $selected = "";
    if ($k == $demogroup) $selected="selected";
    echo "<option $selected value=\"$k\">$name</option>";
  }
}
echo "</optgroup>";

echo <<<HTML
    </select>
    <input type="hidden" value="" name="CHOOSENEW"/>  
    <select name="DEMONUMBER" onchange="document.theform.RESET.click()">
HTML;

$i = 1;
while (!empty($alldemos[$demogroup][$i])) {
  $selected = "";
  if ($i == $demonumber) $selected="selected";
  echo "<option $selected value=\"$i\">$i</option>";
  $i = $i + 1;
}

echo "</select> </div>";

echo "</td></tr></table>";

####### The boxes...

if (!empty($onecolumn)) {
  $columnbreakifneeded = "</tr><tr>";
}

echo " <table>";

####### Replicas

if (!empty($elideabstract)) {
  $firstreplicatitle = "First replica";
  $secondreplicatitle = "Second replica";
  $archivetitle = "Archive";
} else {
  $firstreplicatitle = "First replica (concrete)";
  $secondreplicatitle = "Second replica (concrete)";
  $archivetitle = "Archive (abstract)";
}

echo <<<HTML
      <tr> 
        <td valign=top>
          <div class=label>$firstreplicatitle:</div>
          <textarea name="R1" rows="23" cols="50">$r1</textarea>
        </td>
        $columnbreakifneeded
        <td valign=top>
          <div class=label>$secondreplicatitle:</div>
          <textarea name="R2" rows="23" cols="50">$r2</textarea>
        </td>
      </tr>
HTML;

####### Lens and schema boxes

  $lensr1hex = asc2hex($lensr1);
  echo <<<HTML
      <tr>
        <td colspan=2>
           <table><tr>
             <td>
               <div class=lens>
               <div class=label>Lens: </div>
               <textarea name="LENSR1" class=lenstextarea>$lensr1</textarea>
               </div>
             </td>           
             <td>
               <div class=schema>
               <div class=label>Synchronization schema: </div>
               <textarea name="SCHEMA" class=lenstextarea>$schema</textarea>
               </div>
             </td>           
           </tr></table>
        </td>
      </tr>
HTML;


####### Abstract trees box

echo <<<HTML
    <tr>
      <td>
        <div class=abstracttrees>
          <div class=label>First replica (abstract):</div>
          <textarea readonly rows="23" cols="50">$r1abstract</textarea><br />
        </div>
      </td>
      $columnbreakifneeded
      <td>
        <div class=abstracttrees>
          <div class=label>Second replica (abstract):</div>
          <textarea readonly rows="23" cols="50">$r2abstract</textarea><br />
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
        $columnbreakifneeded
        <td valign="top">
HTML;

####### Archive box

if (!$elidearchive) {
echo <<<HTML
    <div class=label>$archivetitle:</div>
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

if (!empty($elideschema)) {
  $elideschemachecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="ELIDESCHEMA" $elideschemachecked onchange="document.theform.submit()">Elide schema</input>
HTML;

echo "&nbsp;&nbsp";

if (!empty($elidearchive)) {
  $elidearchivechecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="ELIDEARCHIVE" $elidearchivechecked onchange="document.theform.submit()">Elide archive</input>
HTML;

echo "&nbsp;&nbsp";

if (!empty($elideabstract)) {
  $elideabstractchecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="ELIDEABSTRACT" $elideabstractchecked onchange="document.theform.submit()">Elide abstract trees</input>
HTML;

echo "&nbsp;&nbsp";

if (!empty($elideoutput)) {
  $elideoutputchecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="ELIDEOUTPUT" $elideoutputchecked onchange="document.theform.submit()">Elide output</input>
HTML;

echo "&nbsp;&nbsp";

if (!empty($onecolumn)) {
  $onecolumnchecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="ONECOLUMN" $onecolumnchecked onchange="document.theform.submit()">One column</input>
HTML;

echo "&nbsp;&nbsp";

if (!empty($raw)) {
  $rawchecked = "checked";
}
echo <<<HTML
    <input type="checkbox" name="RAW" $rawchecked onchange="document.theform.submit()">Raw display</input>
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

if ($enabledebug) echo join("\n",$debuglog);


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
