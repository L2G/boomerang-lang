<html>
<body>
<h1>The Harmony Sandbox...</h1>

<?

# Things to do:
#   instructions in an (italicized?) box
#   add a minimal CSS

##############################################################################
# Configuration parameters

$defaultdemogroup = "addresses";


##############################################################################
# Grab the post data

function get_post_data ($s) {
  return str_replace("\r","",stripslashes($_REQUEST[$s]));
}

$reset = $_REQUEST['RESET'];
$choosenew = $_REQUEST['CHOOSENEW'];
$nextpart = $_REQUEST['NEXTPART'];
$r1 = get_post_data('R1');
$r2 = get_post_data('R2');
$arhex = $_REQUEST['AR'];
$ar = hex2asc($arhex);
$demogroup = $_REQUEST['DEMOGROUP'];
$demonumber = $_REQUEST['DEMONUMBER'];
$showarchive = $_REQUEST['SHOWARCHIVE'];

# print_r ($_REQUEST);

##############################################################################
# Load the demos

chdir("harmonydir/examples");

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

get_demos_from("addresses");
get_demos_from("structuredtext");
# ... and other subdirs

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
    $shownextparterror = "<br><b>No more parts in this demo</b><br/>(Choose a new demo from the menu above)";
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
$harmonyflags = demoparam("flags");
$instructions = demoparam("instructions");

if (!empty($reset)) {
  $r1 = $r1orig;
}

##############################################################################
# Format the response page

$democmd = "harmonize-" . $demogroup;

$tempbase = "/tmp/hd" . posix_getpid() . str_replace(" ","",microtime()) . "-";

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

if (!file_exists($democmd)) {
  abort("Executable " . $democmd . " not found in " . getcwd(),"");
}

$cmd = 
    "export HOME=../../../..; "
  . "export FOCALPATH=.:../../lenses;"
  . "./" . $democmd . " " . $harmonyflags . " "
  . "-ar " . $arfile . " " 
  . "-r1 " . $r1file . " " 
  . "-r2 " . $r2file . " " 
  . "-newar " . $newarfile . " " 
  . "-newr1 " . $newr1file . " " 
  . "-newr2 " . $newr2file . " " 
  . "2>&1";
# echo "cmd = " . $cmd . "<p>";
$output = shell_exec($cmd);

if (!file_exists($newarfile)) {
  $diag = 
     "Output from Harmony:</br>" . htmlentities($output) . "<p>"
     . "File " . $r1file . " contains:<br><pre>" . (htmlentities($r1)) . "</pre><p>"
     . "File " . $r2file . " contains:<br><pre>" . (htmlentities($r2)) . "</pre><p>"
     . "File " . $arfile . " contains:<br><pre>" . (htmlentities($ar)) . "</pre><p>"
    ;
  abort("Harmony run did not create all three files", $diag);
}

$ar = filecontents($newarfile);
$r1 = filecontents($newr1file);
$r2 = filecontents($newr2file);

$arhex = asc2hex($ar);

##############################################################################
# Format the response page

echo <<<HTML
<script language="JavaScript">
function jsChooseNew() {
   document.theform.CHOOSENEW.value="YES";
   document.theform.submit();
}
</script>
HTML;

echo <<<HTML
  <form name="theform" method="post">
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

echo <<<HTML
    </select>
    <p>
    <i>$instructions</i>
    <p>
    <center>
    <input type="submit" value="Synchronize"/>  
    <input type="submit" value="Reset" name="RESET"/>  
    <input type="submit" value="Next part" name="NEXTPART"/>  
    $shownextparterror
    <p/>
    <table>
      <tr> 
        <td>
          <center>First replica:</center>
          <textarea name="R1" rows="23" cols="50">$r1</textarea>
        </td>
        <td>
          <center>Second replica:</center>
          <textarea name="R2" rows="23" cols="50">$r2</textarea>
        </td>
      </tr>
      <tr>
        <td>
HTML;

if (!empty($output) && empty($reset)) {
  echo <<<HTML
    <center>Harmony output: </center>
    <textarea name="DUMMY" rows="23" cols="50">$output</textarea>
HTML;
}

echo <<<HTML
        </td>
        <td>
HTML;

if (!empty($showarchive)) {
  $showarchivechecked = "checked";
}

echo <<<HTML
    <input type="checkbox" name="SHOWARCHIVE" $showarchivechecked>Show archive</input>
HTML;

if ($showarchive) {
echo <<<HTML
    <center>Archive:</center>
    <textarea name="ARASC" readonly rows="23" cols="50">$ar</textarea><br />
HTML;
}

echo <<<HTML
          <input name="AR" type="hidden" value="$arhex"/>
        </td>
      </tr>
    </table>
    </center>
  </form>
HTML;


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