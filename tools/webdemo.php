<html>
<body>
<h1>The Harmony Sandbox...</h1>

<?

##############################################################################
# Where to find harmony:

$harmonydir = "../../current/harmony/";

##############################################################################
# List of available demos

$demos = array(
  array("structuredtext,1", "Structured text, part 1"),
  array("structuredtext,2", "Structured text, part 2"),
  array("structuredtext,3", "Structured text, part 3"),
  array("structuredtext,9", "TESTING")
);


##############################################################################
# Support stuff

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

function get_post_data ($s) {
  return str_replace("\r","",stripslashes($_REQUEST[$s]));
}


##############################################################################
# Parse POST headers and set up parameters for this run

$reset = $_REQUEST['RESET'];
$r1 = get_post_data('R1');
$r2 = get_post_data('R2');
$arhex = $_REQUEST['AR'];
$ar = hex2asc($arhex);
$demoid = $_REQUEST['DEMOID'];

# echo "$_REQUEST = <br>";
# print_r ($_REQUEST);
# echo "<br>";
# echo "r2 = <br>" . htmlentities($r2) . "<br>";

if (empty($demoid)) {
  $demoid = $demos[0][0];
  echo "demoid = " . $demoid . " by default<p>";
  $reset = "YES";
}

$matches = array();
preg_match("/(.*),(.*)/", $demoid, $matches);
$demoname = $matches[1];
$demonumber = $matches[2];
# echo "demoid = " . $demoid . " and demoname = " . $demoname . " and demonumber = " . $demonumber . "<br>";

# echo "cwd = " . getcwd();

$demodir = $harmonydir . "examples/" . $demoname . "/";
chdir($demodir);

# echo "cwd = " . getcwd();

$files = join(" ", listdir());

$matches = array();
preg_match("/demo".$demonumber."r1.[a-z]+/", $files, $matches);
$r1orig = $matches[0];
# echo "r1orig = " . $r1orig . "<br>";

$matches = array();
preg_match("/demo".$demonumber."format2.([a-z]+)/", $files, $matches);
# print_r ($matches);
$r2format = $matches[1];
if (empty($r2format)) $r2format = "meta";
# echo "r2format = " . $r2format . "<br>";

$democmd = "harmonize-" . $demoname;

$flagsfile = "demo".$demonumber."flags";
if (file_exists($flagsfile)) {
  $harmonyflags = trim(join("",file($flagsfile)));
}

$instructionsfile = "demo".$demonumber."instr.txt";
if (file_exists($instructionsfile)) {
  $instructions = str_replace("\n\n", "<p>", join("",file($instructionsfile)));
}

if (!empty($reset)) {
  if (!file_exists($r1orig)) {
    abort("<h1>Oops: demo file " . $r1orig . " is missing!","");
  }
  $r1 = filecontents($r1orig);
}

$tempbase = "/tmp/hd" . posix_getpid() . str_replace(" ","",microtime()) . "-";

$r1file = $tempbase . "r1" . $r1orig;
$r2file = $tempbase . "r2." . $r2format;
$arfile = $tempbase . "ar.meta";
$newr1file = $tempbase . "newr1.txt";
$newr2file = $tempbase . "newr2." . $r2format;
$newarfile = $tempbase . "newar.meta";

put_file($r1file, $r1);
if (empty($reset)) {
  put_file($arfile, $ar);
  put_file($r2file, $r2);
}

if (!file_exists($democmd)) {
  abort("<h1>Oops: executable " . $democmd . " not found","");
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


##############################################################################
# Format the response page

$html = <<<HTML
  <form method="post">
    <select name="DEMOID">
      %DEMOIDS%
    </select>
    <input type="submit" value="Choose" name="RESET"/>  
    <p>
    <i>%INSTRUCTIONS%</i>
    <p>
    <table>
      <tr> 
        <td>
          First replica:<br/>
          <textarea name="R1" rows="23" cols="50">%R1%</textarea>
        </td>
        <td>
          Second replica:<br>
          <textarea name="R2" rows="23" cols="50">%R2%</textarea>
        </td>
      </tr>
    </table>
    <input type="submit" value="Synchronize!"/>  
    <input type="submit" value="Reset" name="RESET"/>  
    <br/>
    <br/>
    <br/>
    Archive:<br />
    <textarea name="ARASC" readonly rows="23" cols="50">%ARASC%</textarea><br />
    <input name="AR" type="hidden" value="%AR%"/>
  </form>
HTML;

$html = str_replace ("%INSTRUCTIONS%", $instructions, $html);
$html = str_replace ("%R1%", $r1, $html);
$html = str_replace ("%R2%", $r2, $html);
$html = str_replace ("%AR%", asc2hex($ar), $html);
$html = str_replace ("%ARASC%", $ar, $html);

function format_demoid ($a) {
  global $demoid;
  if ($a[0] == $demoid) {
    $selected = "selected";
  }
  return ("<option " . $selected . " value=\"" . $a[0] . "\">" . $a[1] . "</option>\n");
}
$demoids = join("", array_map("format_demoid", $demos));
$html = str_replace ("%DEMOIDS%", $demoids, $html);

echo $html;

if (!empty($output) && empty($reset)) {
  echo "Harmony output: <br><pre>" . $output . "</pre><br><br>";
}


?>
