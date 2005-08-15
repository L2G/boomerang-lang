<?

$demogroupname = "Lens Programming Tutorial";

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<div class="red">(To be written!  For now, you can use this web page
to pretty much follow the lens tutorial in the Quick Check chapter of
the <a href="../doc/main.pdf" target="_blank">Harmony manual</a>.)</div> 

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
{a=foo, b=bar}
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["lensr1"] = "Prelude.id";
$demo["democmd"] = "../../src/harmony";
$demo["extras"] = '$elidelens = ""; $elidearchive = "YES"; $elideoutput=""; ';
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
