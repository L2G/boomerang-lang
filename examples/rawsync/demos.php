<?

$demogroupname = "Playground";

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

This page provides "raw" access to the lenses and sychronization
facilities of Harmony.  You can use it to experiment with the
synchronization algorithm and/or with writing your own lenses.

</div> 

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
{}
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["lensr1"] = "Prelude.id";
$demo["democmd"] = "../../src/harmony";
$demo["extras"] = '$elidelens = ""; $elidearchive = ""; $elideoutput=""; ';
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
