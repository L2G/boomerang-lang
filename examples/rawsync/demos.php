<?

$demogroupname = "Playground";

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<div id="section">Playground</div>

This page provides "raw" access to the lenses and sychronization
facilities of Harmony.  You can use it to experiment with the
synchronization algorithm and/or with writing your own lenses.

XXX;
# ---------------------------------------------------------
$demo["default_h"] = 100;
$demo["forcer1"] = true;
$demo["r1"] = <<<XXX
{}
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["lensr1"] = "Prelude.id";
$demo["democmd"] = "../../src/harmony";
$demo["l1_d"] = "block";
$demo["l2_d"] = "block";
$demo["a1_d"] = "block";
$demo["a2_d"] = "block";
$demo["ar_d"] = "block";
$demo["schema_d"] = "block";
$demo["output_d"] = "block";
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
