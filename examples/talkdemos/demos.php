<?

##
# Global Parameters
##
$demogroupname = "Talk Demos";
$demo["schema"] = "Xml.T";
$demo["l1"] = $demo["l2"] = $demo["la"] = "id";
$demo["r1format"] = $demo["r2format"] = "xml";
$demo["arformat"] = "meta";
$demo["l1_d"] = $demo["l2_d"] = $demo["schema_d"] = $demo["a1_d"] = $demo["a2_d"] = "block";
unset($demo["ar_d"]);
unset($demo["l1_d"]);
unset($demo["l2_d"]);
$demo["instr_d"] = "block";
$demo["output_d"] = "block";

$demo["forcer1"] = true;
$demo["splash"] = false;
$demo["default_h"] = 150;
$demo["default_wide_w"] = 755;
$demo["default_w"] = ($demo["default_wide_w"] - 10) / 2;
$demo["r1_h"] = $demo["r2_h"] = 270;;
$demo["a1_h"] = $demo["a2_h"] = 100;;
$demo["output_h"] = 90;;

# $demo["schema_h"] = 60;;
# $demo["schema_w"] = $demo["default_wide_w"];;
$demo["schema_d"] = "none";;

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<div id="section">Talk Demos</div>

This is the demo setup for ETAPS 2006 invited lecture, with two
slightly different concrete address book formats mapping to the same
abstract schema for synchronization.

XXX;

$demo["r1"] = <<<XXX
<contacts>
  <contact>
    <n>Ginger</n>
    <studio>mgm.com</studio>
    <studio>sony.com</studio>
  </contact>
  <contact>
    <n>Fred</n>
  </contact>
</contacts>
XXX;
$demo["l1"] = <<<XXX
Xml.flatten;
hoist "contacts"; List.hd []; hoist "contact";
List.map (mapp {"n"} (List.hd []; hoist "@pcdata";
                      List.hd []);
          pivot "n");
List.flatten;
map (List.hd [];
     map (List.map (hoist "@pcdata"; List.hd []));
     acond {} [] (const [] {})  (hoist "studio"))
XXX;
$demo["l2"] = <<<XXX
Xml.flatten;
hoist "AddressBook"; List.hd []; hoist "Person";
List.map (map (List.hd []); pivot "@pcdata");
List.flatten;
map (List.hd [];
     map (hoist "Address";
          List.map (hoist "@pcdata"; List.hd []));
     acond {} [] (const [] {})  (hoist "Addresses"))
XXX;
$demo["schema"] = "{* = List.T {! = {}}}";
savedemo();

?>
