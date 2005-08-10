<?

$demogroupname = "Harmony Basics";

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
<center><h2>Welcome to Harmony!</h2></center>

<p> Harmony is a generic synchronization framework for tree-structured
data, such as XML.  
This site is a live demo of some of Harmony's features.
</p>

<p>
Harmony's basic model consists of:
<ul>
<li> two replicas to be synchronized, and
<li> an "archive" representing the last common state of these replicas.
</ul>
</p>

<p> For an extremely simple example of what Harmony is intended to do,
try changing <tt>hello</tt> to <tt>goodbye</tt> in the first replica
below and <tt>world</tt> to <tt>cruel world</tt> in the second
replica.  Then press the "Synchronize" button and notice that each
change is propagated to the other replica.
  </p>

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<a><b>hello</b><c>world</c></a>
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "xml";
$demo["r2format"] = "xml";
$demo["lensr1"] = "Xml.squash";
$demo["lensr2same"] = "YES";
$demo["extras"] = '$elidearchive = ""; $elidelens = "YES";';
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
