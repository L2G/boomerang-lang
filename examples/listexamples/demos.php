<?

$demogroupname = "Lists";
$demo["democmd"] = "../../src/harmony";
$demo["forcer1"] = true;
$demo["default_h"] = 200;
$demo["schema"] = "List.T Value";
$demo["r1format"] = $demo["r2format"] = "txt";
$demo["l1"] = $demo["l2"] = "List.lines";
$demo["output_d"] = "block";

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<div id="section">Lists</div>

Harmony's synchronization algorithm handles <i>lists</i> specially,
using the same alignment algorithm as the popular <tt>diff3</tt>
command-line utility (which is also used by version management systems
such as CVS and Subversion).
<p>

Try the following on the text files below (which are being parsed as lists of lines): 
<ol>
<li> Insert a few new lines between <tt>b</tt> and <tt>c</tt> in replica 1.
<li> Delete <tt>d</tt> and <tt>e</tt> in replica 2.
<li> In both replicas, change <tt>g</tt> to <tt>G</tt>.
</ol>

Now synchronize.  Notice that all the changes get propagated
successfully, even though the insertions and deletions change the
absolute positions of the "corresponding" lines in the replicas.

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
a
b
c
d
e
f
g
XXX;
savedemo();
# ---------------------------------------------------------


?>
