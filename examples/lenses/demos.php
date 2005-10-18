<?php
$demogroupname = "Lens Programming";
$demo["default_w"] = 450;
$demo["default_h"] = 150;
$demo["default_wide_w"] = 910;
$demo["r1format"] = $demo["r2format"] = "meta";
$demo["democmd"] = "../../src/harmony";
$demo["r1_d"] = $demo["l1_d"] = "block";
$demo["l1_w"] = $demo["default_wide_w"];
$demo["r2_d"] = "block";
$demo["l2_d"] = $demo["a1_d"] = $demo["a2_d"] = "none";
$demo["forcer1"] = true;
$demo["l2"] = "id";

############################################################

$demo["instr"] = <<<XXX
<p>This tutorial gives a quick introduction to lens programming in Focal.</p>

<p>A lens is a bi-directional transformation that maps <i>between</i>
sets of trees. Formally, a lens between concrete format <i>C</i> and
abstract format <i>A</i> is a pair of functions, one (pronounced
"get") mapping <i>C</i> to
<i>A</i> and the other (pronounced "put-back") mapping <i>A x C</i> to
<i>C</i>.  (Since, in general, the get function can throw away
information, the put-back function takes the original concrete
tree <i>C</i> as a second argument; it weaves together the new
information in its first argument with the projected-away information
from its second argument to yield a new concrete tree.) Harmony
includes a domain-specific programming language, called Focal, whose
programs denote lenses. The concrete syntax of Focal closely resembles
that of the OCaml programming language, extended with special forms
for writing trees and schemas. A detailed description of Focal can be
found in the <a href="../doc/main.pdf">Harmony Manual</a>. 
</p>

<p>
This demo is setup so that the tree in "Replica #1" represents the
concrete tree and the one in "Replica #2" represents the abstract
tree.

To start, edit the tree in the replica to add a new child
named <tt>c</tt> with a subtree <tt>{3}</tt> to the first replica
(i.e., add '<tt>, c={3}</tt>' just before the closing curly
brace). Click the "Synchronize" button; your change will be 
reflected in the in the second replica.
</p>
XXX;
# ---------------------------------------------------------
$demo["r1"] = "{a={1},b={2}}";
savedemo();

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
<p> 
So far we've only seen the identity lens, which propagates its first 
argument in both directions. Let's play with a more interesting lens. 
</p>

<p>
Clear the text in the lens window and replace it with <tt>filter {a,c} {}</tt>
and then press "Synchronize". The <tt>filter</tt> lens takes a set of
names as its first element and prunes away all of the children that do
not belong to the set.  Notice that the <tt>b</tt> child, and its
subtree, have been filtered away.  You will see the
tree <tt>{a={1},c={3}}</tt> in the second replica.
</p>

<p>
Try adding a child named <tt>d</tt> (pointing to a
tree </tt>{4}</tt>) to the first replica, and press "Synchronize"
again. You will see that the <tt>d</tt> child is also filtered
away.
</p>
XXX;

# ---------------------------------------------------------
$demo["lensr1"] = "Prelude.id";
$demo["r1"] = <<<XXX
{a={1},b={2},c={3}}
XXX;
$demo["r2"] = $demo["ar"] = "{a={},c={}}";
savedemo();

###########################################################

$demo["instr"] = <<<XXX
<p>
So far, we've only examined the get direction of lenses. For this next
example, let's explore the put-back component of the <tt>filter</tt> lens.
</p>

<p>
First, modify the <i>second</i> replica, by deleting the
child <tt>c={3}</tt> and updating the child under <tt>a</tt>
to <tt>{5}</tt>. Now click "Synchronize". 

You should see a new concrete tree in the first replica with the
updated value for a, c missing, and with the subtrees under b and d
carried over like this: <tt>{a = {5}, b = {2}, d = {4}}</tt>.
</p>
XXX;
# ---------------------------------------------------------
$demo["forcer1"] = false;
$demo["l1"] = "filter {a,c} {}";
$demo["r1"] = <<<XXX
{a={1},b={2},c={3},d={4}}
XXX;
$demo["r2"] = $demo["ar"] = "{a={1}, c={3}}";
savedemo();
# ---------------------------------------------------------

$demo["instr"] = <<<XXX
<p>
Now let's play with some XML trees. In the first replica, you see a
simple document in the familiar, concrete XML syntax. The second
replica shows the same tree, in Harmony's internal tree
representation, and printed concretely as "meta".</p>

<p>
Change the lens definition from <tt>id</tt> to <tt>Xml.squash</tt> and
click "Synchronize". That looks better! You can view the source of
the <tt>Xml</tt> module and the definition of the <tt>squash</tt> lens
in the </tt>lenses/xml.src</tt> file of your Harmony distribution.
</p>

XXX;
# ---------------------------------------------------------
$demo["forcer1"] = true; $demo["r1format"] = "xml"; $demo["r2format"]
= $demo["ar_format"] = "meta"; $demo["l1"] = $demo["l2"] = $demo["la"]
= "id"; $demo["r1"] = <<<XXX
<a>
  <b>hello world</b>
</a>
XXX;
$demo["r2"] = $demo["ar"] = "{}";
savedemo();
# ---------------------------------------------------------

$demo["instr"] = <<<XXX
<p>
Next change the definition of the lens from <tt>Xml.squash</tt>
to <tt>Xml.squash; hoist "a"; hoist "b"; hoist Xml.PCDATA</tt>.  You
should now have a very simple tree representing the string <tt>hello
world</tt>. 
</p>

XXX;
# ---------------------------------------------------------
$demo["forcer1"] = true; 
$demo["r1format"] = "xml"; 
$demo["r2format"] = $demo["ar_format"] = "meta"; 
$demo["l1"] = "Xml.squash";
$demo["l2"] = $demo["la"] = "id"; 
$demo["r1"] = <<<XXX
<a>
  <b>hello world</b>
</a>
XXX;
$demo["r2"] = $demo["ar"] = "{}";
savedemo();
# ---------------------------------------------------------

$demo["instr"] = <<<XXX
<p>
Now update the abstract tree in the second replica to <tt>goodbye,
cruel world</tt> and click "Synchronize". Notice how this change is
re-inserted into the structure of the XML document in the first
replica.
</p>

<p>
This concludes our lightning tour of some of the main features of
the Focal language. If you want to go further, the first thing to do
is to read the POPL 2005 paper, 
available <a href="http://www.cis.upenn.edu/~bcpierce/papers/index.shtml#Data%20Synchronization">here</a>. 
The "Playground" demo, which exposes the raw Harmony system, can be used to play further with
lens programs.
</p>

XXX;
# ---------------------------------------------------------
$demo["forcer1"] = false; 
$demo["r1format"] = "xml"; 
$demo["r2format"] = $demo["ar_format"] = "meta"; 
$demo["l1"] = 'Xml.squash; hoist "a"; hoist "b"; hoist Xml.PCDATA';
$demo["l2"] = $demo["la"] = "id";
$demo["r1"] = <<<XXX
<a>
  <b>hello world</b>
</a>
XXX;
$demo["r2"] = $demo["ar"] = "{\"hello world\"={}}";
$demo["schema"] = "Prelude.Value";
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
