<?

$demogroupname = "Harmony Basics";

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
<center><h2>Welcome to Harmony!</h2></center>

<p> Harmony is a generic synchronization framework for tree-structured
data.  This site is a live demo of some of Harmony's features.

<ul> 

<li>If you are new to harmony, press the "Next part" button to begin a
tour of its basic structure.

<li> If you're already familiar with the basic ideas of Harmony and
want to experiment with some more sophisticated demos, choose
"Tutorial synchronization demos" from the pull-down menu below.  (Some
additional demos are available under "Demos for experts."
Documentation for these is minimal, but you are welcome to play with
them.)

<li>If you are familiar with our paper, <i>Combinators for
Bi-Directional Tree Transformations: A Linguistic Approach to the View
Update Problem</i> (available <a
href="http://www.cis.upenn.edu/~bcpierce/papers/index.shtml#Data%20Synchronization">here</a>),
and want to experiment with lens programming, choose "Lens Programming /
Tutorial."

<li>If you are familiar with our paper, <i>Exploiting Schemas in Data
Synchronization</i> (available <a
href="http://www.cis.upenn.edu/~bcpierce/papers/index.shtml#Data%20Synchronization">here</a>),
and want to see some interesting examples of the use of schemas to
guide the synchronization process, the address book and structured
text demos are particularly recommended.

</ul>
</p>

<p> <div class="red"><i>These demos are still very much under
development.  Please let us (bcpierce at cis dot upenn dot edu) know
what you think!</i></div> </p>

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<a><b>hello</b><c>world</c></a>
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "xml";
$demo["r2format"] = "xml";
$demo["lensr1"] = "id";
$demo["lensr2same"] = "YES";
$demo["extras"] = '$elidearchive = "YES"; $elidelens = "YES"; $elideoutput = "YES";';
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> Harmony's basic model consists of two replicas to be synchronized
and an "archive" representing the last common state of these replicas.
(The archive is displayed here in a different format from the two
replicas.  We'll talk about this in a minute.)

When Harmony is run, it performs a recursive tree-walk over the two
replicas and the archive, observes where each replica has changed with
respect to the archive, propagates these changes to the other replica,
and updates the archive to reflect the new synchronized state. </p>

<p> To see this in action, try changing <tt>hello</tt> to
<tt>goodbye</tt> in the first replica below and <tt>world</tt> to
<tt>cruel world</tt> in the second replica.  Then press the
"Synchronize" button and notice how the changes are propagated.  

Then press "Next part" to
go on. 

</p>    

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<a><b>hello</b><c>world</c></a>
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "xml";
$demo["r2format"] = "xml";
$demo["lensr1"] = "id";
$demo["lensr2same"] = "YES";
$demo["extras"] = '$elidearchive = "";';
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
