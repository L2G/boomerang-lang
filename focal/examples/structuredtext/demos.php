<?

$demogroupname = "Structured text";
$demo["democmd"] = "../../src/harmony";
$demo["forcer1"] = true;
$demo["default_h"] = 200;
$demo["r1format"] = $demo["r2format"] = "txt";
$demo["output_d"] = "block";
$demo["a1_d"] = $demo["a2_d"] = "block";
$demo["a1_h"] = $demo["a2_h"] = 150;
$demo["l1_d"] = $demo["l2_d"] = "block";
$demo["l1_h"] = $demo["l2_h"] = 50;

$demo["schema"] = "List.T Value";
$demo["l1"] = $demo["l2"] = "List.lines";

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
<div id="section">Structured Text</div>

Harmony can be used to synchronize text files in a wide variety of
ways.  The most basic of these, illustrated in the previous part of
the demo ("Lists"), parses a text file into a simple list of lines.  

<p>

This can be seen by displaying the abstract versions of the replicas,
as we have done below.  

<p>

(To be completely precise, what happens is that the text file is first
loaded into a concrete tree structure with just a single edge
containing the whole contents of the file as its label.  This tree is
then passed through a lens called <tt>List.lines</tt>, which splits
the single edge label at each newline character.  This can be seen, if
you like, by clicking on the "Show lenses" icon in the row of tools at
the top of the screen.)

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
a
b
d
c
e
f
g
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

$demo["schema"] = "Structuredtext.NestedListOfValues";
$demo["l1"] = $demo["l2"] = "Structuredtext.text_with_simple_star_headers";

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<p> A more interesting way to deal with text is to parse it up into a
hierarchical structure.  

<p>

The abstract schema
for synchronizing simple structured text files consists of a list of blocks,
where each block is itself a list whose first element is a header line.
<p>
Try this now: add a new section header (a line beginning with a star) to the
first replica and see where it winds up in the abstract tree.  
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
* FIRST PART

* SECOND PART

* THIRD PART
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# More text to possibly add back in later...
# 
# (This is achieved by dropping the
# -simplified flag on the command line to harmonize-structuredtext, which
# causes the lens Structuredtext.text_with_combined_headers to be used instead
# of Structuredtext.text_with_simple_star_headers, as can be seen by a quick
# look at harmonize-structuredtext.ml.  These lenses, in turn, are defined in
# the Focal source file structuredtext.fcl -- this involves some pretty tricky
# lens programming, though, so don't be discouraged if that file doesn't make
# much sense right now.)

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<p> Now let's use a more
sophisticated lens to map from the concrete text file to the abstract
tree.
        
<p> Notice that there are now four levels of headers; lines beginning
with one, two, and three stars, and then lines with no stars but
consisting entirely of capital letters and spaces.

<p> Note, also, that levels can be "skipped".  In this case, empty
headers for all the missing levels are automatically inserted.

<p> 
Try making some changes in different parts of the two replicas and seeing
what happens when they are synchronized.

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
* HEADER
** SUBHEADER
*** SUBSUBHEAD
PARA HEAD
para line
PARA HEAD
PARA HEAD
* HEADER
** SUBHEADER
*** SUBSUBHEAD
PARA HEAD
para
para
XXX;
# ---------------------------------------------------------
$demo["l1"] = $demo["l2"] = "Structuredtext.text_with_combined_headers";
savedemo();
# ---------------------------------------------------------

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
Finally, let's try something a bit more interesting: experimenting with
conflicts.
<p>
In one replica, add </tt>FOO</tt> after </tt>one</tt> (on the same line) and </tt>BAR</tt> after
</tt>two</tt>.  In the other replica, add </tt>BAZ</tt> after </tt>two</tt> and </tt>GLOP</tt> after
</tt>three</tt>.  Synchronize and notice what happens, both in the states of the
replicas after synchronization and in the summary printed by Harmony of what
it has done.
<p>
At this point, the first and third lines are known to be synchronized and
can be further edited; changes made just in one replica will be propagated
to the other.  The second line, on the other hand, is in conflict and will
stay so until the conflict is repaired.  There are two ways to do this:
<ol>
<li>
   edit both copies of the second line so that they are identical and
     re-synchronize (the new version will be accepted as the new
     synchronized value for this line), or
<li>
   edit one copy back to its original state (just </tt>two</tt>) and
     resynchronize (the changed version from the other replica will now be
     propagaged) 
</ol>
<p>
Try both ways.
<p>
(Note that the second behavior is a slight difference between the
implementation and the algorithm described in our DBPL paper.  The current
implementation does not record conflicts explicitly in the archive.)
<p>
You can also edit header lines, of course, and the same rules of conflicts
apply there as well.  Try this.
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
* HEADER
one
two
three
XXX;
savedemo();
# ---------------------------------------------------------

?>
