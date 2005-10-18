<?

$demogroupname = "Structured text";
$demo["democmd"] = "../../src/harmony";
$demo["forcer1"] = true;
$demo["default_h"] = 200;
$demo["schema"] = "Structuredtext.NestedListOfValues";
$demo["r1format"] = $demo["r2format"] = "txt";
$demo["l1"] = $demo["l2"] = "Structuredtext.file_with_simple_star_headers";

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
<p>
This first example of structured text synchronization 
illustrates the basic ideas with just one level of structure.
</p>

<p>
Try adding a few new lines below <tt>SECOND PART</tt>
in one replica and below <tt>THIRD PART</tt> in the other.   
</p>

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

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
<p>
This time, instead of synchronizing one structured text file with another
one, we're showing the "abstract tree" that results from applying the
structured text lens to a concrete structured text file.
<p>
The second replica is stored as a simple textual dump of Harmony's internal
tree representation.  During synchronization, the lens applied to the file
(called r2.meta) is the identity lens -- i.e., r2.meta stores exactly the
abstract tree obtained from r1.txt.  You can see that the abstract schema
for synchronizing simple structured text files consists of a list of blocks,
where each block is itself a list whose first element is a header line.
<p>
Try this now: add a new section header (a line beginning with a star) to the
first replica and see where it winds up in the abstract tree.  
<p>
Then go on to the next part of the demo.
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
* FIRST PART

* SECOND PART

* THIRD PART
XXX;
# ---------------------------------------------------------
$demo["l2"] = "id";
$demo["r2format"] = "meta";
savedemo();
# ---------------------------------------------------------

##############################################################################

# More text to possibly add back in later...
# 
# (This is achieved by dropping the
# -simplified flag on the command line to harmonize-structuredtext, which
# causes the lens Structuredtext.file_with_combined_headers to be used instead
# of Structuredtext.file_with_simple_star_headers, as can be seen by a quick
# look at harmonize-structuredtext.ml.  These lenses, in turn, are defined in
# the Focal source file structuredtext.fcl -- this involves some pretty tricky
# lens programming, though, so don't be discouraged if that file doesn't make
# much sense right now.)

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<p> For this next part (and the following ones), we are using a more
sophisticated lens to map from the concrete text file to the abstract
tree.
        
<p> Notice that there are now four levels of headers; lines beginning
with one, two, and three stars, and then lines with no stars but
consisting entirely of capital letters and spaces.

<p> Note, also, that levels can be "skipped".  In this case, empty
headers for all the missing levels are automatically inserted.

<p> Try editing the abstract tree and changing the first <tt>""</tt>
header to a <tt>"* FOO"</tt> (don't forget the space after the star).
See what happens when the abstract tree is pushed back down into the
concrete text format.

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
$demo["l1"] = "Structuredtext.file_with_combined_headers";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
Now we are ready to go back to synchronizing one text file with another,
this time using more deeply structured trees.
<p>
Try making some changes in different parts of the two replicas and seeing
what happens when they are synchronized.
XXX;
# ---------------------------------------------------------
$demo["r2format"] = $demo["r1format"];
$demo["l2"] = $demo["l1"];
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
Now let's try something a bit more interesting: experimenting with
conflicts.
<p>
In one replica, add </tt>FOO</tt> after </tt>one</tt> (on the same line) and </tt>BAR</tt> after
</tt>two</tt>.  In the other replica, add </tt>BAZ</tt> after </tt>two</tt> and </tt>GLOP</tt> after
</tt>three</tt>.  Synchronize and notice what happens, both in the states of the
replicas after synchronization and in the summary printed by Harmony of what
it has done.
<p>
(Yes, we know we need to working on making Harmony's output easier to
understand!)
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

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
For our last experiment with structured text, let's have a look at some
subtleties that arise from Harmony's rather simplistic treatment of lists.
<p>
First, observe that, if we change the list structure only in one replica,
there is never any difficulty.  
<p>
Try adding a line of lowercase characters between <tt>one</tt> and <tt>two</tt> below in
just one of the replicas and see what happens when you sync.
<p>
Next, note that if lines get added to both replicas <em>in the same way</em>, there
is also no problem.
<p>
Try adding the same new line to both replicas between <tt>one</tt> and <tt>two</tt>.  See
what happens.
<p>
However, things are not quite so nice if both the same list in both replicas
is edited.  Try adding another new line to both replicas between <tt>one</tt> and
<tt>two</tt>.  Moreover, add <tt>FOO</tt> to the line <tt>three</tt> in one of the replicas.  See
what happens.  (Probably less than you expected.)
<p>
Delete the <tt>FOO</tt> and re-sync to repair the conflict.  (The <tt>EQUAL</tt>
report signals that Harmony has detected and remembered that the replicas
are now equal.)
<p>
Finally, try adding another new line between <tt>one</tt> and <tt>two</tt> in one replica 
and add a new line at the very end in the other.  Re-sync.  Were you
surprised?  (If not, try it again!)  
<p>
The lesson is that Harmony should be used on list-structured data only with
great care.  When changes are non-overlapping all will be well.  But when
the same parts of a list structure are changed in both replicas,
counter-intuitive behavior can result.  We are investigating how to address
this issue.
<p>
This ends the structured text demo.  
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
* HEAD
one
two
three
XXX;
savedemo();
# ---------------------------------------------------------

?>
