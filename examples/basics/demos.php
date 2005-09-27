<?

$demogroupname = "Harmony Basics";

# ---------------------------------------------------------
$demo["r1format"] = "xml";
$demo["r2format"] = "xml";
$demo["lensr1"] = "id";
$demo["lensr2same"] = "YES";
$demo["schemaorig"] = "Xml.T";

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

<li>If you are familiar with our paper, <i>Combinators for Bi-Directional
Tree Transformations: A Linguistic Approach to the View Update Problem</i>
(POPL 2005, available <a
href="http://www.cis.upenn.edu/~bcpierce/papers/index.shtml#Data%20Synchronization">here</a>),
and want to experiment with lens programming, choose "Lens Programming /
Tutorial."

<li>If you are familiar with our paper, <i>Exploiting Schemas in Data
Synchronization</i> (DBPL 2005, available <a
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
<tt>cruel world</tt> in the second replica.  Press the "Synchronize"
button and notice how the changes are propagated.

Then press "Next part" to go on. 

</p>    

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<a><b>hello</b><c>world</c></a>
XXX;
# ---------------------------------------------------------
$demo["extras"] = '$elidearchive = "";';
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> Internally, Harmony represents all data in the same way: as
unordered, edge-labeled trees.  We sometimes call this the "meta" format.
</p>

<p> The archive is always displayed in this form.  The replicas, on
the other hand, can be presented to Harmony in a variety of concrete
formats: XML (as in this demo), CSV (comma-separated-values), vCard,
HTML, raw text, etc.  The first thing Harmony does in each case is to
parse the concrete files into meta format.  
</p>

<p>You can see this graphically by unchecking the "elide abstract
trees" control at the bottom of the page.  This causes the replicas to
be displayed both in their concrete, external form and in their
"abstract" internal form after parsing.
  </p>

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<a><b/><c/><d/></a>
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> Since XML data is ordered and our internal ("meta") tree
representation is unordered, a little encoding is needed.  The parser for XML
translates each element into a tree node with a single edge
labeled with the node's tag.  The subtree under this edge has a
special child edge named <tt>@children</tt> leading to a <i>list</i>
of the node's sub-elements.  
</p>

<p>To make things easier to read, these lists are printed in a compact
form, surrounded by square brackets.  But internally they are
themselves represented as unordered trees, using a standard "cons
cell" encoding: a list is represented by a tree with either a single
child named <tt>nil</tt> or else two children named <tt>hd</tt> and
<tt>tl</tt>, etc..  To see how things really look internally, check
the "Raw display" control below.</p>

<p> Another detail of the way trees are displayed is that completely empty
nodes are omitted -- for example, a tree with one child named <tt>nil</tt>
leading to an empty node is written <tt>{nil}</tt> instead of
<tt>{nil={}}</tt>. </p>

<p> You may be wondering why we need the extra indirection of the
<tt>@children</tt> edge: Why not just put the list of sub-elements
directly under the edge representing the element's tag?  To see why, try this: 
<ul>
<li>Add the word <tt>hello</tt> between <tt>&lt;b&gt;</tt> and <tt>&lt;/b&gt;</tt>.  Press "Synchronize."
<li>Add a space and then <tt>cruel="world"</tt> between <tt>&lt;c</tt> and the closing <tt>&gt;</tt>.  Press "Synchronize."
</ul>
</p>

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<a><b/><c/><d/></a>
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> The operation of Harmony's synchronization algorithm is simplicity
itself: it starts by comparing the sets of child edges of the root nodes of
all three trees.  

<ul> <li>Any edges that are present in the archive and one of the replicas
but not the other are considered as deleted; the corresponding edge is
deleted from the replica where it exists and from the archive.

<li> Conversely, edges that are present in one replica and absent from the
 archive and the other replica and are considered as added; these edges (and
 the subtrees below them) are copied to the archive and the other replica.

<li> For edges that are present in <i>both</i> replicas, the algorithm
 proceeds recursively on the corresponding subtrees.  </ul> 

The only exception to this behavior occurs when the algorithm realizes that
the "merged" tree that it has constructed is ill-formed with respect to the
<i>synchronization schema</i>.  (The desired synchronization schema is
provided as another input to Harmony.  To see the schema currently in
effect, uncheck the "Elide schema" control below.  To see the actual
definition of this schema, see the "Module XML" section in the chapter on
the standard libraries in the <a href="../doc/main.pdf"
target="_blank">Harmony manual</a>.)</p>

<p> In particular, the synchronization schema for XML specifies that an edge
labeled <tt>@pcdata</tt> must lead to a node with exactly one child.  If a
naive merge of the changes to a given pcdata node leads to a
<tt>@pcdata</tt> with two children, then this node is flagged as a
"synchronization conflict" and the replicas and archive are left unchanged
at this point.
</p>

<p>
Try adding <tt>hello</tt> between <tt>&lt;a&gt;</tt> and <tt>&lt;/a&gt;</tt>
in the first replica and adding <tt>world</tt> in the second replica.  Press
"Synchronize" and see what happens.  (Yes, we know that the harmony output
is a little hard to interpret.  We're working on this.)  
</p>

<p> What do you think will happen if you add the same attribute to both
replicas different values?  </p>

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<a></a>
XXX;
$demo["extras"] = '$elideoutput = "";';
savedemo();
# ---------------------------------------------------------




##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> 
So far, everything is looking good.  But we are not done yet.  
</p>

<p> In many cases, the concrete representation of data as an XML tree is
<i>too concrete</i>.  In particular, the fact that we are capturing and
maintaining the ordering inherent in the concrete XML tree means that
Harmony's recursive-tree-walking synchronization algorithm will synchronize
everything "by position."  This is often not what is wanted.  </p>

<p> The replicas below contain a very simple database of contact information
for some of our friends.  Try adding a third <tt>person</tt> element with
the same structure to the first replica and, at the same time, a different
third <tt>person</tt> element to the second replica.  Synchronize.  Are you
happy with what happened?</p>

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<contacts>
  <person>
    <name>Fred</name>
    <email>singing@intherain.net</email>
  </person>
  <person>
    <name>Greta</name>
    <email>garbo@mgm.com</email>
  </person>
</contacts>
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> To do a better job, we need the synchronizer to be able to <i>align</i>
the information in the two replicas (and the archive) according to some
criterion other than position.  For example, in this case, aligning the
<tt>person</tt> records according to the contents of their <tt>name</tt>
fields would be a natural choice.  </p>

<p> This brings us to the most interesting part of Harmony:
<i>lenses</i>.  </p>

<p> A lens is a bi-directional transformation between some concrete data
format (encoded as a Harmony tree) and a more "abstract" format that is
"aligned for synchronization" -- i.e., the information is rearranged so that
the recursive tree-walking synchronization algorithm will encounter the
"same parts" of the two replicas at the same time.  </p>

<p>Lenses are bi-directional because each replica actually needs to be
transformed twice: once to prepare it for synchronization, and then again
when synchronization is finished, to transform the updated "abstract tree"
back to its original concrete format so that it can be stored back on disk.
Formally, a lens between concrete format <i>C</i> and abstract format
<i>A</i> is a pair of functions, one (pronounced "get") mapping <i>C</i> to
<i>A</i> and the other (pronounced "put-back") mapping <i>A x C</i> to
<i>C</i>.  (Since, in general, the get function can throw away information,
the put-back function takes the original concrete tree <i>C</i> as a second
argument; it weaves together the new information in its first argument with
the projected-away information from its second argument to yield a new
concrete tree.)
</p>

<p>We'll discuss all this in more detail on the next page, but before going
on, try the same experiment as above (adding different new entries to the
two replicas) and observe the result of synchonization.
</p>

XXX;
$demo["lensr1"] = <<<XXX
List.hd []; hoist "contacts"; hoist "@children"; 
List.map (hoist "person"; hoist "@children"; List.flatten; 
   map (List.hd []; hoist "@children"; List.hd []; hoist "@pcdata"); pivot "name");
List.flatten;
map (List.hd [])
XXX;
$demo["schemaorig"] = "{* = {email = {! = {}}}}";
$demo["extras"] = '$elidelens = ""; $elideschema = ""; $elideabstract = ""; ';
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p>Let's look now at the details of what's going on here.  
</p>

<p>
The box labeled "Lens" contains a program in Harmony's domain-specific
programming language, Focal.  When read from left to right, this expression
describes the get transformation, mapping the concrete trees in the top
boxes to the abstract trees below.  The same expression, when read from
right to left, describes the put-back transformation that is applied after
synchronization.  
</p>

<p> Internally, the lens is formed by composing together (using ; and two
forms of "map") a number of primitive lens expressions.  The particulars of
how all these primitives work are described in our POPL 2005 paper and in
the <a href="../doc/main.pdf" target="_blank">Harmony manual</a>.  If you
want to play with the lens, make a copy of it someplace else and 
replace the contents of the Lens box with just <tt>id</tt>, the identity
lens.  Press Synchronize.  Now start building up the final lens step by
step: first replace <tt>id</tt> with <tt>List.hd []</tt> and press
Synchronize.  Then add <tt>hoist "contacts"</tt> (preceded by a semicolon)
and synchronize again.  Etc.
</p>

XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> The other thing difference from the previous version of the
example is that the synchronization schema has been changed from
<tt>Xml.T</tt> to <tt>{* = {email = {! = {}}}}</tt>.  This is an expression
in Focal's type language (which is a variant of regular tree grammars).  It
specifies that a well-formed abstract tree consists of a root with any
number of children with arbitrary names (the <tt>*</tt>), each leading to a
node with exactly one edge named <tt>email</tt>, leading to a tree with
exactly one edge with an arbitrary name (the <tt>!</tt>), leading to an
empty tree.  </p>

<p>
Exercise for the reader: The <tt>email</tt> edges are not doing anything
very useful here.  Rewrite the lens and the synchronization schema to get
rid of them.  
</p>

<p>
(Hint: You may find it easier to work first on the lens and then, when it is
behaving well, fix the schema.  To avoid complaints about trees not
matching the schema while you are experimenting, you can replace the schema
by <tt>Any</tt>.)
</p>

XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> Our choice to align by <tt>name</tt> has some significant consequences
for the end-to-end behavior of synchronization.  
</p>

<p>
Try this: swap the order of the two <tt>person</tt> elements in the first
replica.  In the second replica, change the <tt>email</tt>  of <tt>Fred</tt>
to <tt>top@hat.com</tt>.  What happens when you synchronize?
</p>

<p> Now try this: In the first replica, change the <tt>name</tt> of
<tt>Fred</tt> to <tt>Finian</tt>; in the second replica, change the
<tt>name</tt> of <tt>Fred</tt> to <tt>Jerry</tt>.  What happens when you
synchronize?  Do you understand why?  </p>

XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> Let's look one more variation of this lens.  We've taken away the final
component of the lens -- <tt>map (List.hd [])</tt> -- and changed the
synchronization schema so that, instead of a single email, a list of emails
is allowed. </p>

<p>
Try adding a second email to one of the entries in one of the replicas and
synchronizing.  Try adding emails to the same person in both replicas and
synchronizing.  
</p>

<p>
The point of this example is that, by varying the way we write lenses, we
can create end-to-end synchronization behaviors where some information is
treated in an unordered way (aligned by keys) while other parts of the
structure are aligned positionally.
</p>

XXX;
$demo["lensr1"] = <<<XXX
List.hd []; hoist "contacts"; hoist "@children"; 
List.map (hoist "person"; hoist "@children"; List.flatten;
  mapp {"name"} (List.hd []; hoist "@children"; List.hd []; hoist "@pcdata");
  pivot "name";
  map (
    map (List.map (hoist "@children"; List.hd []; hoist "@pcdata"));
    (* We check whether there is an email, if not we add an edge pointing to [] *)
    acond {} {email = []}
      (add "email" [])
      id
  )
);
List.flatten; map (List.hd [])
XXX;
$demo["schemaorig"] = "{* = {email = (List.T {! = {}})}}";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> Lenses have one other important use in Harmony: supporting heterogeneous
synchronization.  
</p>

<p> There are many situations where we want to synchronize files
representing "the same information" in different concrete formats.  (Think,
for example, of your bookmark collection: if you use multiple browsers, you
might well want to keep a common set of bookmarks across all of them.)  With
lenses, this is easily achieved.  All we have to do is choose a suitable
common synchronization schema and then write a lens for each concrete format
mapping it to this schema.
</p>

<p> We will not give a concrete example here, but this sort of heterogeneous
synchronization is found in several of the demos below.  </p>

XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

<p> 
This completes the tour of Harmony's basic features.  
</p>

<p> 
To go further, select one of the other demos from the drop-down menu below
(or just press "Next part" to go on to the address book demo).  
</p>

<p> 
If you want to understand what's going on in more depth, two papers are
recommended: 
<ul>
<li><i>Combinators for Bi-Directional
Tree Transformations: A Linguistic Approach to the View Update Problem</i>
(POPL 2005)

<li><i>Exploiting Schemas in Data Synchronization</i> (DBPL 2005) </ul> 

Both are available <a
href="http://www.cis.upenn.edu/~bcpierce/papers/index.shtml#Data%20Synchronization">here</a>.
</p>

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<a>That's all, folks</a>
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
