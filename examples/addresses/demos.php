<?
$demogroupname = "Address books";
$demo["democmd"] = "../../src/harmony";
$demo["forcer1"] = true;
$demo["default_h"] = 300;
$demo["schema"] = "Addr.AddrBook";

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<p> We've built Harmony instances (i.e., appropriate lenses plus a
little bit of top-level "glue") for address book data in two different
formats: an XML variant of the popular vCard format, and
comma-separated-value files in the form used by the open-source
<tt>pilot-address</tt> tool, which can be used to exchange data with
PalmOS-based PDAs.</p>

<p>
Try changing the text in the <tt>tel-home</tt> tag of the first entry in one
replica and the <tt>tel-work</tt> tag of the second entry in the other. Then
re-synchronize and see what happens.  Repeat as desired; then go on to
the next part of the demo.
</p>
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<xcard>
  <vcard>
    <n><family>Smith</family></n>
    <tel-home>123-4567</tel-home>
  </vcard>

  <vcard>
    <n><family>Jones</family></n>
    <tel-work>314-1596</tel-work>
  </vcard>

</xcard>
XXX;
# ---------------------------------------------------------
$demo["l1"] = $demo["l2"] = "Addr.xcard";
$demo["r1format"] = $demo["r2format"] = "xml";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<p>
The next part of the demo illustrates some variations in the treatment of names.  
</p>

<p> Names in our vcard format can be represented in several different
formats: given/family, family-only, given-only, and "bare name" (just
text immediately inside the <tt>n</tt> tag).  In the abstract tree, all of
these are transformed into the same format: the whole abstract address
book consists of a bush whose immediate child edges are last names,
whose grandchild edges are first names, and whose great-grand-trees
are the remaining information in each address record.</p>

<p> Look at the records below and observe how each one is reflected in
the abstract tree in the other window.  </p>

<p> (Note that we are taking advantage of a simple form of
heterogeneity here: the first replica is being passed through the
address book lens, while the second replica is using just the identity
lens.  The effect of this is that we can use the synchronizer to push
information back and forth through the lens: change the first replica
and synchronize to see how the get direction of the lens behaves;
change the second replica and synchronize to see the put-back
direction in action.) </p>

XXX;
# --------------------------------------------------------- 
$demo["r1"] = <<<XXX
<xcard>
  <vcard>
    <n><given>John</given><family>Doe</family></n>
  </vcard>
  <vcard>
    <n>Mr. Smith</n>
  </vcard>
  <vcard>
    <n><family>Jones</family></n>
  </vcard>
  <vcard>
    <n><given>Fred</given></n>
  </vcard>
</xcard>
XXX;
# ---------------------------------------------------------
$demo["l2"] = "id";
$demo["r2format"] = "meta";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<p> Now let us look at some subtleties in the treatment of phone
numbers.  In the concrete XML format, there can be any number of
(so-called) telephone entries, with tags <tt>tel-home</tt>,
<tt>tel-work</tt>, <tt>tel-cell</tt>, <tt>email</tt>, and
<tt>fax</tt>.  In the abstract tree, all the entries of each sort are
collected into a list.  </p>

<p>
Check out how this works by looking at the address entries below and the
corresponding records in the abstract tree.  Try editing some of the records
and see what happens.  (Also, try editing some of the information in the
abstract tree and see how it gets propagated back to the concrete one.)
</p>

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<xcard>

  <vcard>
  <n><given>Joe</given><family>Jones</family></n>
  <tel-home>111-1111</tel-home>
  <tel-work>999-9999</tel-work>
  <tel-home>222-2222</tel-home>
  <tel-home>333-3333</tel-home>
  </vcard>

</xcard>
XXX;
# ---------------------------------------------------------
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<p>
The other concrete address format that we've implemented is a pretty
straightforward database-like representation.  (It's exactly the
format understood by the <tt>pilot-address</tt> utility from the
<tt>pilot-link</tt> suite of PalmOS tools for Unix.)  The concrete
format is a sequence of lines of comma-separated values (CSV).
</p>

<p>
This is a much simpler representation than the XML one; the only
tricky bit is that (following Palm's funny format for address records)
there are several columns allocated jointly to telephone numbers of
all varieties.  The particular variety of each number is represented
by a tag in front of the number itself.
</p>

<p>
Look at the concrete CSV format in the first replica and how it is
mapped into the same abstract tree format as we've already seen.  Play
with editing both the concrete and the abstract trees.
</p>
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
# 01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21
"Doe","James","","","Home";"303-999-3727","Work;999-888-9876","","","","","","","","","","","","","","0","Unfiled"
"Smith","John","","","Home";"303-999-3727","Work;999-888-9876","Work;111-223-4567","","","","","","","","","","","","This is a note","0","Unfiled"
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "csv";
$demo["l1"] = "Addr.csv";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
<p>
Finally, let's see a heterogeneous synchronization between an XML address book and a CSV one.
</p>
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
# 01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21
"Doe","James","","","Home";"303-999-3727","Work;999-888-9876","","","","","","","","","","","","","","0","Unfiled"
"Smith","John","","","Home";"303-999-3727","Work;999-888-9876","Work;111-223-4567","","","","","","","","","","","","This is a note","0","Unfiled"
XXX;
savedemo();
# ---------------------------------------------------------

?>
