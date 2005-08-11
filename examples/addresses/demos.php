<?

$demogroupname = "Address books";

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
<p>
This first address book demo illustrates very simple address book
synchronization. 
</p>
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
    <n><family>First entry</family></n>
    <tel-home>123-4567</tel-home>
  </vcard>

  <vcard>
    <n><family>Second entry</family></n>
    <tel-work>314-1596</tel-work>
  </vcard>

</xcard>
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "xml";
$demo["r2format"] = "xml";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
This demo illustrates some variations in the treatment of names.  Names are
recognized in several different formats: given/family, family-only,
given-only, and "bare name" (just text immediately inside the n tag).
In the abstract tree, all of these are transformed into the same
format: the whole abstract address book consists of a bush whose immediate
child edges are last names, whose grandchild edges are first names, and
whose great-grand-trees are the remaining information in each address
record.  Look at the records below and observe how each one is reflected in
the abstract tree in the other window.
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<xcard>

<vcard>
<n><given>John</given><family>Doe</family></n>
</vcard>

<vcard>
<n>Just a name</n>
<note>Hello</note>
</vcard>

<vcard>
<n>Another plain name</n>
</vcard>

<vcard>
<n><family>last</family><given>First</given></n>
</vcard>

<vcard>
<n>Empty note</n>
</vcard>

<vcard>
<n><family>Last only</family></n>
</vcard>

<vcard>
<n><given>First only</given></n>
</vcard>

<vcard>
<n><given>Yet Another First only</given></n>
</vcard>

  <vcard>
    <n>
      <family>Smith</family>
      <given>Beth</given>
    </n>
    <tel-home>215-222-1774</tel-home>
    <tel-cell>215-999-9999</tel-cell>
  </vcard>

</xcard>
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "xml";
$demo["r2format"] = "meta";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
Now let us look at some subtleties in the treatment of phone numbers.  In
the concrete XML format, there can be any number of (so-called) telephone
entries, with tags tel-home, tel-work, tel-cell, email, and fax.  In the
abstract tree, all the entries of each sort are collected into a list.

Check out how this works by looking at the address entries below and the
corresponding records in the abstract tree.  Try editing some of the records
and see what happens.  (Also, try editing some of the information in the
abstract tree and see how it gets propagated back to the concrete one.)
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<xcard>

  <vcard>
  <n><given>John</given><family>Doe</family></n>
  <tel-home>123-4567</tel-home>
  </vcard>

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
$demo["r1format"] = "xml";
$demo["r2format"] = "meta";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
The other concrete address format that we've implemented is a
pretty straightforward database-like representation.  (In fact, it's
exactly the format understood by the pilot-address utility from the
pilot-link suite of PalmOS tools for Unix.)  The concrete format is a
sequence of lines of comma-separated values (CSV).

<p>

This is a much simpler representation than the XML one; the only
tricky bit is that (following Palm's funny format for address records)
there are several columns allocated jointly to telephone numbers of
all varieties.  The particular variety of each number is represented
by a tag in front of the number itself.

<p>

Look at the concrete CSV format in the first replica and how it is
mapped into the same abstract tree format as we've already seen.  Play
with editing both the concrete and the abstract trees.
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
# 01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21
"Doe","James","","","Home";"303-999-3727","Work;999-888-9876","","","","","","","","","","","","","","0","Unfiled"
"Smith","John","","","Home";"303-999-3727","Work;999-888-9876","Work;111-223-4567","","","","","","","","","","","","This is a note","0","Unfiled"
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "csv";
$demo["r2format"] = "meta";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
Finally, let's see a heterogeneous
synchronization between an XML address book and a CSV one.
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
# 01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21
"Doe","James","","","Home";"303-999-3727","Work;999-888-9876","","","","","","","","","","","","","","0","Unfiled"
"Smith","John","","","Home";"303-999-3727","Work;999-888-9876","Work;111-223-4567","","","","","","","","","","","","This is a note","0","Unfiled"
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "csv";
$demo["r2format"] = "xml";
savedemo();
# ---------------------------------------------------------

?>
