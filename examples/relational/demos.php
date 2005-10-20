<?
$demogroupname = "Relational lenses";
$demo["forcer1"] = true;
$demo["r1_h"] = $demo["r2_h"] = 250;

# ---------------------------------------------------------
$demo["instr"] = <<<HTML

<h3>Encoding Relations</h3>

<p> The <code>Relational</code> module contains lenses for manipulating
relational data.  Since Harmony is designed to work with unordered, labeled
trees, the relations will need to be encoded.<p>

<p>A record will be written in the natural way: as a tree mapping labels to
atomic data elements.</p>

<p>A relation (<em>i.e.</em> a set of records with the same labels) will be
enocded as a list of records.  Harmony will ignore the order of the records
when performing relational operations and will display a relation resulting
from a relational operation in lexicographic order.</p>

<p>A database (<em>i.e.</em> a set of distinctly named relations) will be
written as a tree mapping names to relations.</p>

<p>In the concrete view below, there is an instance of a database with a
relation <code>email_addr</code>, having fields <code>nm</code>,
<code>email</code>, and <code>rel</code>, and a relation
<code>phone_num</code>, having fields <code>nm</code>, <code>ph</code>, and
<code>sort</code>.</p>

HTML;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
{ email_addr =
    [ { nm = {"Alice"}, email = {"alice@cis"}, rel = {"prof"} }
    , { nm = {"Bob"},   email = {"bob@bar"},   rel = {"prsn"} }
    , { nm = {"Bob"},   email = {"bob@foo"},   rel = {"prsn"} }
    , { nm = {"Carol"}, email = {"carol@foo"}, rel = {"prsn"} }
    , { nm = {"Fred"},  email = {"fred@cis"},  rel = {"prof"} }
    ]
, phone_num =
    [ { nm = {"Alice"}, ph = {"111-111-1111"}, sort = {"cell"} }
    , { nm = {"Alice"}, ph = {"222-222-2222"}, sort = {"home"} }
    , { nm = {"Bob"},   ph = {"333-333-3333"}, sort = {"cell"} }
    , { nm = {"Dave"},  ph = {"444-444-4444"}, sort = {"home"} }
    , { nm = {"Ellen"}, ph = {"555-555-5555"}, sort = {"cell"} }
    ]
}
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["l1"] = <<<XXX
id
XXX;
$demo["l1_d"] = "block";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<HTML

<h3>Complex Example</h3>

<p>This example demonstrates a reasonably non-trivial query over a database
with multiple tables.  In the subsequent parts, we will demonstrate the lenses
individually.</p>

HTML;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
{ customers =
    [ { nm = {Alice}, ph = {"215-555-1001"}, zip = {19104} }
    , { nm = {Carol}, ph = {"215-555-1002"}, zip = {19146} }
    , { nm = {David}, ph = {"215-555-1003"}, zip = {19104} }
    ]
, corp_customers =
    [ { corp = {"Univ. of Penn."}, rep = {Bob}, ph = {"215-555-5001"}, zip = {19104} }
    , { corp = {"Corner Store"}, rep = {Eve}, ph = {"215-555-5002"}, zip = {19104} }
    , { corp = {"Joe's Pizza"}, rep = {Joe}, ph = {"215-555-5003"}, zip = {19120} }
    ]
, parts =
    [ { id = {1001}, desc = {"Air filter"} }
    , { id = {1002}, desc = {"Oil filter"} }
    , { id = {1003}, desc = {"Quart oil"} }
    , { id = {1004}, desc = {"Fuel filter"} }
    , { id = {1005}, desc = {"Fuel pump"} }
    , { id = {1006}, desc = {"Brake pads"} }
    , { id = {1007}, desc = {"Spark plug"} }
    , { id = {1008}, desc = {"Computer"} }
    ]
, invoices =
    [ { id = {30501}, cust = {"Univ. of Penn."} }
    , { id = {30502}, cust = {"Carol"} }
    , { id = {30504}, cust = {"Corner Store"} }
    , { id = {30507}, cust = {"Joe's Pizza"} }
    , { id = {30508}, cust = {"Corner Store"} }
    , { id = {30509}, cust = {"Alice"} }
    ]
, invoice_parts =
    [ { inv = {30501}, part = {1002}, qty = {1} }
    , { inv = {30501}, part = {1003}, qty = {5} }
    , { inv = {30502}, part = {1001}, qty = {1} }
    , { inv = {30504}, part = {1007}, qty = {4} }
    , { inv = {30507}, part = {1008}, qty = {1} }
    , { inv = {30508}, part = {1005}, qty = {1} }
    , { inv = {30509}, part = {1001}, qty = {1} }
    , { inv = {30509}, part = {1004}, qty = {1} }
    ]
}
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["l1"] = <<<XXX
add "no" [{iscorp = {no}}];
Relational.ijoin1 "customers" "no" "customers";
Relational.project {"corp","ph","zip"} {"corp","ph"} [{rep={""}}]
    "corp_customers" "corp_customers";
Relational.rename "corp" "nm"
    "corp_customers" "corp_customers";
add "yes" [{iscorp = {yes}}];
Relational.ijoin1 "corp_customers" "yes" "corp_customers";
Relational.ojoin [{}] [{}] {} {}
    {nm=Any,ph=Any,zip=Any,iscorp={no}}
    {nm=Any,ph=Any,zip=Any,iscorp={yes}}
    "customers" "corp_customers" "customers";
Relational.project {"nm", "zip"} {"nm"} [{ph={""},iscorp={"no"}}]
    "customers" "customers";
Relational.rename "nm" "cust" "customers" "customers";
Relational.ijoin1 "invoices" "customers" "invoices";
Relational.rename "part" "partid" "invoice_parts" "invoice_parts";
Relational.rename "id" "partid" "parts" "parts";
Relational.rename "desc" "partdesc" "parts" "parts";
Relational.ijoin1 "invoice_parts" "parts" "invoice_parts";
Relational.rename "id" "inv" "invoices" "invoices";
Relational.ijoin2 "invoices" "invoice_parts" "invoice_entries";
Relational.select
    {inv=Any,cust=Any,zip={19104},partid=Any,partdesc=Any,qty=Any}
    "invoice_entries" "invoice_entries";
Relational.project
    {"inv","cust","partid","partdesc","qty"} {"inv"} [{zip={19104}}]
    "invoice_entries" "invoice_entries"

XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<HTML

<h3>Selection</h3>

<p>The <code>select</code> lens filters the records in a relation.  The filter
is the first argument to the lens and is given in the form of a schema.  The
next argument to <code>select</code> is the name of the relation to which to
apply the filter and the final argument is the name to use for the resulting
relation (which, of course, is allowed to be the same as the name of the input
relation if desired).</p>

<p>Suggested modifications of the abstract view:</p>

<ul>
    <li>Try changing Ellen's phone number in the abstract view.</li>
    <li>Try removing the record for Alice.  Notice that only Alice's cell
    phone entry is deleted in the concrete view.</li>
    <li>Try adding an entry for Dave with a cell phone number in the abstract
    view.</li>
    <li><strong>NB:</strong> Adding a record with <code>sort = {"home"}</code>
    in the abstract view is, in essence, a type error, but will not be caught
    by the Harmony system.  The behavior of the lens at such a type is
    unspecified.</li>
</ul>

HTML;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
{ phone_num =
    [ { nm = {"Alice"}, ph = {"111-111-1111"}, sort = {"cell"} }
    , { nm = {"Alice"}, ph = {"222-222-2222"}, sort = {"home"} }
    , { nm = {"Bob"},   ph = {"333-333-3333"}, sort = {"cell"} }
    , { nm = {"Dave"},  ph = {"444-444-4444"}, sort = {"home"} }
    , { nm = {"Ellen"}, ph = {"555-555-5555"}, sort = {"cell"} }
    ]
}
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["l1"] = <<<XXX
Relational.select {nm=Any,ph=Any,sort={"cell"}}
    "phone_num" "cell_phone_num"
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<HTML

<h3>Simple Projection</h3>

<p>The <code>project</code> lens selects a subset of the fields in a relation.
The selected fields are given as the first argument to the lens.  The second
argument to the lens should be a subset of the projected fields that acts as a
key for the relation.  This will affect the behavior of the lens and will be
further explained in the following part.  The third argument to
<code>project</code> gives default values for the missing fields, which will
be used in the case that records are added in the abstract view.  As with the
<code>select</code> lens, the final two arguments are the name of the relation
to which to apply the operation and the name of the relation that will be
produced.</p>

<p>Suggested modifications of the abstract view:</p>

<ul>
    <li>Try removing the record for Alice.  Notice that only Alice's cell
    phone entry is deleted in the concrete view.</li>
    <li>Try adding a new entry for Carol.  Notice that the <code>cell</code>
    is used as the sort in the concrete view.</li>
    <li>Try adding an entry for Dave in the abstract view.  Notice that this
    does not overwrite the original entry for Dave, which is not a cell phone
    number.</li>
    <li>Try changing Ellen's phone number in the abstract view.</li>
</ul>

HTML;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
{ phone_num =
    [ { nm = {"Alice"}, ph = {"111-111-1111"}, sort = {"cell"} }
    , { nm = {"Alice"}, ph = {"222-222-2222"}, sort = {"home"} }
    , { nm = {"Bob"},   ph = {"333-333-3333"}, sort = {"cell"} }
    , { nm = {"Dave"},  ph = {"444-444-4444"}, sort = {"home"} }
    , { nm = {"Ellen"}, ph = {"555-555-5555"}, sort = {"cell"} }
    ]
}
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["l1"] = <<<XXX
Relational.select {nm=Any,ph=Any,sort={"cell"}}
    "phone_num" "cell_phone_num";
Relational.project {"nm", "ph"} {"nm", "ph"} [{sort={"cell"}}]
    "cell_phone_num" "cell_phone_num"
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<HTML

<h3>Advanced Projection</h3>

<p>The previous part showed a simple example of projection.  In the simple
case, altering a record in the projection will destroy the data in the unseen
fields (<em>i.e.</em> replace it with the defaults).  This can be avoided when
a key can be identified in the abstract view and non-key fields are
altered.</p>

<p>We will assume that, in the <code>email_addr</code> relation, there is a
functional dependency <code>nm -> rel</code>.  Then, if we project away the
<code>email</code> field, the remaining field <code>nm</code> will be a key in
the resulting relation.  The lens below expresses this fact by including only
the field name <code>nm</code> in the second argument.</p>

<p>Suggested modifications of the abstract view:</p>

<ul>
    <li>Try changing the <code>rel</code> field of Bob from <code>prsn</code>
    to <code>prof</code>.  Notice that this is not treated as a deletion
    followed by an insertion, since it preserves the contents of the
    <code>email</code> field.</li>
    <li>Try changing the name <code>Carol</code> to <code>Dave</code>.  Notice
    that this is treated as a deletion plus an insertion, as it must.</li>
</ul>

HTML;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
{ phone_num =
    [ { nm = {"Alice"}, ph = {"222-222-2222"}, sort = {"home"} }
    , { nm = {"Bob"},   ph = {"333-333-3333"}, sort = {"cell"} }
    , { nm = {"Carol"}, ph = {"666-666-6666"}, sort = {"cell"} }
    , { nm = {"Dave"},  ph = {"444-444-4444"}, sort = {"home"} }
    , { nm = {"Dave"},  ph = {"777-777-7777"}, sort = {"cell"} }
    , { nm = {"Ellen"}, ph = {"888-888-8888"}, sort = {"cell"} }
    ]
}
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["l1"] = <<<XXX
Relational.project {"nm", "rel"} {"nm"} [{email={""}}]
    "email_addr" "relationship"
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<HTML

<h3>Natural Joins</h3>

<p>There are several lenses for performing joins.  One simple lens for an
inner (or natural) join is <code>ijoin1</code>.  The only arguments to this
lens are the names of the two relations to combine and the name to use for the
resulting relation.  The join is performed on the fields across the two
relations that share the same names--in the case of the lens below, the join
is performed on the field <code>nm</code>.</p>

<p>Suggested modifications of the abstract view:</p>

<ul>
    <li>Add an entry with a name not present in either relation.</li>
    <li>Add an entry for Carol with <code>sort = {"home"}</code> and a new
    phone number.  Notice that this will <em>overwrite</em> the cell phone
    entry for Carol in the concrete view, which is the correct behavior to
    satisfy the lens laws.</li>
    <li>Add one or two entries for Fred with <code>rel = {"prsn"}</code>.
    Notice that this will update the <code>rel</code> field for Fred in the
    concrete view without erasing his email address.</li>
    <li>Try deleting the entry for Alice.  There are at least three reasonable
    behaviors in the <code>put</code> direction.  The lens <code>ijoin1</code>
    will delete the entry only from the left relation of the concrete view (the
    one given as the first argument).  Swapping the first two arguments to
    <code>ijoin1</code> will cause deletions to be propagated to the opposite
    relation.  The lens <code>ijoin2</code> will propagate deletions to both
    relations.</li>
</ul>

HTML;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
{ email_addr =
    [ { nm = {"Alice"}, email = {"alice@cis"}, rel = {"prof"} }
    , { nm = {"Bob"},   email = {"bob@bar"},   rel = {"prof"} }
    , { nm = {"Bob"},   email = {"bob@foo"},   rel = {"prof"} }
    , { nm = {"Dave"},  email = {"dave@cis"},  rel = {"prsn"} }
    , { nm = {"Fred"},  email = {"fred@cis"},  rel = {"prof"} }
    ]
, phone_num =
    [ { nm = {"Alice"}, ph = {"222-222-2222"}, sort = {"home"} }
    , { nm = {"Bob"},   ph = {"333-333-3333"}, sort = {"cell"} }
    , { nm = {"Carol"}, ph = {"666-666-6666"}, sort = {"cell"} }
    , { nm = {"Dave"},  ph = {"444-444-4444"}, sort = {"home"} }
    , { nm = {"Dave"},  ph = {"777-777-7777"}, sort = {"cell"} }
    , { nm = {"Ellen"}, ph = {"888-888-8888"}, sort = {"cell"} }
    ]
}
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["l1"] = <<<XXX
Relational.project {"nm", "rel"} {"nm"} [{email={""}}]
    "email_addr" "relation";
Relational.ijoin1 "phone_num" "relation" "phone_num"
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
