<?

$demogroupname = "Relational lenses";

# ---------------------------------------------------------
$demo["instructions"] = <<<HTML

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

<p>The lens used below demonstrates that we may manipulate encoded databases
as trees, if desired.</p>

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
$demo["lensr1"] = <<<XXX
id
XXX;
$demo["extras"] = '$elidelens = "";';
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<HTML

<p>The <code>select</code> lens filters the records in a relation.  The filter
is the first argument to the lens and is given in the form of a schema.  The
next argument to <code>select</code> is the name of the relation to which to
apply the filter and the final argument is the name to use for the resulting
relation (which, of course, is allowed to be the same as the name of the input
relation if desired).</p>

<p>Suggested modifications of the abstract view:</p>

<ul>
    <li>Try removing the record for Alice.  Notice that only Alice's cell
    phone entry is deleted in the concrete view.</li>
    <li>Try adding an entry for Dave with a cell phone number in the abstract
    view.</li>
    <li>Try changing Ellen's phone number in the abstract view.</li>
    <li><strong>NB:</strong> Adding a record with <code>sort = {"home"}</code>
    in the abstract view is, in essence, a type error, but will not be caught
    by the Harmony system.  The behavior of the lens at such a type is
    unspecified.  In particular, it is not guaranteed to satisfy the
    lens laws.</li>
    <li>Try deleting the entry for Alice.  </li>
</ul>

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
$demo["lensr1"] = <<<XXX
Relational.select {nm=Any,ph=Any,sort={"cell"}}
    "phone_num" "cell_phone_num"
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<HTML

<p>The <code>project</code> lens selects a subset of the fields in a relation.
The selected fields are given as the first argument to the lens.  The second
argument to the lens should be a subset of the projected fields that acts as a
key for the relation.  This will affect the behavior of the lens and will be
further explained in the following part.  The third argument to
<code>project</code> gives a default value for the missing fields, which will
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
$demo["lensr1"] = <<<XXX
Relational.select {nm=Any,ph=Any,sort={"cell"}}
    "phone_num" "cell_phone_num";
Relational.project {"nm", "ph"} {"nm", "ph"} [{sort={"cell"}}]
    "cell_phone_num" "cell_phone_num"
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<HTML

<p>The previous part showed a simple example of projection.  More complex
behavior can arise though.  Since our view-update model is state-based, it is
not possible, in general, to distinguish a modifcation from a deletion
followed by an insertion.  In the case of a relational projection, it is
desirable (and fortunately possible) to have a notion of modify-updates by
noting the set of fields that acts as a key in the relation resulting from the
projection.  Then the lens will align the keys when determining how to
translate an update.</p>

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
    that this is treated as a deletion plus an insertion, as desired.</li>
</ul>

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
$demo["lensr1"] = <<<XXX
Relational.project {"nm", "rel"} {"nm"} [{email={""}}]
    "email_addr" "rel"
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<HTML

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
    concrete view without erasing his email address.  <strong>NB:</strong> It
    is possible to edit an abstract view so as to violate MVD's resulting from
    a join (although there are none in this example if the intended functional
    dependencies are maintained).  The behavior of the lens in these cases is
    unspecified.  In particular, it is not guaranteed to satisfy the lens
    laws.</li>
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
        [ { nm = {"Dave"},  email = {""},          rel = {"prsn"} }
        , { nm = {"Alice"}, email = {"alice@cis"}, rel = {"prof"} }
        , { nm = {"Bob"},   email = {"bob@bar"},   rel = {"prof"} }
        , { nm = {"Bob"},   email = {"bob@foo"},   rel = {"prof"} }
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
$demo["lensr1"] = <<<XXX
Relational.project {"nm", "rel"} {"nm"} [{email={""}}]
    "email_addr" "relation";
Relational.ijoin1 "phone_num" "relation" "phone_num"
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
