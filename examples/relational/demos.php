<?

$demogroupname = "Relational lenses";

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
Suggested modifications: delete Alice, add Carol, add Dave, change Ellen's
phone number.
XXX;
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
        [ { nm = {"Alice"}, ph = {"123-456-7890"}, sort = {"cell"} }
        , { nm = {"Alice"}, ph = {"234-567-8901"}, sort = {"home"} }
        , { nm = {"Bob"},   ph = {"345-678-9012"}, sort = {"cell"} }
        , { nm = {"Dave"},  ph = {"456-789-0123"}, sort = {"home"} }
        , { nm = {"Ellen"}, ph = {"567-890-1234"}, sort = {"cell"} }
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
    "cell_phone_num" "cell_phone_num";
Prelude.focus "cell_phone_num" {}
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
Suggested modifications: change the "rel" of Bob from "prsn" to "prof", delete
Carol, add Dave.
XXX;
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
        [ { nm = {"Alice"}, ph = {"234-567-8901"}, sort = {"home"} }
        , { nm = {"Bob"},   ph = {"345-678-9012"}, sort = {"cell"} }
        , { nm = {"Carol"}, ph = {"678-901-2345"}, sort = {"cell"} }
        , { nm = {"Dave"},  ph = {"456-789-0123"}, sort = {"home"} }
        , { nm = {"Dave"},  ph = {"789-012-3456"}, sort = {"cell"} }
        , { nm = {"Ellen"}, ph = {"890-123-4567"}, sort = {"cell"} }
        ]
    }
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["lensr1"] = <<<XXX
Relational.project {"nm", "rel"} {"nm"} [{email={""}}]
    "email_addr" "relation";
Prelude.focus "relation" {}
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
Suggested modifications: add an entry for Carol with "rel = {prsn}", add
home and cell phone entries for Fred with "rel = {prsn}", delete Alice, add a
brand new entry.
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
    { email_addr =
        [ { nm = {"Dave"},  email = {""},              rel = {"prsn"} }
        , { nm = {"Alice"}, email = {"alice@cis"},     rel = {"prof"} }
        , { nm = {"Bob"},   email = {"bob@bar"},       rel = {"prof"} }
        , { nm = {"Bob"},   email = {"bob@foo"},       rel = {"prof"} }
        , { nm = {"Fred"},  email = {"fred@cis"},      rel = {"prof"} }
        ]
    , phone_num =
        [ { nm = {"Alice"}, ph = {"234-567-8901"}, sort = {"home"} }
        , { nm = {"Bob"},   ph = {"345-678-9012"}, sort = {"cell"} }
        , { nm = {"Carol"}, ph = {"678-901-2345"}, sort = {"cell"} }
        , { nm = {"Dave"},  ph = {"456-789-0123"}, sort = {"home"} }
        , { nm = {"Dave"},  ph = {"789-012-3456"}, sort = {"cell"} }
        , { nm = {"Ellen"}, ph = {"890-123-4567"}, sort = {"cell"} }
        ]
    }
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "meta";
$demo["r2format"] = "meta";
$demo["lensr1"] = <<<XXX
Relational.project {"nm", "rel"} {"nm"} [{email={""}}]
    "email_addr" "relation";
Relational.ijoin1 "phone_num" "relation" "phone_num";
Prelude.focus "phone_num" {}
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

?>
