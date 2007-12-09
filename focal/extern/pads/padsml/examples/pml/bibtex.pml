(***************************************************************)
(* The Harmony Project                                         *)
(* harmony@lists.seas.upenn.edu                                *)
(*                                                             *)
(* bibtex.pml - PADS BiBTeX Data Description                   *)
(* (c) 2007 Nate Foster <jnfoster@cis.upenn.edu>               *)
(***************************************************************)
(* $Id: bibtex.pml 2184 2007-01-31 14:04:55Z jnfoster $ *)

open Built_ins

let ws_re = "/[ \\t\\n]*/"

ptype ident = pstring_ME("/[a-zA-Z0-9]+/")

ptype nobqs = pstring_ME("/[^\\\"\\}]*/")
ptype bracketed_noqs = 
      BQFlat of nobqs
    | BQNested of '{' * bracketed_noqs * '}'

ptype nobs = pstring_ME("/[^\\}]*/")
ptype bracketed = 
  BFlat of nobs
| BNested of nobs * '{' * bracketed * '}' * nobs

ptype quoted =
    QFlat of nobqs
  | QNested of nobqs * '{' * bracketed_noqs * '}' * nobqs

ptype bt_value = 
    Bare of ident
  | Quoted of "\"" * quoted * "\""
  | Bracketed of "{" * bracketed * "}"

let entry_sep_re = "/[ \\t\\n]*,[ \\t\\n]*/"

let entry_end_re = "/[ \\t\\n]*?,[ \\t\\n]*\\}[ \\t\\n]*/"

ptype bt_field = {
  name:ident;     pre ws_re;
  '=';            pre ws_re;
  value:bt_value
}

ptype bt_fields = bt_field plist_re(entry_sep_re, entry_end_re)

ptype bt_entry = {
  '@';
  entry_type:ident;  pre ws_re;
  "{";               pre ws_re;
  key:ident;         pre ws_re;
  ',';               pre ws_re;
  fields:bt_fields;  
  pstring_ME(entry_end_re)
}

ptype bt_entries = bt_entry plist_np

ptype source = bt_entries
