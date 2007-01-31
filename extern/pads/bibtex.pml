(* PADS/ML description for (simple) BibTeX databases *)

open Built_ins
  
ptype opt_ws = pstring_ME("/[ \\t\\n]*/")

ptype ident = pstring_ME("/[a-zA-Z0-9]+/")

ptype quoted = pstring_ME("/[^\\\"]+/")

ptype bracketed = pstring_ME("/[^\\\}]+/")

ptype bt_value = 
    Bare of ident
  | Quoted of "\"" * quoted * "\""
  | Bracketed of "{" * bracketed * "}"

let entry_sep_re = "/[ \\t\\n]*,[ \\t\\n]*/"

let entry_end_re = "/[ \\t\\n]*,[ \\t\\n]*\\}/"

ptype bt_field = { name : ident; opt_ws; '='; opt_ws; value : bt_value }
    
ptype bt_fields = bt_field plist_re(entry_sep_re, entry_end_re)

ptype bt_entry = { 
  '@'; 
  entry_type : ident; opt_ws; 
  "{"; opt_ws;
  key : ident; opt_ws; 
  ','; opt_ws;
  fields : bt_fields;
  pstring_ME(entry_end_re);
  opt_ws
}

ptype bt_entries = bt_entry plist_np

ptype source = bt_entries
