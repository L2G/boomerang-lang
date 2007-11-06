(** converts a structure into a lens module

    the first string is the module name to use, the second name is the name
    to give the top-level lens, and the third name is the (optional) lines name,
    which will be the lines lens, (top_level_lens . "\n")*
*)
val struct_to_module : Struct.t -> string -> string -> string option -> Bsyntax.modl
