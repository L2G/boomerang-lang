(** converts a structure into a lens module

    the first string is the module name to use, the second name is the name
    to give the top-level lens
*)
val struct_to_module : Struct.t -> string -> string -> Bsyntax.modl
