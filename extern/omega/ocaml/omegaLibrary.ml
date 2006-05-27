
type t
type var
external empty : int -> t = "OMEGA_empty"
external empty : int -> t = "OMEGA_empty"
external get_var : t -> int -> var = "OMEGA_get_var"
external add_exists : t -> string -> (t * var) = "OMEGA_add_exists"
external add_and : t -> t = "OMEGA_add_and"
external add_or : t -> t = "OMEGA_add_or"
external add_not : t -> t = "OMEGA_add_not"
external add_geq : t -> (int * var) list -> int -> unit = "OMEGA_add_geq"
external add_eq : t -> (int * var) list -> int -> unit = "OMEGA_add_eq"
external finalize : t -> unit = "OMEGA_finalize"
external satisfiable : t -> bool = "OMEGA_satisfiable"
external fast_sat : t -> 'a -> bool = "OMEGA_fast_sat"
external print : t -> unit = "OMEGA_print"
