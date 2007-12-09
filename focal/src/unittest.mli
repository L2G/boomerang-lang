
exception Unittest

val format_bool : bool -> unit
val format_string : string -> unit
val format_option : ('a -> unit) -> 'a option -> unit

val assert_true : bool -> unit
val assert_false : bool -> unit
val assert_equal : ('a -> unit) -> ('a -> 'a -> bool) -> 'a -> 'a -> unit
val assert_eq : ('a -> unit) -> 'a -> 'a -> unit
val assert_exn : exn -> (unit -> unit) -> unit

val add_test : string -> (unit -> unit) -> unit
val run : unit -> unit

