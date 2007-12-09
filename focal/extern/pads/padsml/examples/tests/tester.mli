exception Failure

(** Parse one element from the named file with the given parse function. *)
val parse_with_from_file : (Pads.handle -> 'a) -> string -> 'a

(** Parse one element from stdin with the given parse function. *)
val parse_with : (Pads.handle -> 'a) -> 'a

(** Parse one element from stdin with the given parse function,
    assuming an io source without records. *)
val parse_with_norec : (Pads.handle -> 'a) -> 'a

(** Parse one element from the named file with the given parse function,
    with record discipline set by bool value. *)
val parse_source : (Pads.handle -> 'a) -> string -> bool -> 'a

(** Parse the source and print it with the Debug tool. 
    The name of the source file is optionally specified on the command line as the first argument.
    In the absence of a first argument, reads from stdin.

    Assumes that the format uses a newline record discipline.

    Returns nothing. *)
module Debug_test (Ty:Type.S) : sig end

(** Parse the source and print it with the Debug tool. 
    The name of the source file is optionally specified on the command line as the first argument.
    In the absence of a first argument, reads from stdin.

    Assumes that the format uses no record discipline.

    Returns nothing. *)
module Debug_test_norec (Ty:Type.S) : sig end
