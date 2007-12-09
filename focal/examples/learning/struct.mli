type chunk = Oracle.chunk (* Lex.token list *)

type chunks = Oracle.chunks (* chunk list *)

type t =
    (* a structured sequence of fields *)
    Structure of (chunks * t) list
      
    (* an unstructured homogenous list *)
  | Sequence of chunks * t

    (* a union of possible variants *)
  | Union of (chunks * t) list

    (* a regular expression over strings *)
  | Regex of chunks * string

    (* a constant string *)
  | Constant of string

    (* nothing, the empty string, e.g., Structure [] *)
  | Empty

    (* failure, e.g., Union [] *)
  | Void

val to_string : t -> string

(** returns a list of all regex tokens used in the structure *)
val tokens : t -> string list

(** computes the number of bits necessary to transmit data matching a
    description

    this corresponds to the CD function of figure 7 in Fisher et al
*)
val cost : t -> float

(** convert a set of chunks into a structured type using the oracle 

    this corresponds to figure 5 in Fisher et al., though we keep
    track of the chunks in our type t to facilitate data-independent
    rewrites
*)
val discover : chunks -> t


(** refines a discovered structured type

    corresponds to figures 8 and 9 in Fisher et al
*)
val refine : t -> t
