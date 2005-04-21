(* $Id: palm.mli,v 1.1 2004/09/15 15:15:36 schmitta Exp $ *)

type sink = Buffer.t
and source = { buf : string; mutable pos : int; } 
and rdwr = { rd : source -> V.t; wr : sink -> V.t -> unit; } 

val record_rw : (Name.t * rdwr) list -> rdwr

val getbyte : source -> int
val getshort : source -> int
val getshortBE : source -> int
val getint : source -> int
val getintBE : source -> int
val get_var_length_string : source -> string
val putbyte : sink -> int -> unit
val putshort : sink -> int -> unit
val putshortBE : sink -> int -> unit
val putint : sink -> int -> unit
val putintBE : sink -> int -> unit

val rw_lens : rdwr -> Lens.t
val bitmap_lens : (int * Name.t) list -> Lens.t
val generic_pdb_lens : Lens.t
val generic_pdb_rcd_lens : Lens.t
val ugen_factory : V.t option -> (unit -> Name.t)
