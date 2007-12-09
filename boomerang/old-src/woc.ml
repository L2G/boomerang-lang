(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* woc.ml - wrapped out_channel                           *)
(*********************************************************)
(* $Id: wic.ml 3062 2007-06-15 17:05:43Z apilki $ *)


(* This module provide a "wrapped" out_channel. It's in fact either a
 * out_channel or a buffer. This is usefull for the external lenses
 * that may need to delay the printing on the output *)

let max_size = ref 80
let new_max_size m = 
  max_size := max !max_size m 

let size () = !max_size


(* the type of the wrapped channel  *)
type t = { buffer : Buffer.t option;
	   channel : out_channel option; (* option : to wrapped a single buffer *)
	 }


(* creation of a new wrapped channel *)
let t_of_channel chan = 
  { buffer = None;
    channel = Some chan;
  }

let t_of_buffer buff = 
  { buffer = Some buff;
    channel = None;
  }

let create () = 
  { buffer = Some (Buffer.create (size ()));
    channel = None}
  

let write_string t s = match t.buffer, t.channel with
  | Some b, None  -> Buffer.add_string b s; t
  | None, Some oc -> output_string oc s; t
  | _   , _       -> assert false

let extract_string t = match t.buffer with
  | Some b -> 
      let s = Buffer.contents b in
	new_max_size (String.length s);
	s
  | None -> ""

let output_woc woc t = match t.buffer with
  | Some b -> 
      let s = Buffer.contents b in 
	new_max_size (String.length s);
	write_string woc s
  | None -> woc
