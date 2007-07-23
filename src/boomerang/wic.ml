(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* wic.ml - wrapped in_channel                           *)
(*********************************************************)
(* $Id: wic.ml 3062 2007-06-15 17:05:43Z apilki $ *)


(* This module provide a "wrapped" in_channel. It contains a regular
   in_channel, plus a string buffer for the chars allready extracted
   from the in_channel but not used yet. When this wrapped channel is
   accessed, chars are first read from the buffer, and then form the
   channel
*)


type orig = Buff | Chann


(* the type of the wrapped channel. We assume that when orig = Buff,
   pos < Buffer.length buffer. This should be an invarient of all the
   functions of this module
*)
type t = { buffer : Buffer.t;
	   channel : in_channel;
	   mutable orig : orig;
	   mutable pos : int}


(* reference to keep track of the maximum size of buffers. (May be)
   usefull for kleenestar, to directly create a buffer of a good size
*)

let max_buff_size = ref 80

let set_max_size i = 
  if i > !max_buff_size then
    max_buff_size := i

let buff_size () = !max_buff_size


(* creation of a new wrapped channel *)
let new_t buff chann = 
  let orig = 
    let length = Buffer.length buff in
    set_max_size length;
    if length = 0 then Chann else Buff in
  { buffer = buff;
    channel = chann;
    orig = orig;
    pos = 0}


let read_char t = 
  if t.orig = Chann then
    input_char t.channel
  else 
  begin
    let pos = t.pos in
    if pos + 1 = Buffer.length t.buffer then
      t.orig <- Buff
    else 
      t.pos <- pos + 1;
    Buffer.nth t.buffer pos
  end


let append_buff buff t = 
  if t.orig = Chann then
    new_t buff t.channel
  else
    let s = Buffer.sub t.buffer t.pos ((Buffer.length t.buffer) - t.pos) in
    Buffer.add_string buff s;
    new_t buff t.channel

