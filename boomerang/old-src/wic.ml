(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* wic.ml - wrapped in_channel                           *)
(*********************************************************)
(* $Id: wic.ml 3062 2007-06-15 17:05:43Z apilki $ *)


(* This module provide a "wrapped" in_channel. It contains a regular
 * in_channel, plus a string buffer for the chars allready extracted
 * from the in_channel but not used yet. When this wrapped channel is
 * accessed, chars are first read from the buffer, and then form the
 * channel
 *)


type orig = Buff | Chann


(* the type of the wrapped channel. We assume that when orig = Buff,
 * pos_buff < Buffer.length buffer. This should be an invarient of all the
 * functions of this module
 *)

(* saved_char is here for the test of emptyness. For now, I didn't
 * succed in testing the emptyness of an in_channel without reading a
 * char from it.  *)
type t = { buffer : Buffer.t;
	   channel : in_channel option; (* option : to wrapped a single buffer *)
	   mutable saved_char : char option;
	   mutable orig : orig;
	   mutable pos_buff : int;
	   mutable pos_file : int}


(* creation of a new wrapped channel *)
let create buff chann = 
  let length = Buffer.length buff in
  let pos_file = (pos_in chann) - length in
  let orig = 
    if length = 0 then Chann else Buff in
  { buffer = buff;
    channel = Some chann;
    saved_char = None;
    orig = orig;
    pos_buff = 0;
    pos_file = pos_file}

let t_of_buffer buff = 
  let orig = 
    let length = Buffer.length buff in
      if length = 0 then Chann else Buff in
  { buffer = buff;
    channel = None;
    saved_char = None;
    orig = orig;
    pos_buff = 0;
    pos_file = 0;}

let t_of_string s = 
  let buff = Buffer.create (String.length s) in
  Buffer.add_string buff s;
  t_of_buffer buff

(*** test of emptyness. This should probably be more simple ***)

let is_empty_chann t = 
  if t.saved_char = None then (* we already checked !*)
  begin
    try 
      match t.channel with
	| None -> true (* no channel, so it's empty*)
	| Some ic ->
	    (let c = input_char ic in
	       t.saved_char <- Some c;
	       false)
    with
      | End_of_file -> true
  end
  else false

let is_empty t = 
  (t.orig = Chann) && (is_empty_chann t)

let incr_pos t = t.pos_file <- t.pos_file + 1

let read_char t = 
  if t.orig = Chann then
    match t.saved_char, t.channel with
      | Some c, _ -> t.saved_char <- None; incr_pos t; Some c
      | None, Some ic -> 
	  (try
	     let c = input_char ic in
	     incr_pos t;
	     Some c
	   with End_of_file -> None)
      | None, None -> None
  else 
  begin
    let pos_buff = t.pos_buff in
    if pos_buff + 1 = Buffer.length t.buffer then
      t.orig <- Chann
    else 
      t.pos_buff <- pos_buff + 1;
    incr_pos t;
    Some (Buffer.nth t.buffer pos_buff)
  end


let append_buff buff t = 
  if t.orig = Buff then begin
    let s = Buffer.sub t.buffer t.pos_buff ((Buffer.length t.buffer) - t.pos_buff) in
    Buffer.add_string buff s
  end;
  match t.channel with 
    | Some c -> create buff c
    | None -> t_of_buffer buff

let pos_file t = t.pos_file

let seek_in wic n = 
  match wic.channel with 
    | None -> raise (Invalid_argument "Wic with no channel")
    | Some ic ->
	seek_in ic n;
	{wic with
	   saved_char = None;
	   orig = Chann;
	   pos_file = n}
