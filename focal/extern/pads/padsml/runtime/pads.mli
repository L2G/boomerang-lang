(***********************************************************************
*                                                                      *
*             This software is part of the padsml package              *
*           Copyright (c) 2006-2007 Knowledge Ventures Corp.           *
*                         All Rights Reserved                          *
*        This software is licensed by Knowledge Ventures Corp.         *
*           under the terms and conditions of the license in           *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                   Knowledge Ventures Labs Research                   *
*                           Florham Park NJ                            *
*                                                                      *
*            Yitzhak Mandelbaum <yitzhak@research.att.com>>            *
*                                                                      *
***********************************************************************)
(* XXX: This interface was originally generated automatically from the
implementation. We should review it and specify some types more
appropriately than the type inference engine did. E.g. some types 'a
could be more specific.*)

(** parsing flags that can affect parsing *)
type parse_state

(** create an initial parsing state *)
val ps_init        : parse_state

(* functions for setting, unsetting and testing parse state flags *)

val ps_set_panic   : parse_state -> parse_state
val ps_unset_panic : parse_state -> parse_state
val ps_is_panic    : parse_state -> bool

val ps_set_partial   : parse_state -> parse_state
val ps_unset_partial : parse_state -> parse_state
val ps_is_partial    : parse_state -> bool

type pos = int64

type span = (pos * pos) option

(** arguments: function name, span and error message. *)
exception Runtime_error of string * span * string

type 'a result = Ok of 'a | Error

type error_code = Good | Maybe | Nest of span | Syn | Sem

type corrupted = string

type error_info = 
    No_info 
  | Padsc_error_code of Padsc.perrCode_t
  | Error_span of span 
  | Corrupted_data of corrupted
  | Unexpected_data_before_eor
  | Eof_before_eor
      
type pd_header = {
  state : parse_state;
  nerr : int;
  error_code : error_code;
  error_info : error_info;
  span : span;
}

type padsc_handle = Padsc.p_t Com.opaque
type handle

type 'a pd = pd_header * 'a

type base_pd_body = unit
type base_pd = unit pd

type ('a,'b) parser = handle -> 'a * ('b pd)
type ('a,'b) printer = 'a -> ('b pd) -> handle -> unit

type reg_exp = Padsc.pregexp_t Com.opaque

val base_pd_of_pbase_pd : Padsc.pbase_pd -> pos -> base_pd

val pbase_pd_of_base_pd : base_pd -> Padsc.pbase_pd

val string_of_pos : pos -> string

val string_of_span : span -> string

val string_of_error_code : error_code -> string

val string_of_error_info : error_info -> string

val p_char_lit_scan1 : handle -> char -> int result

val p_str_lit_scan1 : handle -> string -> int result

val p_int_lit_scan1 : handle -> int -> int result

val p_regexp_scan1 : handle -> reg_exp -> int result

val p_regexp_str_scan1 : handle -> string -> int result

val print_char_lit: char -> handle -> unit

(* print an ocaml string as a C string. Note that any '\0's in the
   string will prematurely terminate it. *)
val print_str_lit: string -> handle -> unit

val get_pd_hdr : 'a * 'b -> 'a

(** Check whether a pd describes an error-free parse. *)
val pd_is_ok : 'a pd -> bool

module Log : sig

  type report_level =
      Info_level
    | Warning_level
    | Error_level
    | Fatal_level

  val report : report_level -> fun_name:string -> span -> error_info -> 
    msg:string -> handle -> unit
  val report_info : fun_name:string -> span -> error_info -> msg:string -> handle -> unit
  val report_warning : fun_name:string -> span -> error_info -> msg:string -> handle -> unit
  val report_error : fun_name:string -> span -> error_info -> msg:string -> handle -> unit
  val report_fatal : string -> span -> error_info -> string -> handle -> unit
end

val get_current_pos : handle -> pos

(** Compare two positions. Compatible with Pervasives.compare *)
val comp_pos: pos -> pos -> int

(** Compare two positions for equality. *)
val eq_pos: pos -> pos -> bool

(** Create a zero-length span at the current position. *)
val make_empty_span : handle -> span

(** Create a span from a pair of begin and end positions, including byte at end position. *)
val make_incl_span : b:pos -> e:pos -> span

(** Create a span from a pair of begin and end positions, excluding byte at end position. *)
val make_excl_span : b:pos -> e:pos -> span

(** Merge two spans by taking beginning position of first
    and ending position of second. *)
val merge_spans : first:span -> second:span -> span

(** Create a span from the given position until the current position, exclusive. *)
val close_span : pos -> handle -> span

(** Get the beginning position of a span. 
    N.B. Assumes span is of form Some x.
*)
val get_begin_pos : span -> pos

(** Get the ending position of a span 
    N.B. Assumes span is of form Some x.
*)
val get_end_pos : span -> pos

val make_valid_pd_hdr : span -> pd_header

val make_empty_pd_hdr : handle -> pd_header

(** Valid pd header with span set to None. *)
val spanless_pd_hdr : pd_header

(** Create a valid PD with an empty span. *)
val make_empty_pd : handle -> base_pd

(** A good base pd for use base type gen_pd functions. *)
val gen_base_pd : base_pd

(** Get the PADS/C handle from a PADS/ML handle *)
val get_padsc_handle: handle -> padsc_handle

(** Get the output stream from a PADS/ML handle *)
val get_out_stream: handle -> Padsc.sfioPtr

(** Initialize the system, specifying that the data source uses newline-terminated records. *)
val open_handle : unit -> handle result

(** Initialize the system, specifying that the data source does not use records. *)
val open_handle_norec : unit -> handle result

(** Shutdown the system. *)
val close_handle : handle -> unit result

(** Create a copy of the given handle where output state is duplicated
    in the new handle and other state is shared between the original and
    the copy. *)
val share_handle : handle -> handle

type timezone = Padsc.tm_zone_t

val timezone_of_string : string -> timezone option

module IO : sig
  (** DEPRECATED. Use open_in_file instead.
      Open the handle's input file. The second argument is the file name. *)
  val open_file : handle -> string -> unit result

  (** Open the handle's input file. 
      XXX TODOC: What happens to any existing open input stream?
      The second argument is the file name. 
  *)
  val open_in_file : handle -> string -> unit result

  (** Open the handle's output file. The second argument is the file name.
    Does *not* close current output file (if any). Merely replaces it.  
  *)
  val open_out_file : handle -> string -> unit result

  (** Open a new ouput stream for the handle without replacing the
      current out stream. Return a new handle containing the new out
      stream. All other state is unchanged.  The second argument is
      the file name.  *)
  val open_add_out_file : handle -> string -> handle result

  (* Close the IO system for this pads handle. *)
  val close : handle -> unit result

  val is_speculative : handle -> bool
  val get_spec_level : handle -> int

  (* XXX:These functions should really only be available to other runtime modules, not 
     the general module public.  *)
  (** First argument indicates whether checkpoint is for speculative parsing.
      true indicates that it is for speculation.
  *)
  val checkpoint : bool -> handle -> unit
  val commit :     handle -> unit
  val restore :    handle -> unit
end

(** 
  Raised when an error is encounterd during a speculative
  parse. Speculative parses are used by datatypes to try out the
  different variants.
*)
exception Speculation_failure

val find_eor : pd_header -> handle -> pd_header

(** Append an open record marker (as applicable) to the current out stream. 
    The out stream must be open.
*)
val print_open_rec : handle -> unit

(** Append a close record marker (as applicable) to the current out stream. 
    The out stream must be open.
*)
val print_close_rec : handle -> unit

module Record : sig
  val create_pd_hdr : handle -> pd_header
  val finish_pd_hdr : pd_header -> handle -> pd_header

  (* Update an existing hdr and with the pd from a subcomponent. *)
  val update_pd_hdr : pd_header -> pd_header -> pd_header

  val parse_first : ('a,'b) parser -> handle -> 'a * ('b pd) * pd_header
  val parse_next : ('a,'b) parser -> pd_header -> handle -> 'a * ('b pd) * pd_header

  val absorb_first : ('a,'b) parser -> handle -> pd_header
  val absorb_next : ('a,'b) parser -> pd_header -> handle -> pd_header

  val absorb_first_char : lit:char -> handle -> pd_header
  val absorb_next_char  : lit:char -> pd_header -> handle -> pd_header

  val absorb_first_string :  lit:string -> handle -> pd_header
  val absorb_next_string  :  lit:string -> pd_header -> handle -> pd_header

  val absorb_first_int : lit:int -> handle -> pd_header
  val absorb_next_int  : lit:int -> pd_header -> handle -> pd_header

  val absorb_first_regexp :  lit:reg_exp -> handle -> pd_header
  val absorb_next_regexp  :  lit:reg_exp -> pd_header -> handle -> pd_header

  val absorb_first_regexp_str :  lit:string -> handle -> pd_header
  val absorb_next_regexp_str  :  lit:string -> pd_header -> handle -> pd_header

  val gen_pd : pd_header -> 'a pd -> 'a pd * pd_header
end

module Compute : sig

  (** Make a parsing function from a computed value and gen_pd function. *)
  val generate_parser : 'a -> ('a -> 'b pd) -> ('a,'b) parser

end

module Where : sig
  val gen_pd : 'a pd -> bool -> ('a pd) pd 
  val make_pd : 'a pd -> bool -> handle -> ('a pd) pd
  val parse_underlying : ('a,'b) parser -> ('a -> bool) -> ('a,('b pd)) parser
end

module Datatype : sig
  val parse_variant  : ('a,'b) parser -> handle -> ('a * ('b pd)) option
  val absorb_variant : ('a,'b) parser -> handle -> span option
  val absorb_char_variant   : lit:char   -> handle -> span option
  val absorb_string_variant : lit:string -> handle -> span option
  val absorb_int_variant    : lit:int    -> handle -> span option
  val absorb_regexp_variant : lit:reg_exp -> handle -> span option
  val absorb_regexp_str_variant : lit:string -> handle -> span option

  val parse_case  : ('a,'b) parser -> ('a -> 'c) -> ('b pd -> 'd) -> ('c, 'd) parser
  val absorb_case : ('a,'b) parser -> 'c -> 'd -> ('c, 'd) parser
  val absorb_char_case   : char -> 'a -> 'b -> ('a, 'b) parser
  val absorb_string_case : string -> 'a -> 'b -> ('a, 'b) parser
  val absorb_int_case    : int -> 'a -> 'b -> ('a, 'b) parser
  val absorb_regexp_case : reg_exp -> 'a -> 'b -> ('a, 'b) parser
  val absorb_regexp_str_case : string -> 'a -> 'b -> ('a, 'b) parser

  (** args: gen_rep genpd_fn rep_constructor pd_constructor *)
  val gen_case : 'a -> ('a -> 'b pd) -> ('a -> 'c) -> ('b pd -> 'd) -> ('c,'d) parser
  
  val make_pd_hdr : pd_header -> pd_header
  val make_rep : 'a -> 'a

  (** Create a pd when there are no errors. *)   
  val make_pd : pd_header * 'a -> 'a pd

  val make_err_pd : handle -> 'a -> 'a pd
  (** If speculative, raise exception. Otherwise, return pd. *)
  val handle_error_variant : handle -> 'a -> 'a pd

(*   val make_gen_pd : handle -> 'a -> 'a pd *)
  val make_absorb_pd : span -> 'a -> 'a pd

  val gen_pd : 'a pd -> ('a pd -> 'b) -> 'b pd
  (* Generate the pd when the variant has no subcomponent. *)
  val gen_pd_empty : 'b -> 'b pd
end
