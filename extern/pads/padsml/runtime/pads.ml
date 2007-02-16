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
(** parsing flags that can affect parsing *)
type parse_state = bool * bool
    
let ps_init = (false,false)

let ps_set_panic (_,par) = (true,par)
let ps_unset_panic (_,par) = (false,par)
let ps_is_panic (p,_) =  p

let ps_set_partial (pan,_) = (pan,true)
let ps_unset_partial (pan,_) = (pan,false)
let ps_is_partial (_,p) =  p

(** position in data source *)
type pos = int64

(** span in data source *)
type span = (pos * pos) option

(** arguments: function name, span and error message. *)
exception Runtime_error of string * span * string

(* XXX: I think that all or nearly all uses of results should be replaced by exceptions. *)

type 'a result = Ok of 'a | Error

type error_code =
    Good                   (* all good recursively *)
  | Maybe                  (* possible error, possibly not *)
  | Nest of span           (* nested error at specified span. *)
  | Syn                    (* syntactic error *)
  | Sem                    (* do we want to say somehow what the
                              semantic constraint was or why it
                              was violated?  How would we say that?
                              Perhaps use an 'a pd with clause
                              Sem of 'a -> bool  where the function is the
                              semantic function that returns false? *)

type corrupted = string   (* the raw string that we couldn't parse. *)

(** detailed information about the error. *)
type error_info =
    No_info
  | Padsc_error_code of Padsc.perrCode_t
  | Error_span of span
  | Corrupted_data of corrupted
  | Unexpected_data_before_eor
  | Eof_before_eor

type pd_header   = {
  state        : parse_state;
  nerr         : int;
  error_code   : error_code;
  error_info   : error_info;
  span         : span
}

type padsc_handle = Padsc.p_t Com.opaque
type handle = padsc_handle * (Padsc.sfioPtr ref)

type 'a pd = pd_header * 'a
type base_pd_body = unit
type base_pd     = unit pd

type ('a,'b) parser = handle -> 'a * ('b pd)
type ('a,'b) printer = 'a -> ('b pd) -> handle -> unit

type reg_exp = Padsc.pregexp_t Com.opaque  

let get_padsc_handle (h,_) = h
let get_out_stream (_,os_ref) = !os_ref

let string_of_pos p = Int64.to_string p

let string_of_span = function
    None -> "No span recorded"
  | Some (b,e) -> string_of_pos b ^ " - " ^ string_of_pos e

let string_of_error_code = function
    Good -> "Good"
  | Maybe -> "Maybe"
  | Nest sp -> "Nested error at " ^ string_of_span sp
  | Syn -> "Syn"
  | Sem -> "Sem"

let string_of_error_info = function
    No_info -> "No information"
  | Padsc_error_code _ -> "PADS/C error code"
  | Error_span sp -> "Error at span " ^ string_of_span sp
  | Corrupted_data d -> "Corrupted data: " ^ d
  | Unexpected_data_before_eor -> "Unexpected data before EOR"
  | Eof_before_eor -> "EOF before EOR"

let comp_pos p1 p2 = Int64.compare p1 p2

let eq_pos p1 p2 = p1 = p2

let get_begin_pos = function Some (b,e) -> b

let get_end_pos = function Some (b,e) -> e

let make_incl_span ~b ~e = Some (b, e)

let make_excl_span ~b ~e = Some (b, Int64.pred e)

let merge_spans ~first ~second = 
  match (first,second) with
       (Some (b,_), Some (_,e)) -> Some(b,e)

let make_valid_pd_hdr sp = 
  {state= ps_init;
   nerr= 0; 
   error_code = Good; 
   error_info = No_info;
   span= sp}

let spanless_pd_hdr = make_valid_pd_hdr None

(* Make a pd header for case when we encounter an error while parsing a literal. *)
let make_lit_error_hdr s  = 
  {state = ps_init; nerr = 1; error_code = Syn;
   error_info = Error_span s; span = s}
    
let gen_base_pd = (spanless_pd_hdr,())

let get_pd_hdr (h,_) = h

let pd_is_ok (h,_) = h.nerr = 0

(* PADS/C --->  PADS/ML: Conversion functions from PADS/C to PADS/ML. *)

let parse_state_of_pflags f =
  (* We can cut it down to an int first because we know we only care
    about lower bits. *)
  let int_f = Int32.to_int f in
  let panic = (int_f land 0x0001) > 0 in
  let partial = (int_f land 0x0002) > 0 in
    (panic,partial)  

let pflags_of_parse_state (panic,partial) =
  let f = if panic then 0x0001 else 0x0 in
  let f = if partial then f lor 0x0002 else f in
    Int32.of_int f

(* Conversion function between padsc results (type perror_t) and
   padsml results (type result). *)

let result_of_perror_t v = 
  function
      Padsc.P_OK -> Ok v
    | Padsc.P_ERR -> Error
	
(* Conversion function between padsc error codes (type perrCode_t) and
   padsml error codes (type error_code).  *)

let ec_of_pec = function
    Padsc.P_NO_ERR -> Good
  | _ -> Syn

(* Conversion function between padsc error codes (type perrCode_t) and
   padsml error information (type error_info). *)

let ei_of_pec = function 
    Padsc.P_EXTRA_BEFORE_EOR -> Unexpected_data_before_eor
  | Padsc.P_EOF_BEFORE_EOR -> Eof_before_eor
  | pec -> Padsc_error_code pec

let pec_of_ei = function
    Padsc_error_code ec -> ec
  | No_info -> Padsc.P_NO_ERR
  | Unexpected_data_before_eor -> Padsc.P_EXTRA_BEFORE_EOR
  | Eof_before_eor -> Padsc.P_EOF_BEFORE_EOR
  | _ -> Padsc.P_UNEXPECTED_ERR

(* Ploc to span. *)
let span_of_loc (l:Padsc.ploc_t) = Some (l.Padsc.b.Padsc.offset, l.Padsc.e.Padsc.offset)
let bpos_of_loc (l:Padsc.ploc_t) = l.Padsc.b.Padsc.offset
let epos_of_loc (l:Padsc.ploc_t) = l.Padsc.e.Padsc.offset

let loc_of_span  = function
    None ->
      let bpos = {Padsc.fIELD_BYTE = 0; Padsc.num = 0; Padsc.offset = Int64.zero} in
      let epos = {Padsc.fIELD_BYTE = 0; Padsc.num = 0; Padsc.offset = Int64.zero} in
	{Padsc.b = bpos; Padsc.e = epos}
  | Some (b,e) ->
      let bpos = {Padsc.fIELD_BYTE = 0; Padsc.num = 0; Padsc.offset = b} in
      let epos = {Padsc.fIELD_BYTE = 0; Padsc.num = 0; Padsc.offset = e} in
	{Padsc.b = bpos; Padsc.e = epos}

(* PADS/C base pd to PADS/ML base pd. *)
let base_pd_of_pbase_pd 
    { Padsc.pstate = pstate;
      Padsc.nerr = nerr;
      Padsc.errCode = errCode;
      Padsc.loc = loc;} 
    current_pos =
  let span = 
    if nerr = 0 then
      (* End loc is not set in PADS/C runtime, so must set it now. *)
      make_excl_span (bpos_of_loc loc) current_pos
    else 
      (* There was an error, so the end pos. is set. *)
      span_of_loc loc 
  in
  let hdr = {state      = parse_state_of_pflags pstate;
	     nerr       = nerr;
	     error_code = ec_of_pec errCode;
	     error_info = ei_of_pec errCode;
	     span       = span} in
    (hdr,())

let pbase_pd_of_base_pd 
    ({state = state; nerr = nerr;
      error_info = ei; span = sp}, ()) =
  {Padsc.pstate = pflags_of_parse_state state;
   Padsc.nerr = nerr;
   Padsc.errCode = pec_of_ei ei;
   Padsc.loc = loc_of_span sp;}

(* INTERNAL LOGGING *)

module Log = struct

  type report_level =
      Info_level
    | Warning_level
    | Error_level
    | Fatal_level

  let (pc_LEV_INFO,
       pc_LEV_WARN, 
       pc_LEV_ERROR, 
       pc_LEV_FATAL) =
    Padsc.p_get_error_levels ()


  let pc_lev_of_lev = function
      Info_level -> pc_LEV_INFO
    | Warning_level -> pc_LEV_WARN
    | Error_level -> pc_LEV_ERROR
    | Fatal_level -> pc_LEV_FATAL
      

  let report level ~fun_name span error_info ~msg pads = 
    ignore (Padsc.pDCI_report_err 
      (get_padsc_handle pads) 
      (pc_lev_of_lev level)
      (loc_of_span span) 
      (pec_of_ei error_info)
      fun_name msg)

  let report_info = report Info_level
  let report_warning = report Warning_level
  let report_error = report Error_level
  let report_fatal fun_name span error_info msg pads = 
    report Fatal_level fun_name span error_info msg pads;
    (* This line is never reached as Fatal_level kills the program. 
       We include the code so that the type checker will assign it type 'a. *)
    raise Exit 
    
end

(* Span and pos-related functions that use the pads handle. *)
 
let get_current_pos pads = 
  match Padsc.p_io_getPos (get_padsc_handle pads) 0 with
      (Padsc.P_OK,{Padsc.offset = p}) -> p  (* PADS/ML positions are simply offsets, 
					 rather than the more complex positions 
					 of pads/c. *)     
    | (Padsc.P_ERR,_) -> Log.report_fatal "Pads.get_current_pos" None (Padsc_error_code Padsc.P_IO_ERR) "Failed to get current position" pads (* Should never happen. *)

let make_empty_span pads = 
  let p = get_current_pos pads in make_excl_span p p

let close_span b pads =
  make_excl_span b (get_current_pos pads)

let make_empty_pd_hdr pads = make_valid_pd_hdr (make_empty_span pads)

let make_empty_pd pads = (make_empty_pd_hdr pads,())
    
(* These functions dealt with pstrings when they were opaque. Hence,
   no longer applicable. *)
(* let string_of_pstring = Padsc.string_of_pstring *)

(* let pstring_of_string s pads = *)
(*   let len = String.length s in *)
(*     match (Padsc.pstring_cstr_copy (get_padsc_handle pads) s len) with *)
(* 	(Padsc.P_OK,pstring) -> pstring *)
(*       | (Padsc.P_ERR,_) -> Log.report_fatal "pstring_of_string" None No_info  *)
(* 	  "C string to Pstring copy failed." pads *)
	  

(* SCANNING FUNCTIONS *)

let p_char_lit_scan1 pads c = 
  let (e,offset) = Padsc.pchar_lit_scan1 (get_padsc_handle pads) c 1 0 in
    (result_of_perror_t offset e)

let p_str_lit_scan1 pads s = 
  let (e,offset) = Padsc.pstr_lit_scan1 (get_padsc_handle pads) s 1 0 in
    (result_of_perror_t offset e)

let p_int_lit_scan1 pads i = 
  let (e,offset) = Padsc.pstr_lit_scan1 (get_padsc_handle pads) (string_of_int i) 1 0 in
    (result_of_perror_t offset e)

let p_regexp_scan1 pads regexp = 
  let (e,offset) = Padsc.pre_scan1 (get_padsc_handle pads) regexp 1 0 in
    (result_of_perror_t offset e)

let p_regexp_str_scan1 pads regexp_str =
    (* XXX: use exceptions here for error handling. *)
    let ph = get_padsc_handle pads in
    let (err,re) = Padsc.pregexp_alloc ph in
    let err = Padsc.pregexp_compile_cstr ph regexp_str re in
    let res = p_regexp_scan1 pads re in
    let err = Padsc.pregexp_cleanup ph re in
    let err = Padsc.pregexp_free ph re in
      res

let print_char_lit c pads = 
  ignore (Padsc.pchar_lit_write2io (get_padsc_handle pads) (get_out_stream pads) c)

let print_str_lit s pads = 
  ignore (Padsc.pstr_lit_write2io (get_padsc_handle pads) (get_out_stream pads) s)

let open_handle_with_rec_disc rec_disc =
  let (ec,h) =  Padsc.p_open None rec_disc in
    match Padsc.p_fopen "/dev/stdout" "a" with
	None -> Error
      | Some out -> result_of_perror_t (h,ref out) ec

(** Open the handle, and set the io discipline to use newline-terminated records. *)
let open_handle () = open_handle_with_rec_disc (Padsc.p_ctrec_make (int_of_char '\n') 0)

(** Open the handle, and set the io discipline not to use records. *)
let open_handle_norec () = open_handle_with_rec_disc (Padsc.p_norec_make 0)

let close_handle pads = result_of_perror_t () (Padsc.p_close (get_padsc_handle pads))  

let share_handle (ph,out_ref) = let out = !out_ref in (ph, ref out)

type timezone = Padsc.tm_zone_t

let timezone_of_string = Padsc.p_cstr2timezone

module IO = struct
  let open_in_file pads file_name = result_of_perror_t () (Padsc.p_io_fopen (get_padsc_handle pads) file_name)
  let open_file = open_in_file

  let open_out_file (_, out_ref) file_name = 
    match Padsc.p_fopen file_name "w" with
	None -> Error
      | Some out -> (out_ref := out; Ok ())
	 
  let open_add_out_file pads file_name = 
    match Padsc.p_fopen file_name "w" with
	None -> Error
      | Some out -> Ok (get_padsc_handle pads, ref out)

  let close (ph, out_ref) = 
    let res1 = Padsc.p_io_close ph  in
    let res2 = Padsc.p_fclose !out_ref in
      match (res1,res2) with
	  (Padsc.P_OK, Padsc.P_OK) -> Ok ()
	| (Padsc.P_ERR,_) -> Error
	| (_,Padsc.P_ERR) -> Error

  let checkpoint speculative pads = 
    let spec = if speculative then 1 else 0 in
      ignore (Padsc.p_io_checkpoint (get_padsc_handle pads)  spec)
  let commit pads = ignore (Padsc.p_io_commit (get_padsc_handle pads)) 
  let restore pads = ignore (Padsc.p_io_restore (get_padsc_handle pads)) 
  let is_speculative pads = (Padsc.p_is_current_spec (get_padsc_handle pads)) > 0
  let get_spec_level pads = Padsc.p_spec_level (get_padsc_handle pads)
end

(** 
  Raised when an error is encounterd during a speculative
  parse. Speculative parses are used by datatypes to try out the
  different variants.
*)
exception Speculation_failure

let find_eor hdr pads = 
  let b_pos = get_current_pos pads in
  let res, v = Padsc.p_io_next_rec (get_padsc_handle pads) in
    match result_of_perror_t v res with
	Ok 0 -> 
	  if not (ps_is_panic hdr.state) then hdr
	  else {hdr with state = ps_unset_panic hdr.state}
      | Ok bytes_skipped ->
	  let err_span = close_span b_pos pads in
	    if ps_is_panic hdr.state then
	      (Log.report_info "Pads.Record.find_eor" err_span 
		 (Padsc_error_code Padsc.P_NO_ERR)
		 "Resynching at EOR" pads;
	       {hdr with state = ps_unset_panic hdr.state})
	    else
	      (Log.report_warning "Pads.Record.find_eor" err_span 
		 (Padsc_error_code Padsc.P_EXTRA_BEFORE_EOR)
		 "Unexpected data before EOR" pads;
	       if IO.is_speculative pads then
		 raise Speculation_failure
	       else
		 let new_state = ps_unset_panic hdr.state in
		 let new_nerr = hdr.nerr + 1 in
		 let new_ec, new_einfo = 
		   if new_nerr = 1 then
		     Nest err_span, Unexpected_data_before_eor 
		   else hdr.error_code, hdr.error_info
		 in
		   {state=new_state; nerr=new_nerr; 
		    error_code=new_ec; error_info=new_einfo; 
		    span=hdr.span})		    
      | Error -> 
	  let err_span = close_span b_pos pads in
	    Log.report_warning "Pads.Record.find_eor" err_span 
	      (Padsc_error_code Padsc.P_EOF_BEFORE_EOR)
	      "Found EOF when searching for EOR" pads;
	    if IO.is_speculative pads then
	      raise Speculation_failure
	    else	      
	      let new_state = ps_unset_panic hdr.state in
	      let new_nerr = hdr.nerr + 1 in
	      let new_ec, new_einfo = 
		if new_nerr = 1 then
		  Nest err_span, Eof_before_eor 
		else hdr.error_code, hdr.error_info
	      in
		{state=new_state; nerr=new_nerr; 
		 error_code=new_ec; error_info=new_einfo; 
		 span=hdr.span}

let print_open_rec pads = 
  ignore (
    Padsc.pDCI_io_rec_open_write2io 
      (get_padsc_handle pads)
      (get_out_stream pads)
      "Pads.print_open_rec"
  )
    
let print_close_rec pads = 
  ignore (
    Padsc.pDCI_io_rec_close_write2io 
      (get_padsc_handle pads)
      (get_out_stream pads)
      "Pads.print_close_rec"
  )

module Record = struct

  let create_pd_hdr pads = 
    (* finish_pd_hdr will adjust the span to be non-empty. *)
    make_valid_pd_hdr (make_empty_span pads)

  let finish_pd_hdr hdr pads =
    let b_pos = 
      try
	get_begin_pos hdr.span
      with
	  (* should never happen. finish_pd_hdr should always be
	     paired with create_pd_hdr to ensure that span exists. *)
	  Match_failure _ -> 
	    Log.report_fatal "Pads.Record.finish_pd_hdr" None (Padsc_error_code Padsc.P_SYS_ERR) "Misuse of finish_pd_hdr: not paired with create_pd_hdr" pads (* Should never happen. *)
    in
      {hdr with span = close_span b_pos pads}

  (* Update an existing header with the pd from a subcomponent. *)
  let update_pd_hdr h sub =
    if sub.nerr = 0 then h
    else
      let new_state = if ps_is_panic sub.state then 
	ps_set_panic h.state else h.state in
      let new_nerr = h.nerr + 1 in
      let new_ec, new_einfo = 
	if new_nerr = 1 then 
	  match sub.error_code with
	      Nest sp -> Nest sp, No_info
	    | Syn | Sem -> Nest sub.span, No_info
	    | Good | Maybe -> 
		(* These codes shouldn't be listed if nerr > 0 *)
		raise (Runtime_error ("Pads.Record.update_pd_hdr", None,
				     "Invariant violated: nerr > 0, but error code is Good or Maybe"))
	else h.error_code, h.error_info
      in
        {state=new_state; nerr=new_nerr; error_code=new_ec;
	 error_info=new_einfo; span=h.span}

  (* Process result of parsing subcomponent. 
     h : record header.
     sub : subcomponent header.
  *)
  let process_result h sub pads = 
    if sub.nerr > 0 && IO.is_speculative pads then
      raise Speculation_failure
    else
      update_pd_hdr h sub

  (* Process result of scanning for literal. *)
  let process_scan_result h res span pads = 
    match res with
	Ok 0 -> update_pd_hdr h (make_valid_pd_hdr span)
      | _ -> 
	  if IO.is_speculative pads then
	    raise Speculation_failure
	  else
	    update_pd_hdr h (make_lit_error_hdr span)

  let parse_next parse_fn hdr pads =
    let rep,pd = parse_fn pads in
    let h = get_pd_hdr pd in
      rep,pd,process_result hdr h pads

  let parse_first parse_fn pads =
    let initial_hdr = create_pd_hdr pads in
      parse_next parse_fn initial_hdr pads

  let absorb_next parse_fn hdr pads =
    let _,pd = parse_fn pads in 
    let h = (get_pd_hdr pd) in
      process_result hdr h pads

  let absorb_first parse_fn pads =
    let initial_hdr = create_pd_hdr pads in
      absorb_next parse_fn initial_hdr pads

  let absorb_next_literal scan_lit ~lit hdr pads = 
    let begin_pos = get_current_pos pads in
    let res = scan_lit pads lit in
    let end_pos = get_current_pos pads in
    let lit_span = make_excl_span begin_pos end_pos in
      process_scan_result hdr res lit_span pads
	  
  let absorb_first_literal scan_lit ~lit pads = 
    let initial_hdr = create_pd_hdr pads in
      absorb_next_literal scan_lit ~lit:lit initial_hdr pads
	  
  let absorb_first_char   = absorb_first_literal p_char_lit_scan1
	  
  let absorb_next_char    = absorb_next_literal p_char_lit_scan1
	    
  let absorb_first_string = absorb_first_literal p_str_lit_scan1

  let absorb_next_string  = absorb_next_literal p_str_lit_scan1
	    
  let absorb_first_int    = absorb_first_literal p_int_lit_scan1

  let absorb_next_int     = absorb_next_literal p_int_lit_scan1

  let absorb_first_regexp = absorb_first_literal p_regexp_scan1

  let absorb_next_regexp  = absorb_next_literal p_regexp_scan1

  let absorb_first_regexp_str = absorb_first_literal p_regexp_str_scan1

  let absorb_next_regexp_str  = absorb_next_literal p_regexp_str_scan1

  let gen_pd hdr sub_pd = 
    let new_pd_hdr = update_pd_hdr hdr (get_pd_hdr sub_pd) in
      sub_pd,new_pd_hdr

end

module Compute = struct

  (* Make a parsing function from a computed value and gen_pd function. 
     pads argument is ignored. It is there so that function has an ('a,'b) parser type.
  *)
  let generate_parser gen_val genpd_fn pads = gen_val, genpd_fn gen_val 

end

module Where = struct

  let gen_pd elt_pd b = 
    let elt_hdr = get_pd_hdr elt_pd in
      if b && elt_hdr.nerr = 0 then 
	({state      = ps_init;
	  nerr       = 0; 
	  error_code = Good; 
	  error_info = No_info;
	  span       = elt_hdr.span},
	 elt_pd) 
      else (* There is some error. *)
	if elt_hdr.nerr = 0 then (* so b = false *)
	  ({state      = elt_hdr.state;
	    nerr       = 1; 
	    error_code = Sem; 
	    error_info = No_info;
	    span       = elt_hdr.span},
	   elt_pd)
	else 
	  ({state      = elt_hdr.state;
	    nerr       = 1; 
	    error_code =
	       (match elt_hdr.error_code with
		   Nest sp -> Nest sp
		 | Syn | Sem -> Nest elt_hdr.span
		 | Good | Maybe -> 
		     (* These codes shouldn't be listed if nerr > 0 *)
		     raise (Runtime_error ("Pads.Where.gen_pd", None,
					  "Invariant violated: nerr > 0, but error code is Good or Maybe"))); 
	    error_info = No_info;
	    span       = elt_hdr.span},
	   elt_pd)

  let make_pd elt_pd b pads = 
    let elt_hdr = get_pd_hdr elt_pd in
      if b && elt_hdr.nerr = 0 then 
	({state      = ps_init;
	  nerr       = 0; 
	  error_code = Good; 
	  error_info = No_info;
	  span       = elt_hdr.span},
	 elt_pd) 
      else (* There is some error. *)
	if IO.is_speculative pads then
	  raise Speculation_failure
	else 
	  if elt_hdr.nerr = 0 then (* so b = false *)
	    ({state      = elt_hdr.state;
	      nerr       = 1; 
	      error_code = Sem; 
	      error_info = No_info;
	      span       = elt_hdr.span},
	     elt_pd)
	  else 
	    ({state      = elt_hdr.state;
	      nerr       = 1; 
	      error_code =
		 (match elt_hdr.error_code with
		      Nest sp -> Nest sp
		    | Syn | Sem -> Nest elt_hdr.span
		    | Good | Maybe -> 
			(* These codes shouldn't be listed if nerr > 0 *)
			Log.report_fatal "Pads.Where.make_pd" None (Padsc_error_code Padsc.P_SYS_ERR) 
			  "Invariant violated: nerr > 0, but error code is Good or Maybe" pads); 
	      error_info = No_info;
	      span       = elt_hdr.span},
	     elt_pd)

  let parse_underlying parse_fn pred pads = 
    let (r, pd) = parse_fn pads in
    let c = pred r in 
      (r, make_pd pd c pads)

end

module Datatype = struct
  let parse_variant parse_fn pads =
    IO.checkpoint true pads;
    let spec_level = IO.get_spec_level pads in
      try 
	let r,p = parse_fn pads in
	  if spec_level = (IO.get_spec_level pads) then (* no nested commit. *)
	    if pd_is_ok p then
	      (IO.commit pads; Some(r,p))
	    else
	      (IO.restore pads; None) (* an error occurred without raising 
					 an exception (e.g. in ptry). *)
	  else (* a nested element committed. *)
	    Some (r,p)
      with
	  Speculation_failure -> 
	    if spec_level = (IO.get_spec_level pads) then 
	      (IO.restore(pads); None)
	    else
	      raise Speculation_failure

  let absorb_variant parse_fn pads =
    IO.checkpoint true pads;
    let spec_level = IO.get_spec_level pads in
      try 
	let r,((h,_) as p) = parse_fn pads in
	  if spec_level = (IO.get_spec_level pads) then (* no nested commit. *)
	    if pd_is_ok p then
	      (IO.commit pads; Some h.span)
	    else
	      (IO.restore pads; None) (* an error occurred without raising 
					 an exception (e.g. in ptry). *)
	  else (* a nested element committed. *)
	    Some h.span
      with
	  Speculation_failure ->
	    if spec_level = (IO.get_spec_level pads) then 
	      (IO.restore(pads); None)
	    else
	      raise Speculation_failure

  let absorb_literal_variant scan_lit ~lit pads =
    IO.checkpoint true pads;
    let begin_pos = get_current_pos pads in
    let res = scan_lit pads lit in
    let end_pos = get_current_pos pads in
    let lit_span = make_excl_span begin_pos end_pos in      
      match res with
	  Ok 0 -> (IO.commit(pads); Some lit_span)
	| _ -> (IO.restore(pads); None)
	
  let absorb_char_variant = absorb_literal_variant p_char_lit_scan1
  let absorb_string_variant = absorb_literal_variant p_str_lit_scan1
  let absorb_int_variant = absorb_literal_variant p_int_lit_scan1
  let absorb_regexp_variant = absorb_literal_variant p_regexp_scan1
  let absorb_regexp_str_variant = absorb_literal_variant p_regexp_str_scan1
		
  let make_pd_hdr sub_hdr = make_valid_pd_hdr sub_hdr.span
      
  let make_rep r = r 

  let make_pd (sub_hdr, pd_body) = (make_pd_hdr sub_hdr, pd_body) 

  let make_err_pd pads err_pd_body =
    ({state      = ps_set_panic ps_init;
      nerr       = 1; 
      error_code = Syn; 
      error_info = No_info;
      span       = make_empty_span pads
     },
     err_pd_body)    

  let handle_error_variant pads err_pd_body = 
    if IO.is_speculative pads then
      raise Speculation_failure
    else make_err_pd pads err_pd_body

(*   let make_gen_pd pads gen_pd_body =  *)
(*     ({state= ps_init; *)
(*       nerr= 0;  *)
(*       error_code=Good;  *)
(*       error_info=No_info; *)
(*       span=make_empty_span pads}, *)
(*      gen_pd_body)     *)

  let make_absorb_pd sp body = (make_valid_pd_hdr sp, body) 
 
  let process_case_result h = 
    if h.nerr = 0 then h
    else {h with nerr = 1}

  let parse_case parse_fn rep_con pd_con pads = 
    let rep,pd = parse_fn pads in
    let hdr = process_case_result (get_pd_hdr pd) in
      rep_con rep,(hdr, pd_con pd)

  let absorb_case parse_fn rep_con pd_con pads = 
    let rep,pd = parse_fn pads in
    let hdr = process_case_result (get_pd_hdr pd) in
      rep_con,(hdr, pd_con)

  let absorb_literal_case scan_lit lit rep_con pd_con pads =
      let begin_pos = get_current_pos pads in
      let res = scan_lit pads lit in
      let end_pos = get_current_pos pads in
      let lit_span = make_excl_span begin_pos end_pos in      
      let hdr = 
	match res with
	    Ok 0 -> make_valid_pd_hdr lit_span
	  | _ -> make_lit_error_hdr lit_span
      in
	rep_con,(hdr,pd_con)	

  let absorb_char_case c = absorb_literal_case p_char_lit_scan1 c
  let absorb_string_case s = absorb_literal_case p_str_lit_scan1 s
  let absorb_int_case i = absorb_literal_case p_int_lit_scan1 i
  let absorb_regexp_case r = absorb_literal_case p_regexp_scan1 r
  let absorb_regexp_str_case rs = absorb_literal_case p_regexp_str_scan1 rs

  let gen_case gen_val genpd_fn = 
    parse_case (Compute.generate_parser gen_val genpd_fn)

  let gen_pd pd pd_con = 
    let hdr = process_case_result (get_pd_hdr pd) in
      (hdr, pd_con pd)

  let gen_pd_empty pd_con = 
    let hdr = make_valid_pd_hdr None in
      (hdr, pd_con)

end


