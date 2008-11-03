(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                 *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* /boomerang/src/registry.ml                                                 *)
(* Boomerang run-time registry                                                *)
(* $Id$ *)
(******************************************************************************)

let sprintf = Printf.sprintf
let debug = Trace.debug "registry"
let verbose = Trace.debug "registry+"
let msg = Util.format

(* finite maps *)
module QM = Bident.Qid.Env

(* --------------- Identifier parsing -------------- *)
let parse_uid s =  
  let lexbuf = Lexing.from_string s in
    Blexer.setup "identifier constant";
    let x = 
      try Bparser.uid Blexer.main lexbuf
      with _ -> 
        raise 
          (Error.Harmony_error
             (fun () -> 
                msg "@[%s:@ syntax@ error@ in@ identifier@ %s.@]" 
                  (Info.string_of_t (Blexer.info lexbuf))
                  s)) in 
      Blexer.finish ();                    
      x

let parse_qid s =
  let lexbuf = Lexing.from_string s in
    Blexer.setup "qualified identifier constant";
    let q =
      try Bparser.qid Blexer.main lexbuf
      with _ -> raise
        (Error.Harmony_error
           (fun () ->
              msg "@[%s:@ syntax@ error@ in@ qualified@ identifier@ %s.@]"
                (Info.string_of_t (Blexer.info lexbuf))
                s)) in
      Blexer.finish ();
      q

(* --------------- Registry values -------------- *)
type rs = 
  | Sort of Bsyntax.sort      
  | Unknown 

type rv = rs * Bvalue.t

(* utility functions *)

let value_of_rv (_,v) = v

let format_rv (rs,v) = 
  msg "@[";
  Bvalue.format v;
  (match rs with
    | Sort(s)     -> msg ":@ ";  Bprint.format_sort s
    | Unknown     -> ());
    msg "@]"

(* type abbreviation for the constructors for a type *)
type tcon = Bident.Qid.t * Bsyntax.sort option
type tspec = Bident.Id.t list * tcon list

(* --------------- Focal library -------------- *)
(* state *)
module type REnvSig = 
sig
  type t
  val empty : unit -> t
  val lookup : t -> Bident.Qid.t -> rv option
  val lookup_type: t -> Bident.Qid.t -> (Bident.Qid.t * tspec) option
  val lookup_con : t -> Bident.Qid.t -> (Bident.Qid.t * tspec) option
  val update : t -> Bident.Qid.t -> rv -> t
  val update_type : t -> Bident.Id.t list -> Bident.Qid.t -> tcon list -> t
  val overwrite : t -> Bident.Qid.t -> rv -> unit
  val iter : (Bident.Qid.t -> rv -> unit) -> t -> unit
  val iter_type : (Bident.Qid.t -> tspec -> unit) -> t -> unit
  val fold : (Bident.Qid.t -> rv -> 'a -> 'a) -> t -> 'a -> 'a
end

module REnv : REnvSig = 
struct  
  (* "type map" from names (Prelude.list) to type variables (['a]) and
     constructors / optional sorts (["Nil", None; "Cons", 'a * 'a list]) *)
  type tmap = (Bident.Id.t list * tcon list) QM.t

  (* "reverse type map" from constructor names (Prelude.Nil) to types (Prelude.list) *)
  type rmap = Bident.Qid.t QM.t 

  (* enviroments are have two components: for types and values *)
  type t = (tmap * rmap) * (rv ref) QM.t

  let empty () = ((QM.empty (), QM.empty ()), QM.empty ())

  let lookup (_,ve) q = match QM.lookup ve q with 
      Some r -> Some (!r)
    | None -> None

  let lookup_type ((tm,_),_) q = 
    match QM.lookup tm q with
      | None -> None
      | Some r -> Some (q,r)
      
  let lookup_con ((tm,rm),_) q = match QM.lookup rm q with 
    | None -> None
    | Some q' -> begin match QM.lookup tm q' with 
        | None -> Berror.run_error (Info.M "Bregistry.lookup_con") 
            (fun () -> msg "datatype %s missing" 
               (Bident.Qid.string_of_t q'))
        | Some (sl,cl) -> Some (q',(sl,cl))
      end
  
  let update (te,ve) q v = 
    (te, QM.update ve q (ref v))

  let update_type (((tm,rm),ve)) svl q cl = 
    let tm' = QM.update tm q (svl,cl) in 
    let rm' = 
      Safelist.fold_left 
        (fun rmi (qi,so) -> QM.update rmi qi q)
        rm cl in 
    ((tm',rm'),ve)

  let overwrite (te,ve) q v = 
    match QM.lookup ve q with 
        Some r -> r:=v
      | None -> 
          raise (Error.Harmony_error
                   (fun () -> msg "Registry.overwrite: %s not found" 
                      (Bident.Qid.string_of_t q)))
  let iter f (_,ve) = 
    QM.iter (fun q rvr -> f q (!rvr)) ve
  let iter_type f ((tm,_),_) = 
    QM.iter f tm 
  let fold f (_,ve) init = 
    QM.fold (fun q rvr a -> f q (!rvr) a) ve init
end

let loaded = ref ["Native"]

let library : REnv.t ref = ref (REnv.empty ())

(* constants *)
let pre_ctx = Safelist.map parse_uid [ "Core" ; "Prelude" ]

(* utilities *)
let get_library () = !library

(* hack to reset library to native stuff in visualizer *)
let old_library = ref None
let reset () = 
  if !old_library = None then old_library := Some !library;
  loaded := [];
  library := 
    match !old_library with 
        Some l -> l
      | None -> assert false

(* --------------- Lookup functions -------------- *)

let paths = Prefs.createStringList 
  "include" 
  "search path for .boom sources"
  "Boomerang modules are loaded, compiled, and registered on-demand. The search path specifies where the run-time system should search for module sources (the current directory and the paths specified in the BOOMPATH environment variable are also searched)."
let _ = Prefs.alias paths "I"

let boompath =
  try Util.splitIntoWords (Unix.getenv "BOOMPATH") ':'
  with Not_found -> ["."]

(* get the filename that a module is stored at *)
let get_module_prefix q = 
  match Bident.Qid.qs_of_t q with 
    | [] -> None
    | n::_ -> Some n

let find_filename basename exts = 
  let rec loop dirs = match dirs with
    | []    -> None
    | dir::drest ->
        let try_fn ext k = 
          let fn = sprintf "%s%s%s%s%s"
            dir
            (if dir.[String.length dir - 1] = '/' then "" else "/")
            basename 
            (if String.length ext = 0 then "" else ".")
            ext in 
            if Sys.file_exists fn && Misc.is_file fn then begin
              verbose (fun () -> msg "%s found for %s@\n" fn basename);
              Some fn
            end else begin
              verbose (fun () -> msg "%s not found@\n" fn);
              k ()
            end in
        let rec inner_loop = function
            [] -> try_fn "" (fun () -> loop drest)                
          | ext::erest -> try_fn ext (fun () -> inner_loop erest) in
          inner_loop exts              
  in
    loop ((Safelist.rev (Prefs.read paths) @ boompath))

(* backpatch hack *)
let interp_file_impl = 
  ref (fun _ _ -> 
         msg "@[Boomerang compiler is not linked! Exiting...@]"; 
         exit 2)

let interp_string_impl = 
  ref (fun _ _ _ -> 
         msg "@[Boomerang compiler is not linked! Exiting...@]"; 
         exit 2)  

let load ns = 
  (* helper, when we know which compiler function to use *)  
  let go comp source = 
    debug (fun () -> msg "[@[loading %s ...@]]@\n%!" source);
    loaded := ns::(!loaded);
    comp ();
    debug (fun () -> msg "[@[loaded %s@]]@\n%!" source) in      
  let go_wrap m = 
    if (Safelist.mem ns (!loaded)) then true
    else begin
      match find_filename m ["boom"; "src"] with 
        | None -> 
            begin
              try 
                (* check for baked in source *)
                let str = Hashtbl.find Bakery.items m in 
                let i = sprintf "<baked source for %s>" ns in  
                  go (fun () -> (!interp_string_impl) i str ns) i;
                  true
              with Not_found -> false
            end
        | Some fn ->
            go (fun () -> (!interp_file_impl) fn ns) fn; 
            true 
    end in 
    if go_wrap ns then true
    else go_wrap (String.uncapitalize ns)
      
let load_var q = match get_module_prefix q with 
  | None -> ()
  | Some n -> ignore (load (Bident.Id.string_of_t n))

(* lookup in a naming context, with a lookup function *)
let lookup_library_generic lookup_fun nctx q = 
  verbose (fun () -> msg "lookup_library_generic [%s] [%s]@\n%!" 
           (Bident.Qid.string_of_t q)
           (Misc.concat_list "," (Safelist.map Bident.Qid.string_of_t nctx)));
  let rec lookup_library_aux nctx q2 =       
    verbose (fun () -> msg "lookup_library_aux [%s] [%s]@\n%!" 
             (Bident.Qid.string_of_t q2)
             (Misc.concat_list "," (Safelist.map Bident.Qid.string_of_t nctx)));
    let try_lib () = lookup_fun !library q2 in
      (* try here first, to avoid looping on native values *)
      match try_lib () with
        | Some r -> Some r
        | None -> 
            begin match load_var q2; try_lib () with
              | Some r -> Some r
              | None -> match nctx with 
                  | []       -> None
                  | o::orest -> 
                      lookup_library_aux orest (Bident.Qid.t_dot_t o q) 
            end
  in
  lookup_library_aux nctx q

let lookup_library_ctx os q = 
  verbose (fun () -> msg "lookup_library_ctx [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_generic REnv.lookup os q

let lookup_type_library_ctx os q = 
  verbose (fun () -> msg "lookup_type_library_ctx [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_generic REnv.lookup_type os q

let lookup_con_library_ctx os q = 
  verbose (fun () -> msg "lookup_con_library_ctx [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_generic REnv.lookup_con os q

let lookup_library q = 
  verbose (fun () -> msg "lookup_library [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_ctx [] q

let lookup_type_library q = 
  verbose (fun () -> msg "lookup_type_library [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_type_library_ctx [] q

let lookup_con_library q = 
  verbose (fun () -> msg "lookup_con_library [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_con_library_ctx [] q

(* --------------- Registration functions -------------- *)

(* lookup in a naming context, with a lookup function *)
let resolve_library lookup_fun nctx m q = 
  verbose (fun () -> msg "resolve_library [%s] [%s] in [%s]@\n" 
           (Bident.Qid.string_of_t q)
           (Misc.concat_list "," (Safelist.map Bident.Qid.string_of_t nctx))
	   (Bident.Id.string_of_t m));
  let rec resolve_library_aux nctx q2 =       
    verbose (fun () -> msg "resolve_library_aux [%s] [%s]@\n" 
             (Bident.Qid.string_of_t q2)
             (Misc.concat_list "," (Safelist.map Bident.Qid.string_of_t nctx)));
    match lookup_fun !library q2 with
      | Some r -> q2
      | None -> begin match nctx with 
          | []       -> Bident.Qid.id_dot m q
          | o::orest -> 
              resolve_library_aux orest (Bident.Qid.t_dot_t o q) 
        end
  in
  resolve_library_aux nctx q

(* --------------- Registration functions -------------- *)

(* register a value *)
let register q r = 
  library := (REnv.update (!library) q r)

let register_type q (svl,cl) = 
  library := (REnv.update_type (!library) svl q cl)

let register_native_qid q s v = 
  register q (Sort s,v)

(* register a native value *)
let register_native qs s v = 
  register_native_qid (parse_qid qs) s v

(* register a whole (rv Env.t) in m *)
let register_env ev nctx m = 
  let qualify_rv (rs,v) =
    let new_rs = match rs with
      | Sort s -> 
          Sort (Bsubst.qualify_sort (resolve_library REnv.lookup nctx m) [] s)
      | Unknown -> Unknown in
    (new_rs,v) in
  REnv.iter (fun q r -> register (Bident.Qid.id_dot m q) (qualify_rv r)) ev;  
  REnv.iter_type 
    (fun q ts -> 
       let (svl,cl) = ts in 
       let cl' = Safelist.map (fun (x,so) -> (Bident.Qid.id_dot m x,so)) cl in 
       register_type q (svl,cl')) ev
