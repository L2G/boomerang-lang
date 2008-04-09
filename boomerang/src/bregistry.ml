(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/registry.ml                                                  *)
(* Boomerang run-time registry                                                 *)
(* $Id$ *)
(*******************************************************************************)

let sprintf = Printf.sprintf
let debug = Trace.debug "registry"
let verbose = Trace.debug "registry+"

(* --------------- Registry values -------------- *)
type rv = Bsyntax.sort_or_scheme * Bvalue.t

(* utility functions *)
let make_rv s v = (s,v)
let value_of_rv (s,v) = v
let sort_or_scheme_of_rv (s,v) = s
let format_rv rv = 
  let (s,v) = rv in    
    Util.format "@[";
    Bvalue.format v;
    Util.format ":@ ";
    (match s with 
       | Bsyntax.Sort s -> Bsyntax.format_sort s
       | Bsyntax.Scheme s -> Bsyntax.format_scheme s);
    Util.format "]"


(* type abbreviation for the constructors for a type *)
type tcon = Bsyntax.qid * Bsyntax.sort option
type tspec = Bsyntax.svar list * tcon list

(* --------------- Focal library -------------- *)
(* state *)
module type REnvSig = 
sig
  type t
  val empty : unit -> t
  val lookup : t -> Bsyntax.qid -> rv option
  val lookup_type: t -> Bsyntax.qid -> tspec option
  val lookup_con : t -> Bsyntax.qid -> (Bsyntax.qid * tspec) option
  val update : t -> Bsyntax.qid -> rv -> t
  val update_type : t -> Bsyntax.svar list -> Bsyntax.qid -> tcon list -> t
  val overwrite : t -> Bsyntax.qid -> rv -> unit
  val iter : (Bsyntax.qid -> rv -> unit) -> t -> unit
  val iter_type : (Bsyntax.qid -> tspec -> unit) -> t -> unit
  val fold : (Bsyntax.qid -> rv -> 'a -> 'a) -> t -> 'a -> 'a
end

module REnv : REnvSig = 
struct
  module QM = 
    Env.Make(struct
               type t = Bsyntax.qid
               let compare = Bsyntax.qid_compare
               let to_string = Bsyntax.string_of_qid
             end) 
  
  (* "type map" from names (Prelude.list) to type variables (['a]) and
     constructors / optional sorts (["Nil", None; "Cons", 'a * 'a list]) *)
  type tmap = (Bsyntax.svar list * tcon list) QM.t

  (* "reverse type map" from constructor names (Prelude.Nil) to types (Prelude.list) *)
  type rmap = Bsyntax.qid QM.t 

  (* enviroments are have two components: for types and values *)
  type t = (tmap * rmap) * (rv ref) QM.t

  let empty () = ((QM.empty (), QM.empty ()), QM.empty ())

  let lookup (_,ve) q = match QM.lookup ve q with 
      Some r -> Some (!r)
    | None -> None

  let lookup_type ((tm,_),_) q = QM.lookup tm q

  let lookup_con ((tm,rm),_) q = match QM.lookup rm q with 
    | None -> None
    | Some q' -> begin match QM.lookup tm q' with 
        | None -> Berror.run_error (Info.M "Renv.lookup_con") 
            (fun () -> Util.format "datatype %s missing" 
               (Bsyntax.string_of_qid q'))
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
                   (fun () -> Util.format "Registry.overwrite: %s not found" 
                      (Bsyntax.string_of_qid q)))
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
let pre_ctx = Safelist.map Bvalue.parse_uid ["Prelude"]

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

(* --------------- Registration functions -------------- *)

(* register a value *)
let register q r = 
  library := (REnv.update (!library) q r)

let register_type q (svl,cl) = 
  library := (REnv.update_type (!library) svl q cl)

let register_native_qid q s v = 
  register q (Bsyntax.Scheme s,v)

(* register a native value *)
let register_native qs s v = 
  register_native_qid (Bvalue.parse_qid qs) s v

(* register a whole (rv Env.t) in m *)
let register_env ev m = 
  REnv.iter (fun q r -> register (Bsyntax.id_dot m q) r) ev;  
  REnv.iter_type 
    (fun q ts -> 
       let (svl,cl) = ts in 
       let cl' = Safelist.map (fun (x,so) -> (Bsyntax.id_dot m x,so)) cl in 
       register_type (Bsyntax.id_dot m q) (svl,cl')) ev
    
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
  match Bsyntax.qid_qualifiers q with 
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
              verbose (fun () -> Util.format "%s found for %s@\n" fn basename);
              Some fn
            end else begin
              verbose (fun () -> Util.format "%s not found@\n" fn);
              k ()
            end in
        let rec inner_loop = function
            [] -> try_fn "" (fun () -> loop drest)                
          | ext::erest -> try_fn ext (fun () -> inner_loop erest) in
          inner_loop exts              
  in
    loop ((Safelist.rev (Prefs.read paths) @ boompath))

(* load modules dynamically *)
(* backpatch hack *)
let compile_file_impl = ref (fun _ _ -> Util.format "@[Boomerang compiler is not linked! Exiting...@]"; exit 2)  
let compile_boom_str_impl = ref (fun _ _ -> Util.format "@[Boomerang compiler is not linked! Exiting...@]"; exit 2)  

let load ns = 
  (* helper, when we know which compiler function to use *)  
  let go comp source = 
    debug (fun () -> Util.format "[@[loading %s ...@]]@\n%!" source);
    loaded := ns::(!loaded);
    comp ();
    debug (fun () -> Util.format "[@[loaded %s@]]@\n%!" source) in      
  let go_wrap m = 
    if (Safelist.mem ns (!loaded)) then true
    else begin
      match find_filename m ["boom"; "src"] with 
        | None -> 
            begin
              try 
                (* check for baked in source *)
                let str = Hashtbl.find Bakery.items m in 
                  go (fun () -> 
                        (!compile_boom_str_impl) str ns) 
                    (sprintf "<baked source for %s>" ns);
                  true
              with Not_found -> false
            end
        | Some fn ->
            go (fun () -> (!compile_file_impl) fn ns) fn; 
            true 
    end in 
    if go_wrap ns then true
    else go_wrap (String.uncapitalize ns)
      
let load_var q = match get_module_prefix q with 
  | None -> ()
  | Some n -> ignore (load (Bsyntax.string_of_id n))

(* lookup in a naming context, with a lookup function *)
let lookup_library_generic lookup_fun nctx q = 
  debug (fun () -> Util.format "lookup_library_generic [%s] [%s]@\n" 
           (Bsyntax.string_of_qid q)
           (Misc.concat_list "," (Safelist.map Bsyntax.string_of_id nctx)));
  let rec lookup_library_aux nctx q2 =       
    let try_lib () = lookup_fun !library q2 in
      (* try here first, to avoid looping on native values *)
      match try_lib () with
        | Some r -> Some r
        | None -> 
            begin match load_var q2; try_lib () with
              | Some r -> Some r
              | None -> match nctx with 
                  | []       -> None
                  | o::orest -> lookup_library_aux orest (Bsyntax.id_dot o q) 
            end
  in
  lookup_library_aux nctx q

let lookup_library_ctx = 
  lookup_library_generic REnv.lookup

let lookup_type_library_ctx = 
  lookup_library_generic REnv.lookup_type

let lookup_con_library_ctx = 
  lookup_library_generic REnv.lookup_con

let lookup_library q = 
  debug (fun () -> Util.format "lookup_library [%s]@\n" (Bsyntax.string_of_qid q));
  lookup_library_ctx [] q

let lookup_type_library q = 
  debug (fun () -> Util.format "lookup_type_library [%s]@\n" (Bsyntax.string_of_qid q));
  lookup_type_library_ctx [] q

let lookup_con_library q = 
  debug (fun () -> Util.format "lookup_con_library [%s]@\n" (Bsyntax.string_of_qid q));
  lookup_con_library_ctx [] q
