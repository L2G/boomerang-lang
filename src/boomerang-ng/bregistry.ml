(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* registry.ml - library of Boomerang values                        *)
(********************************************************************)
(* $Id $ *)

let sprintf = Printf.sprintf
let debug = Trace.debug "registry"
let verbose = Trace.debug "registry+"

(* --------------- Registry values -------------- *)
type rv = Bsyntax.sort * Bvalue.t

(* utility functions *)
let make_rv s v = (s,v)
let value_of_rv (s,v) = v
let sort_of_rv (s,v) = s
let format_rv rv = 
  let (s,v) = rv in    
    Util.format "@[";
    Bvalue.format v;
    Util.format ": %s" (Bsyntax.string_of_sort s);
    Util.format "]"

(* --------------- Focal library -------------- *)

(* state *)
module type REnvSig = 
sig
  type t
  val empty : unit -> t
  val lookup : t -> Bsyntax.qid -> rv option
  val update : t -> Bsyntax.qid -> rv -> t
  val overwrite : t -> Bsyntax.qid -> rv -> unit
  val iter : (Bsyntax.qid -> rv -> unit) -> t -> unit
end

module REnv : REnvSig = 
struct
  module M = Env.Make(struct
                        type t = Bsyntax.qid
                        let compare = Bsyntax.qid_compare
                        let to_string = Bsyntax.string_of_qid
                      end) 
  type t = (rv ref) M.t
  let empty = M.empty
  let lookup e q = match M.lookup e q with 
      Some r -> Some (!r)
    | None -> None
  let update e q v = M.update e q (ref v)
  let overwrite e q v = 
    match M.lookup e q with 
        Some r -> r:=v
      | None -> raise (Error.Harmony_error
                         (fun () -> Util.format "Registry.overwrite: %s not found" 
                           (Bsyntax.string_of_qid q)))
  let iter f e = M.iter (fun q rvr -> f q (!rvr)) e 
end

let loaded = ref []

let library : REnv.t ref = ref (REnv.empty ())

(* constants *)
let pre_ctx = List.map Bvalue.parse_qid ["Prelude"]

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
let register q r = library := (REnv.update (!library) q r)

(* register a native value *)
let register_native qs s v = 
  let q = Bvalue.parse_qid qs in
    register q (s,v)

(* register a whole (rv Env.t) in m *)
let register_env ev m = REnv.iter (fun q r -> register (Bsyntax.qid_dot m q) r) ev

(* --------------- Lookup functions -------------- *)

let paths = Prefs.createStringList 
  "include" 
  "search path for .boom sources"
  "Boomerang modules are loaded, compiled, and registered on-demand. The search path specifies where the run-time system should search for module sources (the current directory and the paths specified in the BOOMPATH environment variable are also searched)."
let _ = Prefs.alias paths "I"

let boompath =
  try Util.splitIntoWords (Unix.getenv "BOOMPATH") ':'
  with Not_found -> []

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
              verbose (fun() -> Util.format "%s found for %s@\n" fn basename);
              Some fn
            end else begin
              verbose (fun() -> Util.format "%s not found@\n" fn);
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
  let uncapped = String.uncapitalize ns in
    if (Safelist.mem ns (!loaded)) then true
    else begin
      match find_filename uncapped ["boom"] with 
        | None -> 
            begin
              try 
                (* check for baked in source *)
                let str = Hashtbl.find Bakery.items uncapped in 
                  go (fun () -> 
                        (!compile_boom_str_impl) str ns) 
                    (sprintf "<baked source for %s>" ns);
                  true
              with Not_found -> false
            end
        | Some fn ->
            go (fun () -> (!compile_file_impl) fn ns) fn; 
            true
    end

let load_var q = match get_module_prefix q with 
  | None -> ()
  | Some n -> ignore (load (Bsyntax.string_of_id n))

(* lookup in a naming context *)
let lookup_library_ctx nctx q = 
  let rec lookup_library_aux nctx q2 =       

    (* Util.format "lookup_library_aux %s in [%s] from %s "
      (Bsyntax.string_of_qid q2)
      (Misc.concat_list ", " (Safelist.map Bsyntax.string_of_qid nctx))
      (Env.to_string !library string_of_rv);
    *)
    (* let _ = debug (fun () -> Util.format "lookup_library_aux %s in [%s] from %s "
       (Bsyntax.string_of_qid q2)
       (Misc.concat_list ", " (Safelist.map Bsyntax.string_of_qid nctx))
       (Env.to_string !library string_of_rv)
       )
       in *)
    let try_lib () = REnv.lookup !library q2 in
      (* try here first, to avoid looping on native values *)
      match try_lib () with
          Some r -> Some r
        | None -> 
            begin
              match load_var q2; try_lib () with
                | Some r -> Some r
                | None -> match nctx with 
                    | []       -> None
                    | o::orest -> lookup_library_aux orest (Bsyntax.qid_dot o q) 
            end
  in
    lookup_library_aux nctx q

let lookup_library q = lookup_library_ctx [] q
