type type_desc = string list
type encoding_key = string
type filename = string
type contents = string
type encoding_test = filename -> contents option -> bool

type content_desc =
    FromString of string
  | FromFile of string

type encoding = {
  description: string;               (** "long" description *)
  encoding_test: encoding_test;      (** id function *)
  reader: content_desc -> V.t;       (** reads data in the given encoding *)
  writer: V.t -> string -> unit;     (** writes data to the specified file *)
}

let simple_reader f c =
  match c with
    FromString s -> V.Tree (f s)
  | FromFile s -> V.Tree (f (Misc.read s))

let simple_writer f v filename = Misc.write filename (f (V.tree_of (Info.M "simple_writer") v))

(* An encoding-keyed map. *)
module EncodingKey : Map.OrderedType with type t = encoding_key =
  struct
    type t = encoding_key
    let compare = Pervasives.compare
  end
module EncodingMap = Map.Make (EncodingKey)

(* An (encoding, type_desc list)-keyed map. *)
module EVTKey : Map.OrderedType with type t = (encoding_key * type_desc ) =
  struct
    type t = encoding_key * type_desc
    let compare = Pervasives.compare
  end
module EVTMap = Map.Make (EVTKey)

let emap = ref EncodingMap.empty

let register_encoding ekey erec = emap := EncodingMap.add ekey erec !emap

let get_encoding ekey = EncodingMap.find ekey !emap
let find_encodings fopt copt =
  EncodingMap.fold (fun ekey e acc ->
                      if e.encoding_test fopt copt then
                        ekey :: acc
                      else acc)
                   !emap
                   []
let get_all_encodings () =
  EncodingMap.fold (fun ekey _ acc -> ekey :: acc) !emap []
let get_reader ekey = (get_encoding ekey).reader
let get_writer ekey = (get_encoding ekey).writer
let get_description ekey = (get_encoding ekey).description
let print_description ekey = print_endline ((get_description ekey) ^ " (" ^ ekey ^ ")")
(*let string_of_encoding_key (ekey:encoding_key) = ekey*)

(* utilities *)
let parse_filename fn =
  try 
    let i = String.rindex fn ':' in
      (String.sub fn 0 i, Some(String.sub fn (i+1) ((String.length fn) - (i+1))))
  with
      Not_found -> (fn,None)

let get_ekey eko fn contents_opt = 
  match eko with 
      Some ekey ->
        begin 
          try 
            let _ = get_encoding ekey in ekey 
          with
            Not_found ->
              raise (Error.Harmony_error (fun () ->
                Format.printf "unknown encoding key: %s\nKnown keys: %s" ekey (String.concat " " (get_all_encodings()))))
        end
    | None -> 
        match (find_encodings fn contents_opt) with
          | [ek] -> ek
          | []    -> raise (Error.Harmony_error (fun () -> Format.printf "No encoding for file '%s'" fn))
          | eks    -> raise (Error.Harmony_error (fun () -> Format.printf "More than one possible encoding for file '%s'" fn ))

let tree_of_file fn reader = 
  if Sys.file_exists fn then
    Some (Util.convertUnixErrorsToFatal "Harmony" ( fun () -> reader (FromFile fn)))
  else
    None
