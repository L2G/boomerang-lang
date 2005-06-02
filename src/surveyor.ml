type type_desc = string list
type encoding_key = string
type filename = string
type contents = string
type encoding_test = filename -> contents option -> bool
type encoding = {
  description: string;               (** "long" description *)
  encoding_test: encoding_test;      (** id function *)
  reader: string -> V.t;             (** reads data in the given encoding *)
  writer: V.t -> string;             (** writes data to the given encoding *)
}

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
	      Not_found -> raise (Error.Fatal_error (Printf.sprintf "unknown encoding key: %s" ekey))
	end
    | None -> 
	match (find_encodings fn contents_opt) with
	  | [ek] -> ek
	  | _    -> raise (Error.Fatal_error (Printf.sprintf "No unique encoding for file '%s'" fn))
