type type_desc = string list
type encoding_key = string
type filename = string
type contents = string
type encoding_test = filename -> contents option -> bool
type encoding = {
  description: string;               (** "long" description *)
  encoding_test: encoding_test;      (** id function *)
  reader: V.reader;                  (** reads data in the given encoding *)
  writer: V.writer;                  (** writes data to the given encoding *)
  from_string: string -> V.t option; (** reads data in the given encoding *)
  to_string: V.t -> string;          (** writes data to the given encoding *)
  base_type: type_desc;              (** the most concrete view type of
                                         instances of this encoding.  the
                                         reader/writer pair translates between this
                                         view type and this encoding. *)
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
let get_from_string ekey = (get_encoding ekey).from_string
let get_to_string ekey = (get_encoding ekey).to_string
let get_description ekey = (get_encoding ekey).description
let get_base_type ekey = (get_encoding ekey).base_type
let print_description ekey = print_endline ((get_description ekey) ^ " (" ^ ekey ^ ")")
(*let string_of_encoding_key (ekey:encoding_key) = ekey*)
