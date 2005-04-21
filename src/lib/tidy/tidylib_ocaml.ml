type tidy_doc

type t = tidy_doc

type tidy_option =
  | WrapLen of int (** Wrap margin *)
  | ShowWarnings of bool (** however errors are always shown *)
  | XmlOut of bool (** create output as XML *)
  | XhtmlOut of bool (** output extensible HTML *)
  | HtmlOut of bool (** output plain HTML, even for XHTML input.  Yes means set
                        explicitly *)
  | Quiet of bool (** no 'Parsing X', guessed DTD or summary *)
  | NumEntities of bool (** Use numeric entities *)
  | Mark of bool (** Add meta element indicating tidied doc *)

type status = Success | Warning

exception Error
exception Severe_error

(* allocates up a tidy_doc *)
external tidyCreate : unit -> tidy_doc = "tidy_tidyCreate"
(* frees memory used by the tidy_doc *)
external tidyRelease : tidy_doc -> unit = "tidy_tidyRelease"
(* gets the release date of the tidy yer using :P *)
external tidyReleaseDate : unit -> string = "tidy_tidyReleaseDate"
(* parses a string into a tidy_doc allocated with tidyCreate *)
external tidyParseString : tidy_doc -> string -> int = "tidy_tidyParseString"
(* runs "clean and repair" on a tidy doc *)
external tidyCleanAndRepair : tidy_doc -> int = "tidy_tidyCleanAndRepair"
(* saves a tidy doc to a file *)
external tidySaveFile : tidy_doc -> string -> int = "tidy_tidySaveFile"
(* saves a tidy doc to a string.  This USES the tidy procedure
   "tidySaveString", but it also has to do some magic; it is NOT merely
   a wrapper like most of the others. *)
external tidySaveString : tidy_doc -> string = "tidy_tidySaveString"
external tidyRunDiagnostics : tidy_doc -> int = "tidy_tidyRunDiagnostics"
external tidySetCharEncoding : tidy_doc -> string = "tidy_tidySetCharEncoding"
external tidyDetectedHtmlVersion : tidy_doc -> int =
  "tidy_tidyDetectedHtmlVersion"
external tidyDetectedXhtml : tidy_doc -> bool = "tidy_tidyDetectedXhtml"
external tidyDetectedGenericXml : tidy_doc -> bool =
  "tidy_tidyDetectedGenericXml"
external tidySetShowWarnings : bool -> tidy_doc -> bool =
  "tidy_tidySetShowWarnings"
external tidySetXmlOut : bool -> tidy_doc -> bool = "tidy_tidySetXmlOut"
external tidySetXhtmlOut : bool -> tidy_doc -> bool = "tidy_tidySetXhtmlOut"
external tidySetHtmlOut : bool -> tidy_doc -> bool = "tidy_tidySetHtmlOut"
external tidySetQuiet : bool -> tidy_doc -> bool = "tidy_tidySetQuiet"
external tidySetMark : bool -> tidy_doc -> bool = "tidy_tidySetMark"
external tidySetNumEntities : bool -> tidy_doc -> bool =
  "tidy_tidySetNumEntities"
external tidySetWrapLen : int -> tidy_doc -> bool = "tidy_tidySetWrapLen"


(* translate_errors : int -> status *)
let translate_errors = function
    0 -> Success
  | 1 -> Warning
  | 2 -> raise Error
  | _ -> raise Severe_error

let create = tidyCreate
let release = tidyRelease
let get_release_date = tidyReleaseDate
let parse_string doc s = translate_errors (tidyParseString doc s)
let clean_and_repair doc = translate_errors (tidyCleanAndRepair doc)
let run_diagnostics doc = translate_errors (tidyRunDiagnostics doc)
let save_file doc file = translate_errors (tidySaveFile doc file)
let save_string = tidySaveString

(* For some reason, the tidy api is returning booleans from the setOption
   functions.  This function returns these booleans, while the normal
   set_option does not. *)
let set_option_bool = function
  | WrapLen i -> tidySetWrapLen i
  | ShowWarnings b -> tidySetShowWarnings b
  | XmlOut b -> tidySetXmlOut b
  | XhtmlOut b -> tidySetXhtmlOut b
  | HtmlOut b -> tidySetHtmlOut b
  | Quiet b -> tidySetQuiet b
  | NumEntities b -> tidySetNumEntities b
  | Mark b -> tidySetMark b

let set_option td opt = let _ = set_option_bool opt td in ()

let fix_string options s =
  let td = create () in
  List.iter (set_option td) options;
  let s1 = parse_string td s in
  let s2 = clean_and_repair td in
  let s3 = run_diagnostics td in
  let status = match s1,s2,s3 with
    Success, Success, Success -> Success | _ -> Warning
  in
  (save_string td, status)
