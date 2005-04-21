(** O'Caml bindings for html tidy. *)

type t

type status = Success | Warning
exception Error
exception Severe_error

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


(** Convenience function to tidy up a string without having to worry about
    tidy documents, etc.
    @param tidy_option list of options to use in tidy
    @param string to tidy
    @raise Error when an error occurs
    @raise Severe_error when a severe error occurs *)
val fix_string : tidy_option list -> string -> (string * status)

(** Creates a new tidy document *)
val create : unit -> t

(** Releases a tidy document *)
val release : t -> unit

(** Gets a string representing the release date of your current version
    of html tidy. *)
val get_release_date : unit -> string

(** Tries to parse the given string into the given document.
    @raise Error when an error occurs
    @raise Severe_error when a severe error occurs *)
val parse_string : t -> string -> status

(** Tries to clean and repair the given tidy document (must be parsed first).
    @raise Error when an error occurs
    @raise Severe_error when a severe error occurs *)
val clean_and_repair : t -> status

(** Tries to save the given tidy document to the file identified by the
    given string.
    @raise Error when an error occurs
    @raise Severe_error when a severe error occurs *)
val save_file : t -> string -> status

(** Returns the contents of the given tidy document as a string.
    {b WARNING}: This is experimental! *)
val save_string : t -> string

(** Sets the given option within tidy.  *)
val set_option : t -> tidy_option -> unit
