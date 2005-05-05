type type_desc = Optometrist.type_desc
type encoding_key = Surveyor.encoding_key
exception Syntax_error of string

type location =
    Local
  | Remote of (string option (** user *)
             * string) (** hostname *)

type path = Path of (string * location)

type replica = Replica of ( path * encoding_key option * type_desc option )

type t =
    Sync of (replica            (** left *)
           * replica            (** right *)
           * replica option        (** archive *)
           * type_desc option)  (** sync_vt *)
  | Show of replica * type_desc
  | Updown of replica * type_desc
  | Upcreate of replica * type_desc
  | Identify of replica
  | Capabilities
  | Test

val load_path : path -> string
(** [load_path path] returns a local path regardless of the locality of [path].
*)

val save_path : string -> path -> unit
(** [save_path lpath rpath] saves [lpath] to the possibly remote [rpath]. *)

val get_af : path -> path -> string
(** [get_af path1 path2] infers an archive name for [path1] and [path2]. *)
