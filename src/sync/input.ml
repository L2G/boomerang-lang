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
           * replica option     (** archive *)
           * type_desc option)  (** sync_vt *)
  | Show of replica * type_desc
  | Updown of replica * type_desc
  | Upcreate of replica * type_desc
  | Identify of replica
  | Capabilities
  | Test

(* scp_replica : string -> string option * string -> string *)
let scp_replica = function
    Path (path, Local) -> assert ("Owen" = "stupid"); exit 1
  | Path (path, Remote (Some user, host)) -> user ^ "@" ^ host ^ ":" ^ path
  | Path (path, Remote (None, host)) -> host ^ ":" ^ path

(* do_scp : string -> string -> unit *)
let do_scp from_here to_there =
  match Unix.system ("scp " ^ from_here ^ " " ^ to_there) with
    Unix.WEXITED 0 -> ()
  | Unix.WEXITED 1 -> () (* this generally means that one of the replicas
                               was not found. *)
  | Unix.WSTOPPED _
  | Unix.WSIGNALED _
  | _ ->  (print_endline "unexpected scp failure, harmony is exiting.."; exit 1)

let load_path = function
    Path (path, Local) -> path
  | Path (path, Remote _) as r ->
      let localpath = Config.temp_dir ^ "/" ^ Filename.basename path in
      do_scp (scp_replica r) localpath; localpath

let save_path lpath p =
  (*print_endline ("saving path " ^ lpath);*)
  match p with
    Path (path, Local) -> ()
  | r ->
      try
        let c = open_in lpath in
        close_in c;
        do_scp lpath (scp_replica r)
      with Sys_error s ->
        print_endline ("--> local replica has been removed, please remove "
                     ^ "remote replica manually\n(or maybe you didn't use "
                     ^ "the -replace option?).")

let get_af p1 p2 =
  let path_to_s = function
      Path (path, Local) -> path
    | Path (path, Remote (Some user, host)) -> user ^ "#" ^ host ^ "#" ^ path
    | Path (path, Remote (None, host)) -> host ^ "#" ^ path
  in
  let sorted = Safelist.sort Pervasives.compare [path_to_s p1; path_to_s p2] in
  Digest.to_hex (Digest.string (String.concat "#" sorted)) ^ ".harmony"
