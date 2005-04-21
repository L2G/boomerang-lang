(* $Id: lockfile.ml,v 1.1 2004/09/15 14:54:58 schmitta Exp $ *)
(* module for locking files. for systems that implement Unix.lockf, we can *)
(* just use that, otherwise we use roll-your-own lock files. *)

type lock = Unixlock of Unix.file_descr
  | Otherlock of string

let using_lockf = 
  match Sys.os_type with
    "MacOS" | "Unix" | "Cygwin" -> true
  | _ -> false

(* locking a file consists of getting an open file descriptor for the file *)
(* and then holding on to it. Note that closing the file descriptor *)
(* implicitly unlocks the file. We return the file descriptor we used as an *)
(* opaque lock type so that we can close it when we unlock it. *)
let unix_try_lock_file fname =
  try
    let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
    try
      Unix.lockf fd Unix.F_TLOCK 0;
      Some(Unixlock fd)			(* got lock! *)
    with Unix.Unix_error _ -> Unix.close fd; None
  with Unix.Unix_error _ -> None

let unix_unlock_file = function
    Unixlock fd ->
      Unix.lockf fd Unix.F_ULOCK 0;
      Unix.close fd
  | _ -> assert false

(****** IMPLEMENTATION WITH LOCK FILES ******)

(* otherwise, we will have to try to roll our own. for this, we need a
   canonical name for the lock file, and attempt to create it to lock the
   file. make sure to register signal handlers to clear out any lock files 
   we have created if the program aborts *)

let allsigs =
  [Sys.sigabrt; Sys.sigalrm; Sys.sigfpe; Sys.sighup; Sys.sigill;
   Sys.sigint; (* Sys.sigkill; *) Sys.sigpipe; Sys.sigquit; Sys.sigsegv;
   Sys.sigterm; Sys.sigusr1; Sys.sigusr2; Sys.sigchld; Sys.sigcont;
   (* Sys.sigstop; *) Sys.sigtstp; Sys.sigttin; Sys.sigttou; Sys.sigvtalrm;
   Sys.sigprof]

let keep_sigs = ref []

let clean_on_sig fname signum =
  let old_sig_behavior = Sys.signal signum Sys.Signal_default in
  let new_handler = 
    (fun i ->
      try Sys.remove fname with _ -> ();
      match old_sig_behavior with
	Sys.Signal_default -> exit(-1)
      | Sys.Signal_ignore -> ()
      | Sys.Signal_handle oldh -> oldh i) in
  Sys.set_signal signum (Sys.Signal_handle new_handler)

let clean_on_abort fname =
  (* would normally use Safelist.iter here, but will use Safelist.iter for
     portability *)
  let sigs_to_clean_on = Safelist.filter
      (fun s -> not (Safelist.mem s !keep_sigs)) allsigs in
  Safelist.iter (clean_on_sig fname) sigs_to_clean_on

let make_enum l = 
  Safelist.rev
    (snd (Safelist.fold_left
	    (fun (i,acc) k -> (i+1,(i,k)::acc))
	    (0,[]) l))

let wday_assoc = make_enum ["Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"]
let month_assoc = make_enum ["Jan";"Feb";"Mar";"Apr";"May";"Jun";
			     "Jul";"Aug";"Sep";"Oct";"Nov";"Dec"]
let sprint_tm t =
  Printf.sprintf "%s %s %2d %d %02d:%02d:%02d"
    (Safelist.assoc t.Unix.tm_wday wday_assoc) 
    (Safelist.assoc t.Unix.tm_mon month_assoc)
    t.Unix.tm_mday (t.Unix.tm_year + 1900) 
    t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

let tempdir = ref "/tmp"

let lock_name fname =
  if not (Sys.file_exists fname) then
    Unix.close (Unix.openfile fname [Unix.O_WRONLY; Unix.O_CREAT] 0o600);
  (* according to Ocaml docs, everyone implements Unix.stat *)
  let inode_num = (Unix.stat fname).Unix.st_ino in
  !tempdir ^ "/" ^ (string_of_int inode_num) ^ ".lk"

let other_try_lock_file fname =
  let lockfile = lock_name fname in
  if Sys.file_exists lockfile then 
    (* should check for stale lock files. a lock file is definitely
       stale if either: is older than the system uptime (system crashed
       since then), or the pid contained in it is no longer running.
       does not appear to be a portable way to write this for Win32, as the
       Unix module does not provide a way to either (1) check the system
       uptime, or (2) check whether a process is alive (well, Unix.kill will
       let you do this, but it is not implemented for Windows). *)
    None 
  else
    try
      clean_on_abort lockfile;
      (* note: O_EXCL is broken on NFS file systems, but the workaround
	 requires Unix.link, which we don't have at this point (not
	 implemented for Windows). The user can work around this by ensuring
	 that the environment variable TEMP points to a directory on the
	 local file system. *) 
      let fd = Unix.openfile lockfile 
	  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
      let whostr = (string_of_int (Unix.getpid ())) ^ "\n" ^
	(sprint_tm (Unix.localtime (Unix.time ()))) ^ "\n" in
      let _ = Unix.write fd whostr 0 (String.length whostr) in
      Unix.close fd;
      Some (Otherlock fname)		(* got lock! *)
    with Unix.Unix_error _ -> None

let other_unlock_file = function
    Otherlock fname -> 
      (try Sys.remove (lock_name fname) with _ -> ())
  | _ -> assert false

(****** EXPORTED INTERFACE ******)

let nfs_warning () = 
  let ep = Printf.eprintf in
  ep "****** WARNING!! ******\n";
  ep "Because we don't have a working lockf() for your system, we will be\n";
  ep "implementing file locking by creating lock files in the following\n";
  ep "directory:\n\t%s\n" !tempdir;
  ep "If this directory is on an NFS file system, it WILL NOT WORK.\n";
  ep "To work around this problem, please set the environment variable\n";
  ep "TEMP to a directory on a local filesystem.\n";
  flush stdout

let sig_warning () =
  let ep = Printf.eprintf in
  ep "****** WARNING!! ******\n";
  ep "If this process is killed by a non-catchable signal (SIGKILL or\n";
  ep "SIGSTOP), or the system crashes, it may leave stale lock files in the\n";
  ep "following directory:\n"; 
  ep "\t%s\n" !tempdir;
  ep "Lock files will have an .lk file extension and will contain the\n";
  ep "PID of the process that created them and the time at which they were\n";
  ep "created. Please remove any stale locks by hand.\n";
  flush stdout

let keep_locks_thru_signals ss = 
  (* don't have to do anything special if we are using lockf() *)
  if not using_lockf then keep_sigs := ss

let is_dir fname =
  (Unix.stat fname).Unix.st_kind = Unix.S_DIR

let init () =
  if not using_lockf then begin
    (try tempdir := Sys.getenv "TEMP"
    with Not_found ->
      if (Sys.file_exists !tempdir) && (is_dir !tempdir) then () 
      else tempdir := ".");
    nfs_warning ();
    sig_warning ()
  end

let try_lock_file fname =
  if using_lockf then unix_try_lock_file fname
  else other_try_lock_file fname

exception Timeout

let must_lock_file fname period num_tries =
  let rec loop = function
      0 -> raise Timeout
    | n ->
	match try_lock_file fname with
	  Some(l) -> l
	| None -> (Unix.sleep period; loop (n - 1)) in
  loop num_tries

let unlock_file the_lock =
  if using_lockf then unix_unlock_file the_lock
  else other_unlock_file the_lock
