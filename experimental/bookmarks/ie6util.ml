(* A wrapper for IE6 bookmarks' synchronization *)
open Unix

type ('a,'b) directory = Folder of string * 'b * ('a,'b) directory list | File of string * 'a
    
type 'a url = Url of 'a | NotUrl of string
    
type ie6link = Default of (string * string) list | InetShortcut of (string * string) list | NoDefault
type ie6 = ie6link * ie6link
      
type bookmark = (ie6 url, bool) directory
      
let favdir = "/mnt/win_e/Documents and Settings/Stéphane/Favoris/"
let moveddir = "/home/steak/devel/harmony4/experimental/bookmarks/IE/rep"
    
let try_finalize f x e y =
  try let r = f x in e y; r
  with exn -> e y; raise exn
      
let whack_chars s =
  let s =
    (* escape only the strictly necessary *)
    List.fold_right
      ( fun (p,r) s -> Str.global_replace (Str.regexp p ) r s )
      [("\\","\\\\\\\\");("\"","\\\"");("\'","\\\'")]
      s
  in
  let s =
    if List.exists (fun c -> String.contains s c) ([' ';'"';'\t';'\n';'\\'; '{'; '}'; '['; ']'; '='; ':'; ','])  then "\""^s^"\""
    else s in
  if s="" then "\"\""
  else s
      
(* let rec bookmark_to_meta  : (bookmark -> string) =  *)
(*   let rec list_to_meta f = function *)
(*       [] -> "" *)
(*     | a::[] -> Printf.sprintf "%s" (f a) *)
(*     | a::q -> Printf.sprintf "%s,\n %s" (f a) (list_to_meta f q) *)
(*   in *)
(*   let by2 = function *)
(*       (n, v) -> Printf.sprintf "%s = %s" (whack_chars n) (whack_chars v) *)
(*   in *)
(*   let prints_link = function *)
(*       NoDefault, InetShortcut l ->  *)
(* 	Printf.sprintf "{ InetShortcut = {%s} }" (list_to_meta by2 l) *)
(*     | Default l1, InetShortcut l2 ->  *)
(* 	Printf.sprintf "{ \"Default\" = {%s}, \"InetShortcut\" = {%s} }" *)
(* 	  (list_to_meta by2 l1) *)
(* 	  (list_to_meta by2 l2) *)
(*     |  _ -> assert false *)
(*   in *)
(*   function *)
(*       Folder (name, b, l) ->  *)
(* 	if b then  *)
(* 	  Printf.sprintf "[ %s ]" (list_to_meta bookmark_to_meta l) *)
(* 	else *)
(* 	  Printf.sprintf  *)
(* 	    "{ \"folder\" = { \"name\"=%s, \"contents\"= [ %s ]}}\n" (whack_chars name) (list_to_meta bookmark_to_meta l) *)
(*     | File (name, c) -> *)
(* 	Printf.sprintf "{\"link\" = %s}" *)
(* 	  (match c with NotUrl s -> Printf.sprintf "{ \"noturl\" = %s }" (whack_chars s) *)
(* 	  |	Url s ->Printf.sprintf "{ \"name\" = %s,\n \"url\" = %s }" (whack_chars name) (prints_link s)) *)

let rec bookmark_to_desc : (bookmark -> V.desc) =
  function
      Folder (name, b, l) ->
	if b then
	  V.L (List.map bookmark_to_desc l)
	else
	  let ldesc = List.map bookmark_to_desc l in
	  V.V [("folder", V.V [("name", V.Val(name));("contents",V.L(ldesc))])]
    | File (name, c) ->
	let link = 
	  match c with 
	    NotUrl s ->
	      V.V ([("name",V.Val(name));("noturl",V.Val (whack_chars s))])
	  | Url s -> 
	      let desc_of_url = function
		  NoDefault, InetShortcut l ->
		    V.V [("InetShortcut", V.V (List.map (fun (n,s) -> (n, V.Val (whack_chars s))) l))]
		| Default l1, InetShortcut l2 ->
		    V.V [("InetShortcut", V.V (List.map (fun (n,s) -> (n, V.Val (whack_chars s))) l2));
			  ("Default", V.V (List.map (fun (n,s) -> (n, V.Val (whack_chars s))) l1))]
		| _ -> assert false
	      in
	      V.V [("name", V.Val(name));("url", desc_of_url s)]
	in
	V.V [("link",link)]

let bookmark_to_view : (bookmark -> V.t) =
  fun bkmark -> V.from_desc ( bookmark_to_desc bkmark )
      
let rec bookmark_from_view : (V.t -> bookmark list) = function t ->
  let linklists = V.list_from_structure t in
  List.map 
    (fun t -> match V.singleton_dom t with
      "folder" -> 
	let folder = V.get_required t "folder" in
	let name = V.get_required folder "name" in
	let contents = V.get_required folder "contents" in
	Folder(V.get_value name, false, bookmark_from_view contents)
    | "link" ->
	let url_from_view t =
	  let by2 t =
	    V.fold (fun name v acc -> (name, V.get_value v)::acc) t []
	  in
	  match (V.get t "Default", V.get_required t "InetShortcut") with
	    None, inet -> (NoDefault, InetShortcut (by2 inet))
	  | Some d, inet -> (Default (by2 d), InetShortcut (by2 inet))
	in
	let link = V.get_required t "link" in
	let name = V.get_required link "name" in
	let url =
	  match V.get link "url" with
	    Some urlt -> Url (url_from_view urlt)
	  | None -> NotUrl (V.get_value (V.get_required link "noturl"))	in
	File (V.get_value name, url)
    | _ -> assert false
	  )
    linklists
    
let rec really_read fd buffer start length =
  if length <= 0 then () else
  match read fd buffer start length with
    0 -> raise End_of_file
  | r -> really_read fd buffer (start + r) (length - r);;

let contents file =
  let fd = openfile file [O_RDONLY] 0o777 in
  let size = lseek fd 0 SEEK_END in
  let _ = lseek fd 0 SEEK_SET in
  let buf = String.create size in
  really_read fd buf 0 size;
  buf;;

let rec throughDirectory dirname f g = 
  let dirh = opendir dirname in
  let rec aux dirh acc =
  	try ( let newacc =
		match readdir dirh with
		 "." | ".." -> acc
		| file -> let path = Filename.concat dirname file in
			  let st = stat path in
			  match st.st_kind with
				S_REG -> File(file, f path)::acc
				| S_DIR -> let l = throughDirectory path f g in
					   Folder(file, g path, l)::acc
				| _ -> (* Maybe print some warnings or errors *) acc
	      in aux dirh newacc
		)
	with End_of_file -> acc	in
  let do_it () = 
    let liste = aux dirh [] in
    liste in
  try_finalize do_it () closedir dirh;;

let removeDirectory =
  fun dirname -> ignore ( throughDirectory dirname unlink rmdir );;

let splitLines = Str.split (Str.regexp "[\013\n]+")

let parseLines s =
  try (
    let assoc s = 
      (* A not_found exception would be caught after the List.map *)
      let r = String.index s '=' in
      ( String.sub s 0 r, String.sub s (r+1) (String.length s - r - 1))
    in
    List.map assoc (List.rev (splitLines s))
      )
  with _ -> prerr_endline "Invalid bookmark line"; []
    
let scanContents s =
  let p2 s1 s2 =  (Default (parseLines s1)), (InetShortcut (parseLines s2)) in
  let p1 s2 = NoDefault, InetShortcut (parseLines s2) in
  try ( Scanf.sscanf s " [DEFAULT] %s [InternetShortcut] %s@\000" (fun x y -> Url(p2 x y)) )
  with
    _ -> ( try (Scanf.sscanf s " [InternetShortcut] %s@\000" (fun x -> Url (p1 x)) )
    with
      Scanf.Scan_failure _ -> prerr_endline "Invalid bookmark file contents : link stored as non-url"; NotUrl(s)
    | End_of_file -> prerr_endline "Unexpected end of file : link stored as non-url"; NotUrl(s)
	  )
	
let readBookmarkFolder : (string -> bookmark) =
  fun dirname -> Folder( dirname, true,  throughDirectory dirname (fun n -> scanContents (contents n)) (fun _ -> false));;

(* let myBookmarks = readBookmarkFolder favdir;; *)

(* let myBookmarksView = bookmark_to_meta myBookmarks;; *)
      
let rec printLines tag = function
    (a,b) :: q -> (printLines tag q)^(Printf.sprintf "%s=%s\013\n" a b)
  | [] -> Printf.sprintf "[%s]\013\n" tag
	
let printContents = function
    Url (NoDefault, InetShortcut l)  -> printLines "InternetShortcut" l
  | Url (Default ld, InetShortcut li) -> Printf.sprintf "%s\013\n%s" (printLines "DEFAULT" ld) (printLines "InternetShortcut" li)
  | NotUrl s -> s
  | _ -> assert false

let rec writeBookmarkFolder : ( bookmark -> unit ) = function
    Folder( dirname, _, dirlist ) ->
      let curDir = Sys.getcwd () in
      begin
	if not(Sys.file_exists dirname) then mkdir dirname 0o777;
	Sys.chdir dirname;
	List.iter writeBookmarkFolder dirlist;
	Sys.chdir curDir
      end
  | File( filename, cont ) ->
      let fd = openfile filename [O_WRONLY; O_CREAT] 0o777 in
      let str = printContents cont in
      (ignore( write fd str 0 (String.length str));
       ftruncate fd (String.length str))
	;;
  
(* let movedBookmarks =  *)
(*   match myBookmarks with *)
(*     Folder(_, b, dlist) -> *)
(*       Folder(moveddir, b, dlist) *)
(*   | File _ -> assert false *)
(* 	;; *)

let write_bookmark bl newdir =
  (* this is not safe at all !!! *)
  (* I should clean the directory AFTER the writeBookmarkFolder *)
  (* moreover everything gets destroyed in this new directory ! *)
  if Sys.file_exists newdir then removeDirectory newdir;
  writeBookmarkFolder (Folder(newdir, true, bl))
    
let read_bookmark file =
  bookmark_to_view (readBookmarkFolder file)
    
let dump_to_meta t f =
  let outc = open_out f in
  (output_string outc (Meta.writer t); close_out outc)
    
let read_from_meta f =
  Meta.reader (contents f)

let usage = "ie6util get <directory> <dest>\nie6util put <src> <directory>"

let _ = match Array.length (Sys.argv) with
  4 -> 
    begin
      match Sys.argv.(1) with
	"get" -> 
	  let dir = Sys.argv.(2) and dest = Sys.argv.(3) in
	  (dump_to_meta (read_bookmark dir) dest;
	   Printf.printf "[ie6util] Bookmark folder %s successfully dumped into %s\n" dir dest)
      |	"put" ->
	  let src = Sys.argv.(2) and newdir = Sys.argv.(3) in
	  (write_bookmark (bookmark_from_view (read_from_meta src)) newdir;
	   Printf. printf "[ie6util] Bookmark folder %s successfully created from %s\n" newdir src)
      |	_ -> prerr_endline usage
    end
| _ -> prerr_endline usage
