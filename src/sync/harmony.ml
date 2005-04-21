open Config

let _ = Format.set_margin 80

(* filename_part : string -> string
   [filename_part s] returns the portion of [s] that refers to an actual
   filename
*)
let filename_part s =
  match Util.splitIntoWords s ':' with
    [name]
  | [_;name] -> name
  | _ -> assert false

(* spec_part : string -> string option
  [spec_part s] returns the portion of [s] that refers to the spec, or
  None if none exists. *)
let spec_part s =
  match Util.splitIntoWords s ':' with
    [_] -> None
  | [spec; _] -> Some spec
  | _ -> assert false

(* new_filename : string -> string *)
(* acts appropriately based on prefs about backing things up, replacing, etc. *)
(* we always do backups *)
let new_filename_name f = f ^ "." ^ (Prefs.read ext)

let really_new_filename f =
  if Prefs.read backup then Misc.backup f;
  new_filename_name f

let new_filename f =
  if Prefs.read backup then Misc.backup f;
  if Prefs.read replace then f else new_filename_name f

let mv_align_file_if_replace af newaf =
  if Prefs.read replace then Sys.rename newaf af

(* bldwht : string -> string
  bldwht returns s bolded and whitened. *)
let bldwht s = Misc.color s Misc.White ~bold:true

(* contents : string -> string option *)
(* [contents filename] tries to read [filename] into a string.  *)
let contents f =
  try Some (Misc.read f) with
    Sys_error e -> prerr_endline("warning: " ^ e); None
  | _ -> prerr_endline("warning: errors reading " ^ f); None

(* get_ekey : string -> encoding_key *)
(* [get_ekey filename key_option] tries to get the key
 * the surveyor and possibly resorting to the UI as necessary. *)
let get_ekey f = function
    Some ekey ->
      (try let _ = Surveyor.get_encoding ekey in ekey with
          Not_found -> prerr_endline ("unknown encoding key " ^ ekey); exit 1)
  | None -> Ui.choose_encoding f (Surveyor.find_encodings f (contents f))

(* get_base_type : encoding_key -> view_type option *)
(* [get_type_desc_list ekey td_option] gets the list of descriptoins of types 
    type for [ekey], optionally appending [td_option] to the type. *)
let get_base_type ekey = function
    None -> Surveyor.get_base_type ekey
  | Some td -> (Surveyor.get_base_type ekey) @ td

(* apply_opt : lens -> view option -> (view option * pushdown) *)
let apply_opt lens = function
    Some cview -> Some (Lens.get lens cview), (fun a -> Lens.put lens a (Some cview))
  | None -> (None, fun a -> Lens.put lens a None)

(* compose_lens_list : lens list -> lens *)
(* -- was removed from Lens *)
let rec compose_lens_list = 
  function [] -> Pervasives_plugin.id
    | (l::ls) -> Pervasives_plugin.compose2 l (compose_lens_list ls) 

(* pushdown_opt : pushdown -> view option -> view_option *)
let pushdown_opt pd = function
    Some aview -> Some (pd aview)
  | None -> None

(* METAVARIABLES:
    cv    -   concrete view or concrete view option
    av    -   abstract view or abstract view option
    ar    -   archive (concrete, abstract, or optional)
    td    -   type description
    pt    -   plugin type
    ft    -   feature tree type
    pd    -   pushdown
    pN    -   path
*)

(* do_upcreate : filename
          -> encoding_key option
          -> view_type option
          -> view_type
          -> unit *)
let do_upcreate f eko tdo as_td =
  (* get the encoding key, which is possibly specified as an override in eko *)
  let ekey = get_ekey f eko in
  (* print a message detailing what we identified this as *)
  print_string "Replica looks like "; Surveyor.print_description ekey;
  (* get the base type, possibly adding on some types specified in tdo *)
  let tdl = get_base_type ekey tdo in 
  (* get the list of everything that this type can be synchronized as *)
  let opt_result = Optometrist.can_sync_as1 tdl in
  (* apply the reader *)
  let cv = (Surveyor.get_reader ekey) f in
  (* verbose display *)
  if Prefs.read verbose then begin
    V.format_msg [`String (bldwht "\nConcrete view...");
                  `View_opt cv;]; Format.print_flush();
    end;
  (* get the lens *)
  let as_td = Ui.choose_vt (Some as_td) (fst (Safelist.split opt_result)) in
  let lenses,_ = Safelist.assoc as_td opt_result in
  let lens = compose_lens_list lenses in
  (* apply the lens *)
  let av, _ = apply_opt lens cv in
  (* display it *)
  if (Prefs.read verbose) then begin
      V.format_msg [`String (bldwht "\nAbstract view...");
                    `View_opt av;]; Format.print_flush(); end;
  (* apply the create *)
  let cv' = pushdown_opt (fun a -> Lens.put lens a None) av in
  (* display the new concrete view *)
  if Prefs.read verbose then begin
    V.format_msg [`String (bldwht "\nNew (created) concrete view..");
                  `View_opt cv';]; Format.print_flush();
    end;
  let av',_ = apply_opt lens cv' in
  if V.equal_opt av av' then
    print_endline
      ("Same as original abstract view: " ^ (Misc.color "OK!" Misc.Green))
  else begin
    print_endline ("Not same as original abstract view: "
                   ^ (Misc.color "Whoops!" Misc.Red));
    if not (Prefs.read verbose) then 
      V.format_msg [`String (bldwht "\nOld abstract view..."); `View_opt av;];
    V.format_msg [`String (bldwht "\nNew abstract view..."); `View_opt av';];
    Format.print_flush(); 
    exit 1;
  end

(* do_show : filename
          -> encoding_key option
          -> type_desc option
          -> type_desc
          -> boolean                  -> unit *)
(* do_updown is simply an instance of do_show *)
let do_show f eko tdo as_td backdown =
  (* get the encoding key, which is possibly specified as an override in eko *)
  let ekey = get_ekey f eko in
  (* print a message detailing what we identified this as *)
  print_string "Replica looks like ";
    Surveyor.print_description ekey;
    flush stdout;
  (* get the base type, possibly adding on some types specified in tdo *)
  let bt = get_base_type ekey tdo in
  (* get the list of everything that this type can be synchronized as *)
  let opt_result = Optometrist.can_sync_as1 bt in
  (* apply the reader *)
  let cv = (Surveyor.get_reader ekey) f in
  (* verbose display *)
  if Prefs.read verbose then begin
    V.format_msg [`String (bldwht "\nConcrete view...");
                  `View_opt cv;]; Format.print_flush();
    end;
  (* get the lens *)
    let as_td = Ui.choose_vt (Some as_td) (fst (Safelist.split opt_result)) in
    let lenses,_ = Safelist.assoc as_td opt_result in
    let lens = compose_lens_list lenses in
      (* apply the lens *)
    let av, pd = apply_opt lens cv in
      (* display it *)
      if (not backdown && not (Prefs.read terse)) or (Prefs.read verbose) then
	begin
	  V.format_msg [`String (bldwht "\nAbstract view...");
			`View_opt av;]; Format.print_flush();
	end;
      
      (* bring it back down if requested. *)
      if backdown then
	(* apply the pushdown *)
	let cv' = pushdown_opt pd av in
	  (* display the new concrete view *)
	  if Prefs.read verbose then begin
	    V.format_msg [`String (bldwht "\nNew concrete view..");
			  `View_opt cv';]; Format.print_flush();
	  end;
	  if V.equal_opt cv cv' then
	    print_endline
              ("Same as original concrete view: " ^ (Misc.color "OK!" Misc.Green))
	  else
	    (print_endline ("Not same as original concrete view: "
			    ^ (Misc.color "Whoops!" Misc.Red));
	     exit 1)
	      
let do_sync f1 eko1 tdlo1 f2 eko2 tdlo2 fa ekaf vtaf sugg_td =
  let ekey1 = get_ekey f1 eko1 in
  let ekey2 = get_ekey f2 eko2 in
  let ekeya = get_ekey fa ekaf in

    print_string "First replica looks like "; Surveyor.print_description ekey1;
    print_string "Second replica looks like "; Surveyor.print_description ekey2;
    print_string "Archive looks like "; Surveyor.print_description ekeya;
    flush stdout;

    let raw_arch = ekeya = "xmlarchive" in
      if not raw_arch then
	if (ekey1 <> ekeya) && (ekey2 <> ekeya) then begin
	  prerr_endline "archive is not raw and has concrete format different from both replica";
	  exit 1
	end;
      
      (* Use the optometrist (and the UI) to determine a common
	 abstract view type for our inputs. *)
      let tdll1 = get_base_type ekey1 tdlo1 in
      let tdll2 = get_base_type ekey2 tdlo2 in
      let opt_result = Optometrist.can_sync_as2 (tdll1, tdll2) in
      let sync_td = Ui.choose_vt sugg_td (fst (Safelist.split opt_result)) in 
      let cv1 = (Surveyor.get_reader ekey1) f1 in
      let cv2 = (Surveyor.get_reader ekey2) f2 in
      let cva = (Surveyor.get_reader ekeya) fa in
      let (lens1,ftt),(lens2,_) = Safelist.assoc sync_td opt_result in
      let lens1 = compose_lens_list lens1 in
      let lens2 = compose_lens_list lens2 in
      let lensa =
	if raw_arch then Pervasives_plugin.id else
	  if ekeya = ekey1 then lens1 else
	    if ekeya = ekey2 then lens2 else
	      assert false
      in

      (* lens application (get direction) *)
      let av1, pd1 = apply_opt lens1 cv1 in
      let av2, pd2 = apply_opt lens2 cv2 in
      let ava, pda = apply_opt lensa cva in
	(* verbose information *)
	if Prefs.read verbose then begin
	  V.format_msg [ `String (bldwht "\nAbstract view 1 ("^f1^")...");
			 `View_opt av1;
			 `String (bldwht "\nAbstract view 2 ("^f2^")...");
			 `View_opt av2;
			 `String (bldwht "\nArchive ("^fa^")...");
			 `View_opt ava];
          Format.print_flush ()
	end;

	(* big fork based on whether or not we're going to do alignment  *)
	
	(* unaligned_sync : View.t opt -> View.t opt -> View.t opt
           -> (View.t opt * View.t opt * View.t opt)
	   performs a synchronization with no alignment.
	*)
	let unaligned_sync ar v1 v2 =
	  let action,_,_,_ = Sync.sync ftt ar v1 v2 in
	  let action = Ui.massage_action action in
	    Lens.trap_errors_in
	      (fun () -> Sync.propagate ar v1 v2 action) ()
	in
	  (* aligned_sync : alignment file
             -> Align.keyfactory
             -> Align.keyfactory
             -> View.t opt (* archive *)
             -> View.t opt -> View.t opt (* concrete views *)
             -> View.t opt -> View.t opt (* abstract views *)
             -> (View.t opt * View.t opt * View.t opt)
	     performs a synchronization with alignment. *)
	let aligned_sync af kf1 kf2 ar cv1 cv2 v1 v2 =
	  let newaf = really_new_filename af in
	    (* alignment lenses *)
	  let al1 = Align.aligner_lens af newaf (kf1 cv1) Align.Left in
	  let al2 = Align.aligner_lens af newaf (kf2 cv2) Align.Right in
	  let alar = 
	    if raw_arch then 
              Align.aligner_lens af newaf (Align.default_kf ar) Align.Both
	    else failwith "aligning with concrete archive unsupported"
	  in

	  (* aligned views and "unalignment" pushdowns *)
	  let av_ar, pd_ar = apply_opt alar ar in
	  let av1, pd1 = apply_opt al1 v1 in
	  let av2, pd2 = apply_opt al2 v2 in
	    if Prefs.read paranoid && raw_arch then assert (V.equal_opt av_ar ar);(* sanity check *)
	    (* verbose information *)
	    if Prefs.read verbose then
	      begin
		V.format_msg [`String (bldwht "\nAligned abstract view 1 ("^f1^")...");
			      `View_opt av1;
			      `String (bldwht "\nAligned abstract view 2 ("^f2^")...");
			      `View_opt av2;
			      `String (bldwht "\nAligned archive ("^fa^")...");
			      `View_opt av_ar];
		Format.print_flush ()
	      end;
	    (* the actual synchronization is done by unaligned_sync. *)
	    if Prefs.read verbose then
	      begin
		V.format_msg [`String "\nStarting sync ..."];
		Format.print_flush ()
	      end;
	    let av_ar, av1, av2 = unaligned_sync av_ar av1 av2 in
	      (* verbose information, part II *)
	      if Prefs.read verbose then
		begin
		  V.format_msg [`String (bldwht "\nNew aligned abstract view 1 ("^f1^")...");
				`View_opt av1;
				`String (bldwht "\nNew aligned abstract view 2 ("^f2^")...");
				`View_opt av2;
				`String (bldwht "\nNew aligned archive ("^fa^")...");
				`View_opt av_ar];
		  Format.print_flush ()
		end;
	      (* XXX XXX XXX
		 We must do the "unaligning" pushdown for the archive *after* the
		 unaligning pushdowns for the replicas so we can pick up any new keys that
		 got generated. *)
	      if Prefs.read verbose then
		begin
		  V.format_msg [`String "\nStarting pushdown of aligner ..."];
		  Format.print_flush ()
		end;
	      let av2 = pushdown_opt pd2 av2 in
	      let av1 = pushdown_opt pd1 av1 in
	      let av_ar = pushdown_opt pd_ar av_ar in
		mv_align_file_if_replace af newaf;
		(av_ar, av1, av2)
	in

	let kf1 = Key_factory.get_keyfactory (ekey1, sync_td) in
	let kf2 = Key_factory.get_keyfactory (ekey2, sync_td) in
	let ava, av1, av2 =
	  match Prefs.read alignfile with
              None -> unaligned_sync ava av1 av2
	    | Some af -> aligned_sync af kf1 kf2 ava cv1 cv2 av1 av2
	in
	  (* verbose information *)
	  if Prefs.read verbose then
	    begin
	      V.format_msg [`String (bldwht "\nNew archive...");
			    `View_opt ava;
			    `String (bldwht "\nNew abstract view 1...");
			    `View_opt av1;
			    `String (bldwht "\nNew abstract view 2...");
			    `View_opt av2]; Format.print_flush ()
	    end;
	  (* pushdowns *)
	  let cv1 = pushdown_opt pd1 av1 in
	  let cv2 = pushdown_opt pd2 av2 in
	  let cva = pushdown_opt pda ava in
	    (* verbose information *)
	    if Prefs.read verbose then begin
	      V.format_msg [`String (bldwht "\nNew concrete view 1 ("^f1^")...");
			    `View_opt cv1;
			    `String (bldwht "\nNew concrete view 2 ("^f2^")...");
			    `View_opt cv2;
			    `String (bldwht "\nNew archive ("^fa^")...");
			    `View_opt cva;
			   ]; 
	      Format.print_flush ()
	    end;
	    (* writers *)
	    Surveyor.get_writer ekeya cva (new_filename (filename_part fa));
	    Surveyor.get_writer ekey1 cv1 (new_filename (filename_part f1));
	    Surveyor.get_writer ekey2 cv2 (new_filename (filename_part f2))

let anonymousArgs =
  Prefs.createStringList "rest"
    "*roots" ""

let main () =
  (* FIX: These seem to be out of date wrt. the actual choices... *)
  let usage_msg =
      "harmony [options] sync REPLICA1 REPLICA2 ARCHIVE as [view_type1; ...]\n"
    ^ "harmony [options] show REPLICA as [view_type1; ...]\n"
    ^ "harmony [options] updown REPLICA\n"
    ^ "harmony [options] upcreate REPLICA\n"
    ^ "harmony [options] capabilities\n"
    ^ "harmony [options] test\n"
(** these are not yet implemented! **)
(* 
    ^ "harmony [options] convert REPLICA1 REPLICA2\n"
    ^ "harmony [options] merge REPLICA1 REPLICA2 NEWREPLICA ARCHIVE\n"
*)
    ^ "harmony [options] identify REPLICA\n"
    ^ "Replicas and archives may be specified as local\n"
    ^ "  paths or as user@host:/path.  Encodings can be given immediately\n"
    ^ "  after a replica in parethensis (e.g. file.plist(safari)). \n"
    ^ "[options] include:"
  in
  Config.startup ();
  Prefs.parseCmdLine usage_msg;
  let rest = String.concat " " (Safelist.rev (Prefs.read anonymousArgs)) in
  let lexbuf = Lexing.from_string rest in
  let input =
    try
      Cmdlineparser.main Cmdlinelexer.token lexbuf
    with
      Input.Syntax_error e ->
        prerr_string (bldwht (Printf.sprintf "%s (char %d): %s\n" e (Lexing.lexeme_start lexbuf) rest));
        Prefs.printUsage usage_msg;
        exit 1
  in
  Lockfile.init ();
  match input with
    Input.Sync (rep1, rep2, a_opt, sync_vt_opt) ->
      (* we must save the original paths for save_path at the end of sync. *)
      let (original_p1, eko1, vto1) = match rep1 with Input.Replica r -> r in
      let (original_p2, eko2, vto2) = match rep2 with Input.Replica r -> r in
      let f1 = Input.load_path original_p1 in
      let f2 = Input.load_path original_p2 in
      let (original_af, ekaf, vtaf) =
        match a_opt with
        | Some (Input.Replica r) -> r
        | None -> (
          Input.Path ((Input.get_af original_p1 original_p2), Input.Local),
          Some "xmlarchive",
          None)
      in
      let af = Input.load_path original_af in
      if (af = f1) || (af = f2) then failwith "archive file is identical to one replica, this might cause data loss";
      if Prefs.read verbose then
        begin
          print_endline ("Trying archive file: " ^ af)
        end;
      do_sync f1 eko1 vto1 f2 eko2 vto2 af ekaf vtaf sync_vt_opt;
      Input.save_path f1 original_p1;
      Input.save_path f2 original_p2
  | Input.Show (replica, as_vt) ->
      let (originalp, eko, vto) = match replica with Input.Replica r -> r in
      let f = Input.load_path originalp in
      do_show f eko vto as_vt false;
      Input.save_path f originalp
  | Input.Updown (replica, as_vt) ->
      let (originalp, eko, vto) = match replica with Input.Replica r -> r in
      let f = Input.load_path originalp in
      do_show f eko vto as_vt true;
      Input.save_path f originalp
  | Input.Upcreate (replica, as_vt) ->
      let (originalp, eko, vto) = match replica with Input.Replica r -> r in
      let f = Input.load_path originalp in
      do_upcreate f eko vto as_vt;
      Input.save_path f originalp
  | Input.Identify (replica) ->
      let (originalp, eko, vto) = match replica with Input.Replica r -> r in
      let f = Input.load_path originalp in
      let ekey = get_ekey f eko in
      Surveyor.print_description ekey;
      Input.save_path f originalp
  | Input.Test ->
      Lens.trap_errors_in Test.do_tests ()
  | Input.Capabilities ->
      print_endline ("I can synchronize:");
      Safelist.iter
        (fun ekey ->
          let tds = Surveyor.get_base_type ekey in
            (* TODO: this could still use a little work.. and a little color! *)
          Format.printf "@[%-35s@ as@ @[@<20>%s@]@\n@]"
            ((Surveyor.get_description ekey) ^ "(" ^ ekey ^ ")")
            (Optometrist.type_desc_as_string tds))
        (Surveyor.get_all_encodings ());;

Unix.handle_unix_error (fun ()->
  try
    main()
  with 
  | V.Error l ->
      Format.printf "@,";
      print_string (Misc.color "-----------\nFATAL ERROR\n-----------"
                    Misc.Red ~bold:true);
      Format.printf "@,";
      V.format_msg l;
      flush stdout;
      flush stderr;
      exit 3
  | e ->
      (flush stdout;
      flush stderr;
      raise e)) ();;
