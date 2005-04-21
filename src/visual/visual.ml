open GText
open GToolbox
open Lexing

(* Types for probes & tracepoints *)

type probecall =
    Get of string * V.t * (Lens.stackframe list)
  | Put of string * V.t * V.t option * (Lens.stackframe list)

(* global, mutable variable declarations *)
let concrete1 = ref (Some V.empty)
let abstract1 = ref V.empty 
let abstract2 = ref V.empty
let concrete2 = ref V.empty
let lens = ref None 

(* Debugging *)
let debug_ui = ref false
let debug s = if !debug_ui then prerr_endline s

(* Text formatting *)
let _ = Format.set_margin 25

let encodingslist = Surveyor.get_all_encodings ()
let view_typelist = Safelist.map Surveyor.get_base_type encodingslist
(*
let assoclist = Safelist.flatten 
		  (Safelist.map 
		     (function (v,lensll) -> Safelist.map 
			(fun x -> (v,x)) lensll) 
		     (Safelist.flatten (Safelist.map Optometrist.can_sync_as1 view_typelist)))
*)

let string_of_tree t =
  let out,flush = Format.get_formatter_output_functions () in
  let buf = Buffer.create 64 in
  Format.set_formatter_output_functions 
    (fun s p n -> Buffer.add_substring buf s p n) (fun () -> ());
  V.format t;
  Format.print_flush();
  let s = Buffer.contents buf in
  Format.set_formatter_output_functions out flush;
  s


(**********************************)
(** Callbacks from the interface **)
(**********************************)
class customized_callbacks = object(self)
  inherit Callbacks.default_callbacks
  
  val avaible = ref true
  val mutable example_present = true
  val mutable trace_present = false

  val mutable interactive_put = true
  val mutable interactive_get = true
  val mutable interactive_compile = true
  val mutable interactive_type = true
  val mutable interactive_parse = true

(* Callbacks for the focal editor *)

  (* helper: reads in a file to the lens pane *)
  method input_focal_file filename =    
    let content = 
      try Misc.read filename
      with _ -> Ui_common.error avaible "I/O Error";"" in
    if content <> "" then
      begin
        self#focal_window#focal_buffer#set_text content
      end     

  method on_open_activate () =
    if !avaible then
      begin
	Ui_common.select_file avaible 
	  (function filename -> self#input_focal_file filename);
	  debug "Event activate from open";() 
      end
      
  method on_new_activate () =
    if !avaible then
      begin
        self#focal_window#focal_buffer#set_text "do id";
	debug "Event activate from new";() 
      end

  method on_save_activate () =
    if !avaible then
      begin
        Ui_common.select_file avaible 
	  (function filename -> 
	     let content =  self#focal_window#focal_buffer#get_text () in
	     try Misc.write filename content
	     with _ -> Ui_common.error avaible "I/O Error"
	  );     
	debug "Event activate from save";() 
      end
	
  method on_edit_activate () =
    if !avaible then
      begin
        debug "Event activate from edit";() 
      end
      
  method on_about_activate () =
    if !avaible then
      begin
        debug "Event activate from about";() 
      end
	
  method on_see_example_clicked () =
    if !avaible then
      if example_present then
	begin
	  self#focal_window#example_window#misc#hide ();
	  example_present <- false
	end
(*example_destroy ()*)
      else
	begin
	  example_present <- true;
	  self#focal_window#example_window#misc#show ();
          debug "Event clicked from show_example";()
	end

  method on_seeprobe_clicked () =
    if !avaible then
      if trace_present then
	begin
	  self#trace_destroy ()
	end
      else
      begin
	trace_present <- true;
	self#trace_window#window#show ();
        debug "Event clicked from show_probe";()
      end

  method compile force () =
    let pos_to_iter = self#focal_window#focal_buffer#get_iter in
    let add_tag (pos1,pos2) =
      let it1,it2 = pos_to_iter (`LINECHAR (fst pos1 -1,max (snd pos1 -1) 0)),
      pos_to_iter (`LINECHAR (fst pos2-1,max (snd pos2 -1) 0)) in
      self#focal_window#focal_buffer#remove_all_tags
	~start:(self#focal_window#focal_buffer#start_iter) 
	~stop:(self#focal_window#focal_buffer#end_iter);
      self#focal_window#focal_buffer#apply_tag_by_name "color"  
	~start:it1 ~stop:it2 in
    let remove_tag () =
      self#focal_window#focal_buffer#remove_all_tags
	~start:(self#focal_window#focal_buffer#start_iter) 
	~stop:(self#focal_window#focal_buffer#end_iter) in
    Lexer.lineno := 0 ;
    Lexer.linestart := 1;
    Checker.init_varcounter ();
    if interactive_parse || force then
      begin
	self#focal_window#add_error "OK";
	self#init_get_probe ();
	self#init_put_probe ();
	remove_tag ();
	try
	  let lexbuf = Lexing.from_string ("\n"^(self#focal_window#focal_buffer#get_text ())) in
	  let ast    = Parser.prog Lexer.token lexbuf in
	  if interactive_type || force then
	    begin
	      let p,typ  = Checker.nicetypesoftheprog ast in
	      let s = (Checker.types_to_string typ) in
	      self#focal_window#focal_types_buffer#set_text s;
	      if interactive_compile || force then
		begin
		  let l      = Compiler.compileprog p in 
		  lens := (Some l);
		  if interactive_get || force then
		    begin
		      self#update_abstract1 ();
		      if interactive_put || force then
			begin
			  self#update_concrete2 ();
			  ()
		      end
		    end
		end
	    end
	with
	    Error.Syntax_error (s,info) ->
	      (self#focal_window#add_error "KO";
	       add_tag info;lens := None;
	       self#focal_window#focal_types_buffer#set_text ("Syntax error "^s))
	  | Error.Parse_error (s,info) ->
	      (self#focal_window#add_error "KO";
	       add_tag info;lens := None;
	       self#focal_window#focal_types_buffer#set_text ("Parse error "^s))
	  | Error.Type_error (s,info) ->
	      (self#focal_window#add_error "KO";
	       add_tag info;lens := None;
	       self#focal_window#focal_types_buffer#set_text ("Type error "^s))
	  | Error.Run_error (s,info) ->
	      (self#focal_window#add_error "KO";
	       add_tag info;lens := None;
	       self#focal_window#focal_types_buffer#set_text ("Run error "^s))
      end
    ;
    ()

  method on_focal_texteditor_changed () = 
    let _ = self#focal_window#add_error "" in
    self#compile false ()

  method on_compile_clicked() = 
    self#compile true ()

  (* Option window *)

  method on_rbfull_clicked () =
    interactive_put <- true;
    interactive_get <- true;
    interactive_compile <- true;
    interactive_type <- true;
    interactive_parse <- true;
    self#compile false ();
    self#focal_window#desactivate_compile ();
    debug "Event clicked on rbfull";()
  method on_rbget_clicked () =
    interactive_put <- false;
    interactive_get <- true;
    interactive_compile <- true;
    interactive_type <- true;
    interactive_parse <- true;
    self#compile false ();
    self#focal_window#activate_compile ();
    debug "Event clicked on rbget";()
  method on_rbcomp_clicked () =
    interactive_put <- false;
    interactive_get <- false;
    interactive_compile <- true;
    interactive_type <- true;
    interactive_parse <- true;
    self#compile false ();
    self#focal_window#activate_compile ();
    debug "Event clicked on rbcomp";()
  method on_rbtype_clicked () =
    interactive_put <- false;
    interactive_get <- false;
    interactive_compile <- false;
    interactive_type <- true;
    interactive_parse <- true;
    self#compile false ();
    self#focal_window#activate_compile ();
    debug "Event clicked on rbtype";()
  method on_rbparse_clicked () =
    interactive_put <- false;
    interactive_get <- false;
    interactive_compile <- false;
    interactive_type <- false;
    interactive_parse <- true;
    self#compile false ();
    self#focal_window#activate_compile ();
    debug "Event clicked on rbparse";()
  method on_rbnothing_clicked () =
    interactive_put <- false;
    interactive_get <- false;
    interactive_compile <- false;
    interactive_type <- false;
    interactive_parse <- false;
    self#compile false ();
    self#focal_window#activate_compile ();
    debug "Event clicked on rbnothing";()



(* Callbacks or the Example Window *)

  method on_concrete1_changed () =
    Lexer.lineno := 0 ;
    Lexer.linestart := 1;
    self#init_get_probe ();
    self#init_put_probe ();
    let s=(self#focal_window#example_window#concrete1#buffer#get_text ()) in
    let lexbuf = Lexing.from_string s in
    let b= ref false in
    if s="" then (concrete1 := None;
		  self#focal_window#example_window#abstract1#buffer#set_text "Missing will be used.";
		  self#update_concrete2 ())
    else
      let tree = begin
	try
	  let t = (Compiler.compile_view_lexbuf lexbuf) in
	  self#focal_window#example_window#abstract1#buffer#set_text "";
	  b:=true;
	  t
	with
	    Failure e -> 
	      self#focal_window#example_window#abstract1#buffer#set_text e;
	      V.empty
	  | Error.Parse_error (s,info) ->
	      self#focal_window#example_window#abstract1#buffer#set_text s;
	      V.empty
      end in
      if !b then
	begin
	  concrete1 := Some tree;
	  self#update_abstract1 ();
	  self#update_concrete2 ()
	end;
    debug "Event changed from concrete1";
      () 
      
  method on_abstract2_changed () =
    Lexer.lineno := 0 ;
    Lexer.linestart := 1;
    self#init_put_probe ();
    let lexbuf = Lexing.from_string (self#focal_window#example_window#abstract2#buffer#get_text ()) in
    let b= ref false in
    let tree =
      begin
	try
(*	  try*)
	let t =(Compiler.compile_view_lexbuf lexbuf) in 
	self#focal_window#example_window#concrete2#buffer#set_text "";
	b:=true;
	t
	with
	    Failure e -> 
	      self#focal_window#example_window#concrete2#buffer#set_text e;
	      V.empty
	  | Error.Parse_error (s,info) ->
	      self#focal_window#example_window#concrete2#buffer#set_text s;
	      V.empty
      end in
    if !b then
      begin
	abstract2 := tree;
	self#update_concrete2 ()
      end;
    debug "Event changed from abstract2";()

  method update_abstract1 () =
    if interactive_get then begin
      self#set_fun_get ();
      match !lens,!concrete1 with
	| None,_ -> ()
	| Some l,Some c -> begin
	    try  
	      abstract1 := Lens.get l c;
	      let s = string_of_tree (!abstract1) in
		self#focal_window#example_window#abstract1#buffer#set_text s;
		self#on_initialize_abstract_1_activate ()
	    with
	      | V.Error msglist -> (
		  Format.print_flush ();
		  let out,flush = Format.get_formatter_output_functions () in
		  let buf = Buffer.create 64 in
		  Format.set_formatter_output_functions 
		    (fun s p n -> Buffer.add_substring buf s p n) (fun () -> ());
		  V.format_msg msglist;
		  Format.print_flush();
		  let s = Buffer.contents buf in
		  Format.set_formatter_output_functions out flush;
		  self#focal_window#example_window#abstract1#buffer#set_text s)
	      | V.Illformed ( s,_ ) -> 
		  (self#focal_window#example_window#abstract1#buffer#set_text s)
	  end
	| Some _,None -> 
	    abstract1 := V.empty;
	    let s = string_of_tree (!abstract1) in
	    self#focal_window#example_window#abstract1#buffer#set_text ""
    end

  method update_concrete2 () =
    if interactive_put then begin
      self#set_fun_put ();
      match !lens with
	| None -> ()
	| Some l ->
	    try
	      concrete2 := V.empty;
	      concrete2 := Lens.put l (!abstract2) (!concrete1);
	      let s = string_of_tree (!concrete2) in
	      self#focal_window#example_window#concrete2#buffer#set_text s
	    with
		V.Error msglist -> (
		  Format.print_flush ();
		  let out,flush = Format.get_formatter_output_functions () in
		  let buf = Buffer.create 64 in
		  Format.set_formatter_output_functions 
		    (fun s p n -> Buffer.add_substring buf s p n) (fun () -> ());
		  V.format_msg msglist;
		  Format.print_flush();
		  let s = Buffer.contents buf in
		  Format.set_formatter_output_functions out flush;
		  self#focal_window#example_window#concrete2#buffer#set_text s)
	      | V.Illformed ( s,_ ) -> 
		  (self#focal_window#example_window#concrete2#buffer#set_text s)
    end


  method on_initialize_abstract_1_activate () =
    if !avaible then
      begin
	abstract2 := (!abstract1) ;
	let s = string_of_tree (!abstract2) in
	self#focal_window#example_window#abstract2#buffer#set_text s;
	debug "Event activate from initialize_abstract_1";() 
      end
(* Not used, but perhaps useful in the future *)
(*  method enc_entries view filename = 
    List.map 
      (function enc -> 
	 `I (enc,function () ->
	       try
		 let reader = Surveyor.get_reader enc in
		 begin
		   try 
		     let treeopt = reader filename in
		     match treeopt with
			 Some t -> 
			   let file_content = string_of_tree t in
			   view#buffer#set_text file_content;
			   ()
		       | None -> ()
		   with Sys_error _ -> Ui_common.error avaible "Incorrect file!"
(*		     | _ -> () *)
		 end;
		 avaible := true
	       with
		   Failure e -> prerr_string (e^"\n")
		   
	    )) 
      (Surveyor.get_all_encodings ())
 *)
  method on_openview_activate () =
    if !avaible then
      begin
	Ui_common.select_file avaible 
	  (function filename -> 
	     try
	       let enc = List.hd (Surveyor.find_encodings filename None) in
	       let reader = Surveyor.get_reader enc in
	       begin
		 try 
		   let treeopt = reader filename in
		   match treeopt with
		       Some t -> 
			 let file_content = string_of_tree t in
			 self#focal_window#example_window#concrete1#buffer#set_text file_content;
			   ()
		     | None -> ()
		 with Sys_error _ -> Ui_common.error avaible "Incorrect file!"
(*		     | _ -> () *)
	       end;
	       avaible := true
	     with
		 Failure e -> prerr_string (e^"\n"));
	debug "Event activate from openview";() 
      end

(******************)
(* Probe handling *)
(******************)
  val mutable get_probe_list = ([]:(probecall list))
  val mutable put_probe_list = ([]:(probecall list))
  val mutable add_probe_ref = (fun n p -> ())

  method set_fun_get () =
    add_probe_ref <- self#add_get_probe

  method set_fun_put () =
    add_probe_ref <- self#add_put_probe

  method init_get_probe () = 
    Pervasives_plugin.ref_get_probe := (fun n c s -> self#add_probe n (Get(n,c,s)));
    Pervasives_plugin.ref_put_probe := (fun n a copt s -> self#add_probe n (Put(n,a,copt,s)));
    get_probe_list <- [];
    self#trace_window#get_probe#init ();
    let c = self#trace_window#get_probe#probe_list#children in
    List.iter (self#trace_window#get_probe#probe_list#remove) c;
    self#trace_window#get_probe#buffer#set_text ""

  method init_put_probe () =
    Pervasives_plugin.ref_get_probe := (fun n c s -> self#add_probe n (Get(n,c,s)));
    Pervasives_plugin.ref_put_probe := (fun n a copt s -> self#add_probe n (Put(n,a,copt,s)));
    put_probe_list <- [];
    self#trace_window#put_probe#init ();
    let c = self#trace_window#put_probe#probe_list#children in
    List.iter (self#trace_window#put_probe#probe_list#remove) c;
    self#trace_window#put_probe#buffer#set_text ""


  method on_seeprobe_clicked () =
    if !avaible then
      if trace_present then
	begin
	  self#trace_destroy ()
	end
      else
      begin
	trace_present <- true;
	self#trace_window#window#show ();
        debug "Event clicked from show_probe";()
      end


  method add_get_probe n p =
    if not trace_present 
    then begin
      trace_present <- true;
      self#trace_window#window#show ()
    end;
    get_probe_list <- get_probe_list @ [p];
    let s = match p with Get _ -> "GET " | Put _ -> "PUT " in
    self#trace_window#get_probe#add_probe (s^n);
    ()

  method add_put_probe n p =
    if not trace_present 
    then begin
      trace_present <- true;
      self#trace_window#window#show ()
    end;
    put_probe_list <- put_probe_list @ [p];
    let s = match p with Get _ -> "GET " | Put _ -> "PUT " in
    self#trace_window#put_probe#add_probe (s^n);
    ()

  method add_probe = 
    add_probe_ref

  method print_frame : probecall -> V.msg list = function 
      Get(n,c,s)->([`String
		      (Misc.color n Misc.Yellow ~bold:true);
		    `Break;`String
		      (Misc.color "---------------------------------------------"
			 Misc.Yellow ~bold:true);
		    `Break;`String (Misc.color "Call of get with argt:" Misc.Yellow ~bold:true);
		    `Break;`View c;`Break; `String
		      (Misc.color "---------------------------------------------"
			 Misc.Yellow ~bold:true);`Break;
		    `String (Misc.color "STACK DUMP:" Misc.Yellow ~bold:true);
		    `Break]@(List.concat (List.map Lens.dumpframe s)))
    | Put(n,a,copt,s)->([`String
			   (Misc.color n Misc.Yellow ~bold:true);
			 `Break;`String
			   (Misc.color "---------------------------------------------"
			      Misc.Yellow ~bold:true);
			 `Break;`String (Misc.color "Call of put with argt:" Misc.Yellow ~bold:true);
			 `Break;`View a ; `Break]@
			(match copt with None -> [`String "MISSING";`Break] | Some c -> [`View c; `Break])@
			[ `String
			    (Misc.color "---------------------------------------------"
			       Misc.Yellow ~bold:true);`Break;
			  `String (Misc.color "STACK DUMP:" Misc.Yellow ~bold:true);
			  `Break]@(List.concat (List.map Lens.dumpframe s)))

  method select_get_probe (i:int) =
    let p = List.nth get_probe_list i in
    let msg = self#print_frame p in
    let out,flush = Format.get_formatter_output_functions () in
    let buf = Buffer.create 64 in
    Format.set_formatter_output_functions 
      (fun s p n -> Buffer.add_substring buf s p n) (fun () -> ());
    V.format_msg msg;
    Format.print_flush();
    let s = Buffer.contents buf in
    Format.set_formatter_output_functions out flush;
    self#trace_window#get_probe#buffer#set_text s;
    debug ("Event select from get_probe_list: no"^(string_of_int i)); ()

  method select_put_probe (i:int) =
    let p = List.nth put_probe_list i in
    let msg =self#print_frame p in
    let out,flush = Format.get_formatter_output_functions () in
    let buf = Buffer.create 64 in
    Format.set_formatter_output_functions 
      (fun s p n -> Buffer.add_substring buf s p n) (fun () -> ());
    V.format_msg msg;
    Format.print_flush();
    let s = Buffer.contents buf in
    Format.set_formatter_output_functions out flush;
    self#trace_window#put_probe#buffer#set_text s;
    debug ("Event select from put_probe_list: no"^(string_of_int i)); ()

  method trace_destroy () =
    self#trace_window#window#misc#hide ();
    trace_present <- false;
    self#set_trace_window (new Ui_trace.probe_window self);
    debug "Event destroy from trace_window";()

end

(* helpers for command-line argument parsing *)
let fail s = Format.eprintf "%s@." s; exit 1

    
(* Parse command line arguments *)
let _ = 
  Arg.parse 
    [
      ("-debug-ui",
       Arg.Unit(fun _ -> debug_ui:=true),
       "\tPrint debugging messages"
      );
      ("-test",
       Arg.Unit (fun _ -> Library.get_tests () ()),
       "\tDo unit tests of registered lenses")     
    ]
    (fun s -> fail (Printf.sprintf "extra argument %s" s))
    ("usage: visual [options]")

(** Main function **)
let main () = 
  let callbacks = new customized_callbacks in
  let focal_window = new Ui_focal.focal_window callbacks in
  let trace_window = new Ui_trace.probe_window callbacks in
    (* You should probably remove the next line if you want to use the event masks from glade *)
  let _ = GtkBase.Widget.add_events focal_window#window#as_widget [`ALL_EVENTS] in
  let _ = focal_window#init () in
  let _ = focal_window#window#show() in    
    GMain.Main.main ()

let _ =    
  (* fire up the GUI *)
  (* Change to [GtkThread.start ()] to use threads *)
  let _ = GtkMain.Main.init () in     
  let _ = Printexc.print main () in
    ()

