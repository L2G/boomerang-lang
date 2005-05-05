(* =================================== 
 *  CALLBACKS
 * =================================== *)

let sv_debug = false

class callbacks =

  let string2types str = 
    (try 
       let ts = Types.string2abstract_type str
       in (Some ts,"",None) 
     with 
	 Error.Syntax_error (s,info) -> (None, ("Syntax error: "^s), (Some info))
       | Error.Parse_error (s,info) -> (None, ("Parse error: "^s), (Some info))
       | _ -> (None,"Unknown error",Some Error.bogusInfo))
  in
  
  let string2view str =
    (try 
       let v_opt = 
	 (if str="" then
	    None
	  else
	    (Lexer.lineno := 0 ;
	     Lexer.linestart := 1;
	     let lexbuf = Lexing.from_string str 
	     in Some (Compiler.compile_view_lexbuf lexbuf)))
       in (v_opt,"",None)
     with  
	 Error.Syntax_error (s,info) -> (None, ("Syntax error: "^s), (Some info))
       | Error.Parse_error (s,info) -> (None, ("Parse error: "^s), (Some info))
       | _ -> (None,"Unknown error",Some Error.bogusInfo))
  in
    
  let view2string v_opt =
    let out,flush = Format.get_formatter_output_functions () in
    let res = match v_opt with
	None -> ""
      | Some v -> 
	  begin
	    if V.is_empty v then
	      "{}"
	    else
	      let buf = Buffer.create 64 in
		Format.print_flush();
		Format.set_formatter_output_functions 
		  (fun s p n -> Buffer.add_substring buf s p n) (fun () -> ());
		V.pretty_print v;
		Format.print_flush();
		Buffer.contents buf 
	  end
    in
    let _ = Format.set_formatter_output_functions out flush 
    in res
  in 
	      
  let synchronize ts_opt a_opt l_opt r_opt = 
    let at = (match ts_opt with
		  None -> assert false
		| Some ts -> ts
	     ) 
    in Sync.sync at a_opt l_opt r_opt 
  in
    
object(self)
  val mutable widgets_ = (None : Widgets.widgets option)

  val mutable ctrl_pressed = false;
  val mutable meta_pressed = false;
  
  val mutable parse_error = true;
  val mutable conflict = true;

  val mutable input_archive_v = None
  val mutable input1_v = None
  val mutable input2_v = None

  method widgets =
    match widgets_ with
      | None -> assert false
      | Some c -> c
	  
  method set_window c = widgets_ <- Some c

  method reset_error_info =
    (parse_error <- true);
    (input_archive_v <- None);
    (input1_v <- None);
    (input2_v <- None);
    (conflict <- true)

  method on_key_press (event:GdkEvent.Key.t)  = 
    let kv = GdkEvent.Key.keyval event in
    let _ = if (sv_debug) then (Format.print_int kv; Format.print_newline (); Format.print_flush ()) else () in
      if kv = 65570 then 
	(ctrl_pressed <- true; false)
      else if kv = 65511 then
	(meta_pressed <- true; false)
      else if meta_pressed & (kv = 113) then 
	begin (* pretty print all input buffers *)
	  if (not parse_error) then
	    (self#widgets#set_inputs (view2string input_archive_v) (view2string input1_v) (view2string input2_v); true)
	  else
	    true
	end
      else if meta_pressed & (kv = 111) then
	(* if conflict - copy output{1,2} to input{1,2} 
	   otherwise   - copy output_archive to all inputs *)
	begin
	  if (conflict) & (not parse_error) then 
	    (self#widgets#set_input1 self#widgets#get_output1;
	     self#widgets#set_input2 self#widgets#get_output2;
	     true)
	  else if (not parse_error) then
	    let oa = self#widgets#get_output_archive in
	      self#widgets#set_inputs oa oa oa; true
	  else 
	    true
	end
      else if meta_pressed & (kv = 97) then
	(* copy input_archive to input1 and input2 *)
	let ia = self#widgets#get_input_archive in
	  self#widgets#set_inputs ia ia ia; true
      else if meta_pressed & (kv = 49) then
	(* copy input1 to input_archive and input2 *)
	let il = self#widgets#get_input1 in
	  self#widgets#set_inputs il il il; true
      else if meta_pressed & (kv = 50) then
	(* copy input2 to input_archive and input1 *)
	let ir = self#widgets#get_input2 in
	  self#widgets#set_inputs ir ir ir; true
      else
	false
	  
  method on_key_release (event:GdkEvent.Key.t)  = 
    let kv = GdkEvent.Key.keyval event in
      if kv = 65570 then 
	(ctrl_pressed <- false; false)
      else if kv = 65511 then
	(meta_pressed <- false; false)
      else 
	false

  method check_inputs ts_err a_err l_err r_err =
    let (ts_ok,ts_str) = (match ts_err with
			      None -> (true,"")
			    | Some _ -> (false,"Parse error (types)")
			 ) in
    let (a_ok,a_str) = (match a_err with
			    None -> (true,"")
			  | Some _ -> (false,"Parse error (input archive)")
		       ) in		  
    let (l_ok,l_str) = (match l_err with
			    None -> (true,"")
			  | Some _ -> (false,"Parse error (input 1)")
		       ) in
    let (r_ok,r_str) = (match r_err with
			    None -> (true,"")
			  | Some _ -> (false,"Parse error (input 2)")
		       ) in 
    let _ = parse_error <- (not (ts_ok & a_ok & l_ok & r_ok)) 
    in
      (ts_str^" " ^a_str^" "^l_str^" "^r_str)

  method check_conflict action a'_opt l'_opt r'_opt = 
    let c_opt = (Sync.find_conflict action) in
    let (ok,msg) = (match c_opt with
			None -> (true,"")
		      | Some a -> (false,("Synchronization error: "^(Sync.get_action_name a)))) in
    let _ = conflict <- (not ok) 
    in msg
	 
  method on_change () =
    let _ = self#reset_error_info in
    let _ = self#widgets#pop_status in
    let _ = self#widgets#clear_outputs in
    let _ = self#widgets#undim_outputs in
    let _ = self#widgets#uncolor_inputs in
    let (ts_opt,ts_msg,ts_info) = string2types (self#widgets#get_type_decls) in
    let (a_opt,a_msg,a_info) = string2view (self#widgets#get_input_archive) in
    let (l_opt,l_msg,l_info) = string2view (self#widgets#get_input1) in
    let (r_opt,r_msg,r_info) = string2view (self#widgets#get_input2) in
    let _ = self#check_inputs ts_info a_info l_info r_info in
    let _ = 
      (input_archive_v <- a_opt); 
      (input1_v <- l_opt); 
      (input2_v <- r_opt) 	
    in
    let _ = 
      (if not parse_error then
	 (try 
	    let (action,a'_opt,l'_opt,r'_opt) = synchronize ts_opt a_opt l_opt r_opt in
	    let msg = self#check_conflict action a'_opt l'_opt r'_opt in
	    let _ = (if not conflict then
		       (self#widgets#set_outputs (view2string a'_opt) (view2string l'_opt) (view2string r'_opt))
		     else 
		       (self#widgets#set_outputs "" (view2string l'_opt) (view2string r'_opt);
			self#widgets#dim_outputs;
			self#widgets#push_status msg))
	    in ()
	  with 
	      Error.Type_error (msg,_) -> 
		(self#widgets#dim_outputs;
		 self#widgets#push_status ("Syncronization error: "^msg);
		 ())
	    | e -> 
		(self#widgets#dim_outputs;
		 self#widgets#push_status "Syncronization error: UNKNOWN";
		 raise e;))
       else
	 (self#widgets#color_inputs ts_info a_info l_info r_info;
	  self#widgets#dim_outputs;
	  self#widgets#push_status "Parse error")) 
    in ()
 	
end
