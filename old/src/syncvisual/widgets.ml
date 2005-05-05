(* =================================== 
 *  GTK WIDGETS
 * =================================== *)

let fontMonospaceMediumPango = lazy (Pango.Font.from_string "monospace 16")

let tagtable = GText.tag_table () 

let yellowtag = GText.tag ~name:"yellow" ()
let _ = yellowtag#set_property (`BACKGROUND "yellow2")
let _ = tagtable#add (yellowtag#as_tag)

let greentag = GText.tag ~name:"green" ()
let _ = greentag#set_property (`BACKGROUND "green")
let _ = tagtable#add (greentag#as_tag)

let redtag = GText.tag ~name:"red" ()
let _ = redtag#set_property (`BACKGROUND "red")
let _ = tagtable#add (redtag#as_tag)

let pinktag = GText.tag ~name:"pink" ()
let _ = pinktag#set_property (`BACKGROUND "pink")
let _ = tagtable#add (pinktag#as_tag)
				 
class widgets callbacks =

  let create_editor (box:GPack.box) label wrap on_change on_key_press on_key_release =
    let frame = 
      GBin.frame 
	~label:label 
	~label_xalign:1.0 
	~shadow_type:`ETCHED_IN 
	~packing:(box#pack ~expand:true ~fill:true ~padding:0) 
	() 
    in
    let scroller = 
      GBin.scrolled_window
	~hpolicy:`AUTOMATIC 
	~vpolicy:`AUTOMATIC
	~packing:frame#add
	() 
    in
    let buffer = 
      GText.buffer 
	~tag_table:tagtable 
	~text:"" 
	() 
    in
    let editor = 
      GText.view
	~buffer:buffer
	~packing:scroller#add
	~wrap_mode:(if wrap then `WORD else `NONE)
	~editable:(on_change!=None) 
	() 
    in
    let _ = editor#misc#modify_font 
	      (Lazy.force fontMonospaceMediumPango) in
    let _ = match on_change with 
	Some cb -> 
	  editor#buffer#connect#changed ~callback:cb; ()
      | None -> ()
    in 
    let _ = match on_key_press with
	Some cb -> 
	  editor#event#connect#key_press ~callback:cb; ()
      | None -> ()
    in
    let _ = match on_key_release with
	Some cb -> 
	  editor#event#connect#key_release ~callback:cb; ()
      | None -> ()
    in
      editor
  in
 
  let window = 
    GWindow.window 
      ~title:"Harmony Synchronization Visualizer" 
      ~width:800 
      ~height:600
      ~border_width:15 
      () 
  in
  let _ = window#connect#destroy ~callback:GMain.Main.quit in 
    
  let box = 
    GPack.vbox 
      ~homogeneous:false
      ~packing:window#add 
      () 
  in

  let type_decls = 
    create_editor box "Type Declarations" true (Some callbacks#on_change) None None in

  let input_box = 
    GPack.hbox 
      ~homogeneous:true
      ~packing:(box#pack ~expand:true ~fill:true ~padding:0) 
      () 
  in
    
  let input_archive = create_editor 
			input_box 
			"Input Archive" 
			true 
			(Some callbacks#on_change) 
			(Some callbacks#on_key_press)  
			(Some callbacks#on_key_release)
  in

  let input1 = create_editor 
		 input_box 
		 "Input 1" 
		 true 
		 (Some callbacks#on_change) 
		 (Some callbacks#on_key_press) 
		 (Some callbacks#on_key_release)
  in

  let input2 = create_editor 
		 input_box 
		 "Input 2" 
		 true 
		 (Some callbacks#on_change) 
		 (Some callbacks#on_key_press) 
		 (Some callbacks#on_key_release)
  in
    
  let output_box = 
    GPack.hbox 
      ~homogeneous:true
      ~packing:(box#pack ~expand:true ~fill:true ~padding:0) 
    () 
  in
  
  let output_archive = create_editor 
			 output_box 
			 "Output Archive" 
			 true 
			 None 
			 None 
			 None 
  in

  let output1 = create_editor 
		  output_box 
		  "Output 1"
		  true 
		  None 
		  None 
		  None 
  in

  let output2 = create_editor 
		  output_box 
		  "Output 2" 
		  true 
		  None 
		  None 
		  None 
  in
    
  let statusbar = 
    GMisc.statusbar
      ~height:20
      ~packing:(box#pack ~expand:false ~fill:false ~padding:0) 
      () 
  in      
  let statusbar_context = statusbar#new_context ~name:"ctx" in

  let remove_all_tags buffer = 
    buffer#remove_all_tags
      ~start:buffer#start_iter
      ~stop:buffer#end_iter
  in

  let set_tag buffer pos1 pos2 = 
    let line1 = fst pos1 in
    let line2 = fst pos2 in
    let char1 = max (snd pos1-2) 0 in
    let char2 = max (snd pos2-1) 0 in
    let it_start = buffer#get_iter (`START) in
    let it1 = buffer#get_iter (`LINECHAR (line1,char1)) in
    let it2 = buffer#get_iter (`LINECHAR (line2,char2)) in
    let it_end = (buffer#get_iter (`START))#forward_to_end 
    in
      remove_all_tags buffer;
      buffer#apply_tag_by_name 
	"pink"  
	~start:it1
	~stop:it2;
  in

  let set_tag_opt buffer info_opt =
    match info_opt with
	Some (pos1,pos2) -> set_tag buffer pos1 pos2
      | None -> ()
  in
		     
object(self)
  method window = window
  method get_type_decls = type_decls#buffer#get_text ()
  method get_input_archive = input_archive#buffer#get_text ()
  method get_input1 = input1#buffer#get_text ()
  method get_input2 = input2#buffer#get_text ()
  method set_type_decls str= type_decls#buffer#set_text str
  method set_input_archive str = input_archive#buffer#set_text str
  method set_input1 str = input1#buffer#set_text str
  method set_input2 str = input2#buffer#set_text str
  method set_inputs a l r = 
    begin
      self#set_input_archive a; 
      self#set_input1 l; 
      self#set_input2 r
    end
  method color_type_decls (info_opt:Error.info option) = () (* set_tag_opt type_decls#buffer info_opt *)
  method color_input_archive (info_opt:Error.info option) = set_tag_opt input_archive#buffer info_opt
  method color_input1 (info_opt:Error.info option) = set_tag_opt input1#buffer info_opt
  method color_input2 (info_opt:Error.info option) = set_tag_opt input2#buffer info_opt
  method color_inputs ts_info a_info l_info r_info =
    begin
      self#color_type_decls ts_info;
      self#color_input_archive a_info;
      self#color_input1 l_info;
      self#color_input2 r_info
    end
  method uncolor_inputs = 
    begin
      remove_all_tags type_decls#buffer;
      remove_all_tags input_archive#buffer;
      remove_all_tags input1#buffer;
      remove_all_tags input2#buffer
    end
  method get_output_archive = output_archive#buffer#get_text ()
  method get_output1 = output1#buffer#get_text ()
  method get_output2 = output2#buffer#get_text ()
  method set_output_archive str = output_archive#buffer#set_text str
  method set_output1 str = output1#buffer#set_text str
  method set_output2 str = output2#buffer#set_text str
  method pop_status = statusbar_context#pop ()
  method push_status msg = statusbar_context#push msg; ()
  method set_outputs a l r = 
    begin
      self#set_output_archive a; 
      self#set_output1 l; 
      self#set_output2 r
    end
  method clear_outputs = self#set_outputs "" "" ""
  method dim_outputs = 
    begin
      output_archive#misc#set_sensitive false;
      output1#misc#set_sensitive false;
      output2#misc#set_sensitive false;
    end
  method undim_outputs = 
    begin
      output_archive#misc#set_sensitive true;
      output1#misc#set_sensitive true;
      output2#misc#set_sensitive true;
    end
  initializer 
    begin
      self#set_type_decls "type Any = *[Any]";
      self#set_input_archive "{}";
      self#set_input1 "{}";
      self#set_input2 "{}";
      callbacks#set_window self;
      callbacks#on_change ()
    end
end

