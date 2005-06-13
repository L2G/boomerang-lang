(*************************************)
(** Window for the Focal programing **)
(*************************************)

open GText

let fontMonospaceMediumPango = lazy (Pango.Font.from_string "monospace 14")

let tagtable = tag_table ()
let colortag = tag ~name:"color" ()
let _ = colortag#set_property (`BACKGROUND "yellow")
let _ = tagtable#add (colortag#as_tag)

class focal_options callbacks packing =
let vb = GPack.vbox ~spacing:0 ~homogeneous:false ~packing:packing () in
let lbl_interaction = 
  GMisc.label ~text:"Level of Interaction"
    ~packing:(vb#pack ~padding:0
		~fill:false
		~expand:false) () in
let rbfull = 
  GButton.radio_button ~label:"Fully interactive" 
    ~packing:(vb#pack ~padding:0
		~fill:false
		~expand:false) () in
let group = rbfull#group in
let rbget = 
  GButton.radio_button ~group:group
    ~label:"Get computation"
    ~packing:(vb#pack ~padding:0
		~fill:false
		~expand:false) () in
let rbcomp = 
  GButton.radio_button ~group:group
    ~label:"Compilation"
    ~packing:(vb#pack ~padding:0
		~fill:false
		~expand:false) () in
let rbtype = 
  GButton.radio_button ~group:group
    ~label:"Type checking"
    ~packing:(vb#pack ~padding:0
		~fill:false
		~expand:false) () in
let rbparse = 
  GButton.radio_button ~group:group
    ~label:"Parsing"
    ~packing:(vb#pack ~padding:0
		~fill:false
		~expand:false) () in
let rbnothing = 
  GButton.radio_button ~group:group
    ~label:"Nothing"
    ~packing:(vb#pack ~padding:0
		~fill:false
		~expand:false) () in
let _ =  rbfull#connect#clicked
	~callback:callbacks#on_rbfull_clicked in
let _ =  rbget#connect#clicked
	~callback:callbacks#on_rbget_clicked in
let _ =  rbcomp#connect#clicked
	~callback:callbacks#on_rbcomp_clicked in
let _ =  rbtype#connect#clicked
	~callback:callbacks#on_rbtype_clicked in
let _ =  rbparse#connect#clicked
	~callback:callbacks#on_rbparse_clicked in
let _ =  rbnothing#connect#clicked
	~callback:callbacks#on_rbnothing_clicked in
let lbl_margin = 
  GMisc.label ~text:"Size of the format margin"
    ~packing:(vb#pack ~padding:0
		~fill:false
		~expand:false) () in
let adj = 
  GData.adjustment ~value:25.0
    ~lower:3.0 ~upper:100.0
    ~step_incr:1.0 () in
let format_button =
  GEdit.spin_button ~digits:0
    ~adjustment:adj ~numeric:true
    ~value:25.0  ~update_policy:`ALWAYS
    ~snap_to_ticks:true
    ~packing:(vb#pack ~padding:0
		~fill:false
		~expand:false) () in

object (self)
  inherit GObj.widget_full vb#as_widget
  method update_margin () = Format.set_margin (format_button#value_as_int)
  method set_not_interactive ()= rbnothing#set_active true
  initializer ignore (format_button#connect#changed ~callback:(self#update_margin))
end

class focal_window callbacks =
let tooltips = GData.tooltips () in
let accel_group = GtkData.AccelGroup.create () in
let window = GWindow.window
~title: "Visual"
~resizable:true
~allow_grow:true
~allow_shrink:false
~height:600
~width:800
()
in

let _ = window#add_accel_group accel_group in

(* Main vertical box *)
let main_vertical_box = 
  GPack.vbox ~spacing:0 ~homogeneous:false ~packing:window#add
    ()
in

(* Action buttons *)
let actionBar =
  GButton.toolbar ~style:`BOTH
    ~orientation:`HORIZONTAL ~tooltips:true 
    ~packing:(main_vertical_box#pack ~expand:false) () in

(***************)
(* Lens Widget *)
(***************)
let focal_vertical_box = 
  GPack.vbox ~spacing:0 ~homogeneous:false
    ~packing:(main_vertical_box#pack ~padding:0
		~fill:true
		~expand:true
	     )
    ()
in
let focal_main_panel =
  GPack.paned `VERTICAL ~border_width:0
    ~packing:(focal_vertical_box#pack ~padding:0
		~fill:true
		~expand:true
	     )
    ()
in
let focal_main_horizontal_box = 
  GPack.hbox ~spacing:0 ~homogeneous:false
    ~packing:(focal_main_panel#pack1)
    ()
in
let focal_first_panel =
  GPack.paned `VERTICAL
    ~packing:(focal_main_horizontal_box#pack ~padding:0
		~fill:true
		~expand:true)
    ()
in
let focal_scrolled_window = 
  GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`ALWAYS
    ~packing:(focal_first_panel#pack1)
    ()
in
(* text area *)
let focal_buffer = GText.buffer ~tag_table:tagtable ~text:"" () in
let focal_texteditor = 
  GText.view
    ~buffer:focal_buffer
    ~packing:focal_scrolled_window#add
    ~editable:true
    ()
in
let _ = focal_texteditor#misc#modify_font (Lazy.force fontMonospaceMediumPango) in
(* Types *)
let focal_types_scrolled_window = 
  GBin.scrolled_window
    ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
    ~packing:(focal_first_panel#pack2)
    ()
in
let focal_types_buffer = GText.buffer ~tag_table:tagtable ~text:"" () in
let focal_types_texteditor = 
  GText.view
    ~buffer:focal_types_buffer
    ~packing:focal_types_scrolled_window#add
    ~editable:false
    ~wrap_mode:`WORD
    ()
in

(* Options *)
let focal_options =
  new focal_options callbacks (focal_main_horizontal_box#pack ~padding:0
				 ~fill:false
				 ~expand:false
			      )
in
let _ = focal_options#misc#hide () in
	
let option_present = ref false in
let _ = 
  actionBar#insert_button ~text:"New"
    ~icon:((GMisc.image ~stock:`NEW ())#coerce)
       ~tooltip:"New Focal Program"
       ~callback:(callbacks#on_new_activate) () in
let _ = 
  actionBar#insert_button ~text:"Lens"
    ~icon:((GMisc.image ~stock:`OPEN ())#coerce)
       ~tooltip:"Open a Focal Program"
       ~callback:(callbacks#on_open_activate) () in
let _ = 
  actionBar#insert_button ~text:"Tree"
    ~icon:((GMisc.image ~stock:`OPEN ())#coerce)
       ~tooltip:"Open a Tree"
       ~callback:(callbacks#on_openview_activate) () in
let _ = 
  actionBar#insert_button ~text:"Save"
    ~icon:((GMisc.image ~stock:`SAVE ())#coerce)
       ~tooltip:"Save the current Focal Program"
       ~callback:(callbacks#on_save_activate) () in
let _ = 
  actionBar#insert_button ~text:"Quit"
    ~icon:((GMisc.image ~stock:`QUIT ())#coerce)
       ~tooltip:"Quit the visualizer"
       ~callback:(callbacks#gtk_main_quit) () in
let _ = 
  actionBar#insert_space () in
let compile_button = 
  actionBar#insert_button ~text:"Compile"
    ~icon:((GMisc.image ~stock:`EXECUTE ())#coerce)
       ~tooltip:"Compile and apply the program"
       ~callback:(fun () ->()) () in
let _ = compile_button#misc#set_sensitive false in
let _ = 
  actionBar#insert_space () in
let _ = 
  actionBar#insert_button ~text:"Example"
    ~icon:((GMisc.image ~stock:`FIND ())#coerce)
       ~tooltip:"Show the Example Window"
       ~callback:(callbacks#on_see_example_clicked) () in
let _ = 
  actionBar#insert_button ~text:"Probes"
    ~icon:((GMisc.image ~stock:`INDEX ())#coerce)
       ~tooltip:"Show the Probes Window"
       ~callback:(callbacks#on_seeprobe_clicked) () in
let _ = 
  actionBar#insert_space () in
let _ = 
  actionBar#insert_button ~text:"Options"
    ~icon:((GMisc.image ~stock:`PROPERTIES ())#coerce)
       ~tooltip:"Show the Option Menu"
       ~callback:(fun () ->if !option_present 
		  then begin option_present:=false;
		    focal_options#misc#hide () end
		  else begin option_present:=true;
		    focal_options#misc#show () end) () in
(* Example *)
let example_widget = 
    new Ui_example.example_widget callbacks 
      ~packing:(focal_main_panel#pack2 ~shrink:true) 
in
(* Error Status Bar *)
let focal_statusbar =
  GMisc.statusbar ~packing:(focal_vertical_box#pack ~padding:0
		~fill:false
		~expand:false
	     ) () in
let focal_statusbar_context = focal_statusbar#new_context ~name:"error" in
let _ = focal_statusbar_context#push "OK" in


let _ =  focal_texteditor#buffer#connect#changed
	~callback:callbacks#on_focal_texteditor_changed in
let _ = compile_button#connect#clicked 
	  ~callback:callbacks#on_compile_clicked in
let _ =  window#coerce#misc#connect#destroy
	   ~callback:callbacks#gtk_main_quit in
object(self)
  method activate_compile ()= compile_button#misc#set_sensitive true
  method desactivate_compile ()= compile_button#misc#set_sensitive false
  method tooltips = tooltips
  method window = window
  method focal_buffer = focal_buffer
  method focal_types_buffer = focal_types_buffer
  method add_error s = 
    focal_statusbar_context#pop ();
    ignore (focal_statusbar_context#push s)
  method focal_options = focal_options
  method init () = self#focal_buffer#set_text "let main : lens = id"
  method example_window = example_widget
  initializer callbacks#set_window self 
end


