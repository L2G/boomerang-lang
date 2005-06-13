(*****************************)
(** Window for the examples **)
(*****************************)


open GText

let fontMonospaceMediumPango = lazy (Pango.Font.from_string "monospace 14")


let tagtable = tag_table ()
let colortag = tag ~name:"color" ()
let _ = colortag#set_property (`BACKGROUND "yellow")
let _ = tagtable#add (colortag#as_tag)

class example_widget ~packing ?(show=true) callbacks =
  let main_vertical_box = 
    GPack.vbox ~spacing:0 ~homogeneous:false ~packing:packing ()
  in
  (* Titles *)
  let title_hbox = 
    GPack.hbox ~spacing:0 ~homogeneous:true
      ~packing:(main_vertical_box#pack ~padding:0
		  ~fill:false
		  ~expand:false
	       )
      ()
  in
  let concretelabel = 
    GMisc.label	~text: "Old concrete (type here)"
      ~packing:(title_hbox#pack ~padding:0
		  ~fill:false
		  ~expand:false
	       )
      ~xalign:0.5 ~yalign:0.5 ~xpad:0 ~ypad:0
      ~line_wrap:false
      ()
  in
  let abstractlabel = 
    GMisc.label ~text: "Old abstract"
      ~packing:(title_hbox#pack ~padding:0
		  ~fill:false
		  ~expand:false
	       )
      ~xalign:0.5 ~yalign:0.5 ~xpad:0 ~ypad:0
      ~line_wrap:false
      ()
  in
  let abstract2label = 
    GMisc.label ~text: "New abstract (type here)"
      ~packing:(title_hbox#pack ~padding:0
		  ~fill:false
		  ~expand:false
	       )
      ~xalign:0.5 ~yalign:0.5 ~xpad:0 ~ypad:0
      ~line_wrap:false
      ~show:show
      ()
  in
  let concrete2label = 
    GMisc.label ~text: "New concrete"
      ~packing:(title_hbox#pack ~padding:0
		  ~fill:false
		  ~expand:false
	       )
      ~xalign:0.5 ~yalign:0.5 ~xpad:0 ~ypad:0
      ~line_wrap:false
      ~show:show
      ()
  in
  (* Trees *)
  let view_hbox = 
    GPack.hbox
      ~spacing:2 ~homogeneous:false
      ~packing:(main_vertical_box#pack ~padding:0
		  ~fill:true
		  ~expand:true
	       )
      ()
  in
  (* Conc1 *)
  let scrolledwindow_con1 = 
    GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(view_hbox#pack ~padding:0
		  ~fill:true
		  ~expand:true
	       )
      ()
  in
  let bufcon1 = GText.buffer ~tag_table:tagtable () in
  let concrete1 = 
    GText.view
      ~buffer:bufcon1
      ~packing:scrolledwindow_con1#add
      ~wrap_mode:`WORD
      ~editable:true
      ()
  in
  let _ = concrete1#misc#modify_font (Lazy.force fontMonospaceMediumPango) in
  let _ = concrete1#buffer#insert"{}" in
  (* Separator 1 *)
  let sep1 = GMisc.separator `VERTICAL ~packing:(view_hbox#pack ~padding:0
		  ~fill:false
		  ~expand:false
	       ) () in
  (* Abst1 *)
  let scrolledwindow_abs1 = 
    GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(view_hbox#pack ~padding:0
		  ~fill:true
		  ~expand:true
	       )
      ()
  in 
  let bufabs1 = GText.buffer ~tag_table:tagtable () in
  let abstract1 = 
    GText.view
      ~buffer:bufabs1
      ~packing:scrolledwindow_abs1#add
      ~editable:false
      ~wrap_mode:`WORD
      ~cursor_visible:false
      ()
  in
  let _ = abstract1#misc#modify_font (Lazy.force fontMonospaceMediumPango) in
  let _ = abstract1#buffer#insert "{}" in
  (* Separator 2 *)
  let sep2 = GMisc.separator `VERTICAL ~packing:(view_hbox#pack ~padding:0
		  ~fill:false
		  ~expand:false
	       ) () in
  (* Abst2*)
  let scrolledwindow_abs2 = 
    GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(view_hbox#pack ~padding:0
		  ~fill:true
		  ~expand:true
	       )
      ~show:show
      ()
  in
  let bufabs2 = GText.buffer ~tag_table:tagtable () in
  let abstract2 = 
    GText.view
      ~buffer:bufabs2
      ~packing:scrolledwindow_abs2#add
      ~wrap_mode:`WORD
      ~editable:true
      ()
  in
  let _ = abstract2#misc#modify_font (Lazy.force fontMonospaceMediumPango) in
  let _ = abstract2#buffer#insert "{}" in
  (* Separator 3 *)
  let sep3 = GMisc.separator `VERTICAL ~packing:(view_hbox#pack ~padding:0
		  ~fill:false
		  ~expand:false
	       ) () in
  (* Conc2 *)
  let scrolledwindow_con2 = 
    GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(view_hbox#pack ~padding:0
		  ~fill:true
		  ~expand:true
	       )
      ~show:show
      ()
  in
  let bufcon2 = GText.buffer ~tag_table:tagtable () in
  let concrete2 = 
    GText.view
      ~buffer:bufcon2
      ~packing:scrolledwindow_con2#add
      ~editable:false
      ~wrap_mode:`WORD
      ~cursor_visible:false
      ()
  in
  let _ = concrete2#misc#modify_font (Lazy.force fontMonospaceMediumPango) in
  let _ = concrete2#buffer#insert "{}" in
  let _ =  abstract2#buffer#connect#changed
	     ~callback:callbacks#on_abstract2_changed in
  let _ =  concrete1#buffer#connect#changed
	     ~callback:callbacks#on_concrete1_changed in
object
  inherit GObj.widget_full main_vertical_box#as_widget
  method scrolledwindow_abs1 = scrolledwindow_abs1
  method scrolledwindow_abs2 = scrolledwindow_abs2
  method scrolledwindow_con1 = scrolledwindow_con1
  method scrolledwindow_con2 = scrolledwindow_con2
  method abstract2label = abstract2label
  method concrete2label = concrete2label
  method abstract1 = abstract1
  method concrete2 = concrete2
  method concrete1 = concrete1
  method abstract2 = abstract2
end
