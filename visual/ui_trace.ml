(**********************************)
(** Window for probing and trace **)
(**********************************)

open GText

let fontMonospaceMediumPango = lazy (Pango.Font.from_string "monospace 16")

let tagtable = tag_table ()
let colortag = tag ~name:"color" ()
let _ = colortag#set_property (`BACKGROUND "yellow")
let _ = tagtable#add (colortag#as_tag)

class probe_widget get callbacks ~packing =
(* Main vertical box *)
  let main_vertical_box = 
    GPack.vbox ~spacing:0 ~homogeneous:false ~packing:packing
      ()
  in
(* Label*)
  let title = 
    GMisc.label	~text:(if get then "Get" else "Put")
      ~packing:(main_vertical_box#pack ~fill:true ~expand:false) ()
  in
(* Main panel *)
  let main_panel =
    GPack.paned `HORIZONTAL
      ~packing:(main_vertical_box#pack ~padding:0
		  ~fill:true
		  ~expand:true
	       )
      ()
  in
(* List View *)
  let scrolledwindow_list = 
    GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(main_panel#pack1
	       )
      ()
  in 
  let probe_list =
    GList.liste 
      ~show:true
      ~packing:(scrolledwindow_list#add_with_viewport
	       )
      ()
  in
(* Information display *)
  let scrolledwindow = 
    GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(main_panel#pack2
	       )
      ()
  in 
  let buf = GText.buffer ~tag_table:tagtable () in
  let view = 
    GText.view
      ~buffer:buf
      ~packing:scrolledwindow#add
      ~editable:false
      ~wrap_mode:`WORD
      ~cursor_visible:false
      ()
  in
(* Callbacks *)
  let _ = probe_list#connect#select_child
	    ~callback:(function li -> 
			 (if get then callbacks#select_get_probe (probe_list#child_position li)
			  else callbacks#select_put_probe (probe_list#child_position li))) in
object(self)
  method probe_list = probe_list
  method buffer = buf

  val mutable counter = 0
  method init ()= counter <- 0
  method increase ()= counter<-counter+1;string_of_int counter
  method add_probe n =
    probe_list#add (GList.list_item ~label:((self#increase ())^": "^n) ())
end
  
class probe_window callbacks =
  let tooltips = GData.tooltips () in
  let accel_group = GtkData.AccelGroup.create () in
  let window = GWindow.window
		 ~title:"Probes"
		 ~resizable:true
		 ~allow_grow:true
		 ~allow_shrink:true
		 ~height:450
		 ~width:600 
		 () in
  let _ = window#add_accel_group accel_group in
  let main_horizontal_box = 
    GPack.hbox ~spacing:5 ~homogeneous:true ~packing:window#add
      ()
  in
  let get_probe = 
    new probe_widget true callbacks 
      ~packing:(main_horizontal_box#pack ~padding:0
		  ~fill:true
		  ~expand:true)
  in
  let put_probe = 
    new probe_widget false callbacks 
      ~packing:(main_horizontal_box#pack ~padding:0
		  ~fill:true
		  ~expand:true)
  in
  let _ =  window#coerce#misc#connect#destroy
	     ~callback:(callbacks#trace_destroy) in
object(self)
  
  method window = window
  method get_probe = get_probe
  method put_probe = put_probe

  initializer callbacks#set_trace_window self
    
end



(* Not used any more *)
class trace_window get callbacks =
  let tooltips = GData.tooltips () in
  let accel_group = GtkData.AccelGroup.create () in
  let window = GWindow.window
		 ~title:(if get then "Get probes" else "Put probes")
		 ~resizable:true
		 ~allow_grow:true
		 ~allow_shrink:true
		 ~height:450
		 ~width:300 
		 ()
  in
  let _ = window#add_accel_group accel_group in
(* Main vertical box *)
  let main_vertical_box = 
    GPack.vbox ~spacing:0 ~homogeneous:false ~packing:window#add
      ()
  in
(* Main panel *)
  let main_panel =
    GPack.paned `HORIZONTAL
      ~packing:(main_vertical_box#pack ~padding:0
		  ~fill:true
		  ~expand:true
	       )
      ()
  in

(* List View *)
  let scrolledwindow_list = 
    GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(main_panel#pack1
	       )
      ()
  in 
  let probe_list =
    GList.liste 
      ~show:true
      ~packing:(scrolledwindow_list#add_with_viewport
	       )
      ()
  in
(* Information display *)
  let scrolledwindow = 
    GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(main_panel#pack2
	       )
      ()
  in 
  let buf = GText.buffer ~tag_table:tagtable () in
  let view = 
    GText.view
      ~buffer:buf
      ~packing:scrolledwindow#add
      ~editable:false
      ~wrap_mode:`WORD
      ~cursor_visible:false
      ()
  in

(* Callbacks *)
  let _ = probe_list#connect#select_child
	    ~callback:(function li -> 
			 (if get then callbacks#select_get_probe (probe_list#child_position li)
			  else callbacks#select_put_probe (probe_list#child_position li))) in
  let _ =  window#coerce#misc#connect#destroy
	     ~callback:(if get then callbacks#trace_get_destroy else callbacks#trace_put_destroy) in


object(self)
  
  method window = window
  method probe_list = probe_list
  method buffer = buf

  val mutable counter = 0
  method init ()= counter <- 0
  method increase ()= counter<-counter+1;string_of_int counter

  method add_probe n =
    probe_list#add (GList.list_item ~label:((self#increase ())^": "^n) ())

  initializer 
    if get then callbacks#set_trace_get_window self
    else callbacks#set_trace_put_window self

end
