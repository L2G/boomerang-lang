(********************)
(** File selection **)
(********************)

class fileselection =
  let tooltips = GData.tooltips () in
  let accel_group = GtkData.AccelGroup.create () in
  let fileselection = GWindow.file_selection
			 ~title: "Select a file"
			 ~resizable:true
			 ~show_fileops:true
			 ()
  in
  let ok_button = fileselection#ok_button in
  let cancel_button = fileselection#cancel_button in  
object(self)
  method tooltips = tooltips
  method fileselection = fileselection
  method accel_group = accel_group
  method ok_button = ok_button
  method cancel_button = cancel_button
end

let fileselection = ref (new fileselection)
let fileapp = ref (function _ -> ())


(* Call f with the name of the chosen file *)
let select_file bool_ref f =
  fileapp := f;
  let _ =  (!fileselection)#cancel_button#connect#clicked
	     ~callback:(function () -> bool_ref:=true;(!fileselection)#fileselection#misc#hide ()) in
  let _ =  (!fileselection)#ok_button#connect#clicked
	     ~callback:(function () -> 
			  let filename = (!fileselection)#fileselection#filename in 
			  bool_ref:=true;(!fileselection)#fileselection#misc#hide ();
			    !fileapp filename) in
  let _ =  (!fileselection)#fileselection#coerce#misc#connect#destroy
	     ~callback:(function () ->
			  (!fileselection)#fileselection#misc#hide ();
			  fileselection := (new fileselection);
			  bool_ref:=true) in
  bool_ref:=false;
  (!fileselection)#fileselection#misc#show()


(*********************)
(** Error Reporting **)
(*********************)

class top_error =
let tooltips = GData.tooltips () in
let accel_group = GtkData.AccelGroup.create () in
let error = GWindow.window
~title: "Error"
~allow_shrink:false
~allow_grow:true
()
in

let _ = error#add_accel_group accel_group in
let vbox = GPack.vbox
~spacing:10
~homogeneous:false
~packing:error#add
()
in
let error_label = GMisc.label
~text: "Erreur"
~packing:(vbox#pack ~padding:0
~fill:true
~expand:true
)
~xalign:0.5
~yalign:0.5
~xpad:0
~ypad:0
~line_wrap:false
()
in
let hbox = GPack.hbox
~spacing:5
~homogeneous:true
~packing:(vbox#pack ~padding:0
~fill:true
~expand:false
)
()
in
let error_ok = GButton.button
~packing:(hbox#pack ~padding:0
~fill:true
~expand:true
)
~label: "Ok"
()
in
object(self)
  method tooltips = tooltips
  method error = error
  method accel_group = accel_group
  method vbox = vbox
  method hbox = hbox
  method error_ok = error_ok
  method error_label = error_label
end


let top_error = ref (new top_error)

(* Opens a window with the corresponding error message *)
let error bool_ref s =
  let _ =  (!top_error)#error_ok#connect#clicked
	     ~callback:(function () -> bool_ref:=true;(!top_error)#error#misc#hide ()) in
  let _ =  (!top_error)#error#coerce#misc#connect#destroy
	     ~callback:(function () ->
			  (!top_error)#error#misc#hide ();
			  top_error := (new top_error);
			  bool_ref:=true) in
  (!top_error)#error_label#set_text s;
  bool_ref:=false;
  (!top_error)#error#misc#show()


(***********************)
(** Message Reporting **)
(***********************)

class top_message =
let tooltips = GData.tooltips () in
let accel_group = GtkData.AccelGroup.create () in
let message = GWindow.window
~title: "Message"
~allow_shrink:false
~allow_grow:true
()
in

let _ = message#add_accel_group accel_group in
let vbox = GPack.vbox
~spacing:10
~homogeneous:false
~packing:message#add
()
in
let message_label = GMisc.label
~text: "Message"
~packing:(vbox#pack ~padding:0
~fill:true
~expand:true
)
~xalign:0.5
~yalign:0.5
~xpad:0
~ypad:0
~line_wrap:false
()
in
let hbox = GPack.hbox
~spacing:5
~homogeneous:true
~packing:(vbox#pack ~padding:0
~fill:true
~expand:false
)
()
in
let message_ok = GButton.button
~packing:(hbox#pack ~padding:0
~fill:true
~expand:true
)
~label: "Ok"
()
in
object(self)
  method tooltips = tooltips
  method message = message
  method accel_group = accel_group
  method vbox = vbox
  method hbox = hbox
  method message_ok = message_ok
  method message_label = message_label
end


let top_message = ref (new top_message)

(* Opens a window with the corresponding message *)
let message bool_ref s =
  let _ =  (!top_message)#message_ok#connect#clicked
	     ~callback:(function () -> bool_ref:=true;(!top_message)#message#misc#hide ()) in
  let _ =  (!top_message)#message#coerce#misc#connect#destroy
	     ~callback:(function () ->
			  (!top_message)#message#misc#hide ();
			  top_message := (new top_message);
			  bool_ref:=true) in
  (!top_message)#message_label#set_text s;
  bool_ref:=false;
  (!top_message)#message#misc#show()
