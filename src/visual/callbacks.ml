(* THIS IS A GENERATED FILE. DO NOT EDIT.*)

class default_callbacks = 
object(self)

  val mutable focal_window_ = (None : Ui_focal.focal_window option)
  method focal_window =
    match focal_window_ with
      | None -> assert false
      | Some c -> c
  method set_window c = focal_window_ <- Some c

  val mutable trace_window_ = (None : Ui_trace.probe_window option)
  method trace_window =
    match trace_window_ with
      | None -> assert false
      | Some c -> c
  method set_trace_window c = trace_window_ <- Some c
		
  (* Special *)
  method gtk_main_quit () =
    GtkMain.Main.quit ()

  (* Focal Window *)
  method on_open_activate () =
    prerr_endline "Event activate from open";() 
      
  method on_new_activate () =
    prerr_endline "Event activate from new";() 
      
  method on_save_activate () =
    prerr_endline "Event activate from save";() 
      
  method on_edit_activate () =
    prerr_endline "Event activate from edit";() 
      
  method on_see_example_clicked () =
    prerr_endline "Event clicked from see_example";()

  method on_seeprobe_clicked () =
    prerr_endline "Event clicked from see_probe";()

  method on_focal_texteditor_changed () =
    prerr_endline "Event changed from focal_texteditor";() 
      
  method on_about_activate () =
    prerr_endline "Event activate from about";() 

  method compile (force:bool) () =
    prerr_endline "compile";()

  method on_compile_clicked() =
    prerr_endline "Event clicked from compile";()

  method on_interactive_clicked() =
    prerr_endline "Event clicked from interactive";()

  (* Example Window *)

  method on_concrete1_changed () =
    prerr_endline "Event changed from concrete1";() 
      
  method on_abstract2_changed () =
    prerr_endline "Event changed from abstract2";()
    
  method on_initialize_abstract_1_activate () =
    prerr_endline "Event activate from initialize_abstract_1";() 

  method on_openview_activate () =
    prerr_endline "Event activate from openview";() 

  (* Trace window *)

  method select_get_probe (i:int) =
    prerr_endline ("Event select from probe_list: no"^(string_of_int i));()

  method select_put_probe (i:int) =
    prerr_endline ("Event select from probe_list: no"^(string_of_int i));()

  method trace_destroy () =
    prerr_endline "Event destroy from trace_window";()


  (* Option window *)

  method on_rbfull_clicked () =
    prerr_endline "Event clicked on rbfull";()
  method on_rbget_clicked () =
    prerr_endline "Event clicked on rbget";()
  method on_rbcomp_clicked () =
    prerr_endline "Event clicked on rbcomp";()
  method on_rbtype_clicked () =
    prerr_endline "Event clicked on rbtype";()
  method on_rbparse_clicked () =
    prerr_endline "Event clicked on rbparse";()
  method on_rbnothing_clicked () =
    prerr_endline "Event clicked on rbnothing";()

end
