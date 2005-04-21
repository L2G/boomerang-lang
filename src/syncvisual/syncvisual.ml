let main () =
  let callbacks = new Callbacks.callbacks in
  let widgets = new Widgets.widgets callbacks in
  let _ = widgets#window#show() in    
    GMain.Main.main ()
   	
let _ = Printexc.print main ()

