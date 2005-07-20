val failwith : string -> 'a

val toplevel :
  (* Name of the current program *)
  string ->              
  (* Qualifier to add to archive names to make them unique *)
  (unit -> string) ->    
  (* Encoding chooser *)
  (string ->
     string *
     'a *     (* ... a user-chosen type describing filetypes, for use below *)
     (string -> string -> unit) option *
     (string -> string -> unit) option) ->
  (* Abstract schema chooser *)
  ('a list -> string) ->
  (* Lens chooser *)
  ('a -> string -> string) ->
  unit
