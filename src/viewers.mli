(** Viewers -- readers and writers for various concrete file formats

  The interface of this module is empty because its contents are not called statically.
  Instead, at startup time, it dynamically registers a number of viewers with the
  [Surveyor], which dispatches to them as needed when it is asked to load and dump
  files. 
  *)

val init : unit -> unit
