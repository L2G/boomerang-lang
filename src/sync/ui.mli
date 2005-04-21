(* User Interface requirements *)

val choose_encoding : string -> string list -> string
(** [choose_encoding file encs] chooses one of [encs] to be used
  as the encoding of [file]. *)

val choose_vt : (Optometrist.type_desc) option
             -> (Optometrist.type_desc) list
             -> Optometrist.type_desc
(** [choose_vt vts] chooses one of [vts] to be used as the abstract view
    type for synchronization. *)

val massage_action : Sync.action -> Sync.action
(** [massage_action a] optionally makes modifications to the action list
    before they actually get propagated through the views. *)
