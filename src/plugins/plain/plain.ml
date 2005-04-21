(* Plain.reader : V.reader *)
let reader file = try Some (V.new_value (Misc.read file)) with _ -> None

(* Plain.writer : V.writer *)
let writer vo file =
  match vo with
    None ->
      raise (Misc.Unimplemented
              "[Plain.writer] writing empty view unimplemented")
  | Some v -> Misc.write file (V.get_value v)
