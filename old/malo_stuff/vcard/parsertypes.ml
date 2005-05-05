(* Types and translation functions for the vcard parser *)

type param = elt list
and elt = 
    TYPE of string
  | VALUE of string
  | ENCODING of string
  | CHARSET of string
  | LANGUAGE of string
  | XPARAM of (string*string)
      
type value = parts list
and parts =
    VAL of string
  | PART of string
      
let elttoV = function
    TYPE s -> V.set V.empty "TYPE" (Some (V.new_value s))
  | VALUE s -> V.set V.empty "VAL" (Some (V.new_value s))
  | ENCODING s -> V.set V.empty "ENCODING" (Some (V.new_value s))
  | CHARSET s -> V.set V.empty "CHARSET" (Some (V.new_value s))
  | LANGUAGE s -> V.set V.empty "LANGUAGE" (Some (V.new_value s))
  | XPARAM (s1,s2) -> V.set V.empty ("X-"^s1) (Some(V.new_value s2))
      
      
let rec paramtoV = function
    [] -> []
  | a::q -> elttoV a :: (paramtoV q)
      
      
let rec valuetoV = function
    [] -> []
  | [VAL s] -> [V.set V.empty "VAL" (Some (V.new_value s))]
  | (PART s)::q -> (V.set V.empty "VAL" (Some (V.new_value s)))
                    ::(valuetoV q)
  | _ -> []
      
      
let paramvaluetoV (p,v) = 
    V.set (V.set V.empty "PARAM" (Some (V.structure_from_list (paramtoV p))))
      "VALUE" (Some (V.structure_from_list (valuetoV v)))
      
