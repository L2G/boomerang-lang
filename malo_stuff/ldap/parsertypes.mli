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
  
val paramvaluetoV : param * value -> V.t
