type global_state = Tree.t
type state = global_state
exception Tool_error of state * string
let init _ = ()

module type BaseType = sig
  type t 
  val conv : t -> string
end

module MkBaseType(B:BaseType) = struct
  type state = global_state
  type t = B.t
  let init () = Tree.empty
  let process t res _ = match res with
      Pads.Ok b -> Tree.set Tree.empty (B.conv b) (Some t)
    | Pads.Error -> assert false        
end      
  
module Int = MkBaseType(struct 
  type t = int 
  let conv = string_of_int
end)

module Float = MkBaseType(struct 
  type t = float 
  let conv = string_of_float
end)
module Char = MkBaseType(struct 
  type t = char 
  let conv c = String.make 1 c
end)
module String = MkBaseType(struct 
  type t = string 
  let conv s = s
end)
module Unit = MkBaseType(struct 
  type t = unit 
  let conv () = "()"
end)

module Record = struct
  type partial_state = global_state
  let init _ = Tree.empty
  let start t _ = t
  let project t k = Tree.empty
  let process_field t k tk = Tree.set t k (Some tk)
  let process_last_field = process_field
end

module Datatype = struct
  type partial_state = global_state
  let init () = Tree.empty
  let start t _ = t
  let project t k = Tree.get t k
  let process_variant t k tk = Tree.set t k (Some tk)    
  module Empty = struct
    let init () = Tree.empty
    let process t = t
  end
end

module Constraint = struct
  type partial_state = global_state
  let init _ = Tree.empty
  let start t _ = t
  let project t = t
  let process t _ = t
end

module List = struct
  type partial_state = global_state list
  let init () = Tree.empty_list
  let start t _ = []
  let project_next t = (t,None)
  let process_next t ti = ti::t
  let process_last tl t = Tree.structure_from_list (List.rev (t::tl))
  let process_empty _ = Tree.empty_list
end
