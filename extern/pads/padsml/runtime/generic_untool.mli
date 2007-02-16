type 'b error = 
    BadTupleLength of int * 'b
  | BadSumIndex of 'b
  | BadConstraint of 'b

module type S = sig
  type t     
  val processInt : t -> int
  val processFloat : t -> float
  val processChar : t -> char
  val processString : t -> string 
  val processUnit : t -> unit 
  val processRecord : string list -> t -> t list
  val processTuple : t -> t list
  val processDatatype : t -> (string * t)
  val processList : t -> t list
  val scold : t error -> unit
end

module Rec_ver : sig
  type 'a t = { 
      processInt : 'a -> int;
      processFloat : 'a -> float;
      processChar : 'a -> char;
      processString : 'a -> string;
      processUnit : 'a -> unit;
      processRecord : string list -> 'a -> 'a list;
      processTuple : 'a -> 'a list;
      processDatatype : 'a -> (string * 'a);
      processList : 'a -> 'a list;
      scold : 'a error -> unit;
  }

  module From_mod (Untool:S) : sig 
    val untool : Untool.t t
  end
end

