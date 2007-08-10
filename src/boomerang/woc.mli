type t
val t_of_channel : out_channel -> t
val t_of_buffer : Buffer.t -> t
val create: unit -> t
val write_string : t -> string -> t
val extract_string : t -> string
  (* wirte the content of the second into the first *)
val output_woc : t -> t -> t
