type t
val is_empty : t -> bool
val create : Buffer.t -> in_channel -> t
val t_of_buffer : Buffer.t -> t
val t_of_string : string -> t
val read_char : t -> char option
val append_buff : Buffer.t -> t -> t
val pos_file : t -> int
