type t
val buff_size : unit -> int
val new_t : Buffer.t -> in_channel -> t
val read_char : t -> char
val append_buff : Buffer.t -> t -> t
