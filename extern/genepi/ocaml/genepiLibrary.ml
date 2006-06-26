
type t

external init : unit -> unit = "GENEPI_init"
external terminate : unit -> unit = "GENEPI_terminate"

external linear_contraint : int list -> int -> t = "GENEPI_linear_constraint"
external intersection : t -> t -> t = "GENEPI_union"
external union : t -> t -> t = "GENEPI_union"
external complement : t -> t = "GENEPI_complement"
external project : t -> int list -> t = "GENEPI_project"
external is_empty : t -> bool = "GENEPI_is_empty"
external print : t -> unit = "GENEPI_print"
