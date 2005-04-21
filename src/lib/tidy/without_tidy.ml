type status = Success | Warning

type tidy_option =
  | WrapLen of int (** Wrap margin *)
  | ShowWarnings of bool (** however errors are always shown *)
  | XmlOut of bool (** create output as XML *)
  | XhtmlOut of bool (** output extensible HTML *)
  | HtmlOut of bool (** output plain HTML, even for XHTML input.  Yes means set
                        explicitly *)
  | Quiet of bool (** no 'Parsing X', guessed DTD or summary *)
  | NumEntities of bool (** Use numeric entities *)
  | Mark of bool (** Add meta element indicating tidied doc *)

let fix_string options s =
  raise (Misc.Unimplemented "Tidy not supported in this build.")
