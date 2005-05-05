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
  let change_opt = function
      WrapLen i -> Tidylib_ocaml.WrapLen i
    | ShowWarnings b -> Tidylib_ocaml.ShowWarnings b
    | XmlOut b -> Tidylib_ocaml.XmlOut b
    | XhtmlOut b -> Tidylib_ocaml.XhtmlOut b
    | HtmlOut b -> Tidylib_ocaml.HtmlOut b
    | Quiet b -> Tidylib_ocaml.Quiet b
    | NumEntities b -> Tidylib_ocaml.NumEntities b
    | Mark b -> Tidylib_ocaml.Mark b
  in
  match Tidylib_ocaml.fix_string (Safelist.map change_opt options) s with
    (s, Tidylib_ocaml.Success) -> (s, Success)
  | (s, Tidylib_ocaml.Warning) -> (s, Warning)
