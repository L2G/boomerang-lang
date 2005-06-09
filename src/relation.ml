
module StrSet = Set.Make (String)

module StrListSet = Set.Make (
  struct
    type t = string list
    let compare = Pervasives.compare
  end
)

type t = {
  flds : StrSet.t;
  rows : StrListSet.t
}
type record = (string * string) list

exception Unequal_domains of string list * string list
exception Domain_excludes of string list * string
exception Domain_includes of string list * string

let create flds =
  { flds = List.fold_right StrSet.add flds StrSet.empty;
    rows = StrListSet.empty }

let fields r =
  StrSet.elements r.flds

let insert rcd r =
  let cmpfst (x1, y1) (x2, y2) = compare x1 x2 in
  let (clms, ents) = List.split (List.sort cmpfst rcd) in
  if clms <> StrSet.elements r.flds then
    raise (Unequal_domains(clms, StrSet.elements r.flds));
  {r with rows = StrListSet.add ents r.rows}

let fold f r init =
  let flds = StrSet.elements r.flds in
  let f' row accum =
    let rcd = List.combine flds row in
    f rcd accum
  in
  StrListSet.fold f' r.rows init

let rename m n r =
  if not (StrSet.mem m r.flds) then
    raise (Domain_excludes(StrSet.elements r.flds, m));
  if StrSet.mem n r.flds && n <> m then
    raise (Domain_includes(StrSet.elements r.flds, n));
  let swapname s = if s = m then n else s in
  let flds = List.map swapname (StrSet.elements r.flds) in
  let accum row rel =
    insert (List.combine flds row) rel
  in
  StrListSet.fold accum r.rows (create flds)

let project p r =
  try
    let fld = List.find (fun x -> not (StrSet.mem x r.flds)) p in
    raise (Domain_excludes(StrSet.elements r.flds, fld))
  with
  | Not_found -> (* This means everything is OK.  Proceed as normal. *)
      let flds = StrSet.elements r.flds in
      let accum row rel =
        let keep (x, y) = List.mem x p in
        insert (List.filter keep (List.combine flds row)) rel
      in
      StrListSet.fold accum r.rows (create p)

let lift_set_op op r1 r2 =
  if not (StrSet.equal r1.flds r2.flds) then
    raise (Unequal_domains(StrSet.elements r1.flds, StrSet.elements r2.flds));
  {r1 with rows = op r1.rows r2.rows}

let union = lift_set_op StrListSet.union
let inter = lift_set_op StrListSet.inter
let diff = lift_set_op StrListSet.diff

let equal r1 r2 =
  StrSet.equal r1.flds r2.flds && StrListSet.equal r1.rows r2.rows

