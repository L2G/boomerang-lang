
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
exception Type_error of string

let create flds =
  { flds = List.fold_right StrSet.add flds StrSet.empty;
    rows = StrListSet.empty }

let fields r =
  StrSet.elements r.flds

let insert rcd r =
  let cmpfst (x1, y1) (x2, y2) = compare x1 x2 in
  let (clms, ents) = List.split (List.sort cmpfst rcd) in
  if clms <> StrSet.elements r.flds then
    raise (Type_error("insert"));
  {r with rows = StrListSet.add ents r.rows}

let fold f r init =
  let flds = StrSet.elements r.flds in
  let f' row accum =
    let rcd = List.combine flds row in
    f rcd accum
  in
  StrListSet.fold f' r.rows init

let rename m n r =
  if StrSet.mem n r.flds then
    raise (Type_error("rename: field "^n^" already present"));
  if not (StrSet.mem m r.flds) then
    raise (Type_error("rename: field "^m^" not present"));
  let swapname s = if s = m then n else s in
  let flds = List.map swapname (StrSet.elements r.flds) in
  let accum row rel =
    insert (List.combine flds row) rel
  in
  StrListSet.fold accum r.rows (create flds)

let project p r =
  if not (List.for_all (fun x -> StrSet.mem x r.flds) p) then
    raise (Type_error("project: some field not present"));
  let flds = StrSet.elements r.flds in
  let accum row rel =
    let keep (x, y) = List.mem x p in
    insert (List.filter keep (List.combine flds row)) rel
  in
  StrListSet.fold accum r.rows (create p)

let lift_set_op op r1 r2 =
  if not (StrSet.equal r1.flds r2.flds) then
    raise (Type_error("union"));
  {r1 with rows = op r1.rows r2.rows}

let union = lift_set_op StrListSet.union
let inter = lift_set_op StrListSet.inter
let diff = lift_set_op StrListSet.diff

let equal r1 r2 =
  StrSet.equal r1.flds r2.flds && StrListSet.equal r1.rows r2.rows

