(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: treeschema.mli 1756 2006-05-31 15:08:03Z bohannon $ *)

module Rel = Db.Relation

let compare_pair c1 c2 (x1, y1) (x2, y2) =
  let d = c1 x1 x2 in
  if d <> 0 then d else c2 y1 y2

let format_name_set ns =
  Format.printf "(";
  Misc.format_list "," Format.print_string (Name.Set.elements ns);
  Format.printf ")"

module Relschema = struct

  module Fd = struct
    type fd = Name.Set.t * Name.Set.t

    let format_fd (src, trg) =
      let p s =
        if Name.Set.cardinal s = 1 then
          Format.print_string (Name.Set.choose s)
        else
          format_name_set s
        in
      p src; Format.printf "->"; p trg

    let names (xs, ys) = Name.Set.union xs ys

    let ranges_over fd ns = Name.Set.subset (names fd) ns

    let rename_att a b (xs, ys) =
      (if Name.Set.mem a xs then Name.Set.add b (Name.Set.remove a xs) else xs),
      (if Name.Set.mem a ys then Name.Set.add b (Name.Set.remove a ys) else ys)

    module NameListMap = Map.Make (
      struct
        type t = Name.t list
        let compare = Pervasives.compare
      end
    )

    exception Fd_conflict

    let assert_bind xs ys fdmap =
      try
        let zs = NameListMap.find xs fdmap in
        if zs = ys then fdmap
        else raise Fd_conflict
      with
      | Not_found -> NameListMap.add xs ys fdmap

    let member r fd =
      let ls1 = Name.Set.elements (fst fd) in
      let ls2 = Name.Set.elements (snd fd) in
      let accum rcd =
        assert_bind
          (Name.Map.list_project ls1 rcd)
          (Name.Map.list_project ls2 rcd)
      in
      (* Yes, this is slightly ugly, in that we are using an exception to
       * communicate non-exceptional information.  However, the
       * "short-circuiting" improves efficiency. *)
      try begin
        ignore (Rel.fold accum r NameListMap.empty);
        true
      end with
      | Fd_conflict -> false

    let revise rcd r (xs, ys) =
      let s =
        Rel.select (Rel.Pred.of_record (Name.Map.project xs rcd)) r
      in
      try
        Name.Map.combine rcd (Name.Map.project ys (Rel.choose s))
      with
      | Not_found -> rcd


    module Ord = struct
      type t = fd
      let compare = compare_pair Name.Set.compare Name.Set.compare
    end

    module Set = struct
      include Set.Make(Ord)

      (* Basic functionality *)

      let format_t fds =
        let iter_with_sep f sep fds =
          let first = ref true in
          iter
            (fun k ->
               if not !first then sep();
               first := false;
               f k)
            fds in
        Format.printf "@[<hv1>{";
        iter_with_sep format_fd (fun() -> Format.printf ",@ ") fds;
        Format.printf "}"

      let names fds =
        fold (fun x -> Name.Set.union (names x)) fds Name.Set.empty

      let ranges_over fds ns = Name.Set.subset (names fds) ns

      let rename_att a b fds =
        fold (fun fd -> add (rename_att a b fd)) fds empty

      let member r fds = for_all (member r) fds

      let outputs fds =
        (* FIXME: This actually implements the function "right"! *)
        List.fold_right
          (fun x -> Name.Set.union (fst x))
          (elements fds)
          Name.Set.empty


      (* Record revision *)

      (* "Nodes" are sets of attributes that appear on one side or the other
       * of a functional dependency.  A set of functional dependencies is a
       * graph over nodes of this sort. *)

      module NodeSet = Set.Make (Name.Set)

      let fd_nodes (fds : t) : NodeSet.t =
        fold (fun (xs, ys) nodes -> NodeSet.add ys (NodeSet.add xs nodes))
          fds NodeSet.empty

      let adj_in (ns : Name.Set.t) (fds : t) : NodeSet.t =
        (* Find all of the incoming adjacent nodes. *)
        fold (fun (xs, _) -> NodeSet.add xs)
          (filter (fun (_, ys) -> ys = ns) fds)
          NodeSet.empty

      (* The first important criteria for tree-form-ness is that all of the
       * distinct nodes in a set of functional dependencies be mutually
       * disjoint.   We will call this the "disjointness" property. *)

      exception Not_disjoint
      (* This exception should be local to the following two functions. *)

      let disjoint_union (xs : Name.Set.t) (ys : Name.Set.t) : Name.Set.t =
        if not (Name.Set.equal (Name.Set.inter xs ys) Name.Set.empty) then
          raise Not_disjoint
        else
          Name.Set.union xs ys

      let mutually_disjoint (nodes : NodeSet.t) : bool =
        try begin
          ignore (NodeSet.fold disjoint_union nodes Name.Set.empty);
          true
        end with
        | Not_disjoint -> false

      (* Another criteria for tree-form-ness is that all of the nodes in a set
       * of functional dependencies have in-degree 0 or 1.  We will call this
       * the "single-parent" property.  Note that this does not rule out
       * cycles. *)

      let single_parent (fds : t) : bool =
        let nodes = fd_nodes fds in
        NodeSet.for_all
          (fun ns -> NodeSet.cardinal (adj_in ns fds) <= 1)
          nodes

      exception Not_tree_form

      let choose_rooted (fds : t) : fd =
        (* Raises Not_found if [fds] is empty. *)
        let rec find_from (x, y) visited =
          if NodeSet.mem x visited then raise Not_tree_form;
          let parent_set = adj_in x fds in
          try
            find_from
              (NodeSet.choose (adj_in x fds), x) (NodeSet.add x visited)
          with
          | Not_found -> (x, y)
        in
        find_from (choose fds) NodeSet.empty

      let revise rcd r fds =
        try begin
          if not (mutually_disjoint (fd_nodes fds)) then raise Not_tree_form;
          if not (single_parent fds) then raise Not_tree_form;
          let rec revise' rcd fds =
            try
              let fd = choose_rooted fds in
              revise' (revise rcd r fd) (remove fd fds)
            with
            | Not_found -> rcd
          in
          revise' rcd fds
        end with
        | Not_tree_form ->
            raise (Error.Harmony_error (fun () ->
              Format.printf "Functional dependencies must be in tree form!"))


      (* Unnecessary until we begin using semantic equality.

      let closure fds = fds   (* WRITE ME! *)
      let minimal fds = fds   (* WRITE ME! *)
      let includes fds1 fds2 = false   (* WRITE ME! *)
      let equiv fds1 fds2 = includes fds1 fds2 && includes fds2 fds1

      *)

    end
  end

  type t = Name.Set.t * Fd.Set.t * Rel.Pred.t

  let format_t (ns, fds, pred) =
    Format.printf "("; Misc.format_list "," Format.print_string (Name.Set.elements ns);
    Format.printf ")";
    if pred <> Rel.Pred.True then begin
      Format.printf "@ where ";
      Rel.Pred.format_t pred
    end; 
    if not (Fd.Set.is_empty fds) then begin
      Format.printf "@ with ";
      Fd.Set.format_t fds
    end

  let create ns = (ns, Fd.Set.empty, Rel.Pred.True)  
  let attributes (ns, fds, pred) = ns
  let rename a b (ns, fds, pred) =
    if not (Name.Set.mem a ns) then
      raise (Error.Harmony_error (fun () ->
        Format.printf "Dbschema.Relschema.rename:@ ";
        format_t (ns, fds, pred);
        Format.printf "@ does not contain the attribute %s" a));
    Name.Set.add b (Name.Set.remove a ns),
    Fd.Set.rename_att a b fds,
    Db.Relation.Pred.rename_att a b pred
  let get_fdset (ns, fds, pred) = fds
  let set_fdset (ns, fds, pred) fds' = (ns, fds', pred)
  let get_pred (ns, fds, pred) = pred
  let set_pred (ns, fds, pred) pred' = (ns, fds, pred')
  let member r (ns, fds, pred) = Fd.Set.member r fds
  let includes (ns1, fds1, pred1) (ns2, fds2, pred2) =
    Name.Set.equal ns1 ns2 &&
    Fd.Set.subset fds2 fds1 &&
    Rel.Pred.includes pred1 pred2
  let equiv t1 t2 = includes t1 t2 && includes t2 t1
end

type t = Relschema.t Name.Map.t

let format_t dbs =
  Format.printf "@[<hv3>{{ ";
  Name.Map.iter_with_sep
    (fun k rel -> 
      Format.printf "@[<hv3>%s" k;
      Relschema.format_t rel;
      Format.printf "@]")
    (fun() -> Format.printf ",@ ")
    dbs;
  Format.printf "@] }}"

let base = Name.Map.empty
let extend = Name.Map.add
let remove = Name.Map.remove
let mem = Name.Map.mem
let lookup rn ds =
  try Name.Map.find rn ds with
  | Not_found ->
      raise (Error.Harmony_error (fun () ->
        Format.printf "Dbschema.lookup:@ ";
        format_t ds;
        Format.printf "@ contains no relation named %s" rn))

let member (d : Db.t) (ds : t) =
  try
    Name.Set.for_all
      (fun n ->
        (Relschema.member (Db.lookup n d) (Name.Map.find n ds)))
      (Name.Map.domain ds)
  with
  | Not_found -> false

let includes ds1 ds2 =
  Name.Set.equal (Name.Map.domain ds1) (Name.Map.domain ds2) &&
  Name.Set.for_all
    (fun n ->
      (Relschema.includes (Name.Map.find n ds1) (Name.Map.find n ds2)))
    (Name.Map.domain ds1)

let equiv ds1 ds2 =
  includes ds1 ds2 && includes ds2 ds1

