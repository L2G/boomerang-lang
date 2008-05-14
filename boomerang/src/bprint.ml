open Bsyntax

let msg = Util.format

let rec format_sort = function
  | SUnit -> msg "unit"      
  | SBool -> msg "bool"
  | SInteger -> msg "int"      
  | SString -> msg "string"
  | SRegexp -> msg "regexp"
  | SLens -> msg "lens"      
  | SCanonizer -> msg "canonizer"
      
  | SFunction(x0, s1, s2) ->
      msg "@[(";
      if not (Id.equal x0 Id.wild)
      then msg "%s:@," (Id.string_of_t x0);
      format_sort s1;
      msg "@ ->@ ";
      format_sort s2;
      msg ")@]"
        
  | SProduct(s1,s2) -> 
      msg "@[<2>(";
      format_sort s1;
      msg "@ *@ ";
      format_sort s2;
      msg ")@]"

  | SData([],q1) -> msg "@[%s@]" (Qid.string_of_t q1)

  | SData([s],q1) -> 
      msg "@[";
      format_sort s;
      msg "@ ";
      msg "%s@]" (Qid.string_of_t q1)

  | SData(ms,q1) ->         
      msg "@[(@[<2>";
      Misc.format_list ",@ " (format_sort) ms;      
      msg "@])@ %s@]" (Qid.string_of_t q1)

  | SVar(x) -> msg "%s" (Id.string_of_t x)

  | SForall(x,s) -> 
      msg "@[<2>(forall %s@ =>@ " (Id.string_of_t x);
      format_sort s;
      msg ")@]";

  | SRefine(x0,s0,e0) ->
      msg "@[(";
      if not (Id.equal x0 Id.wild)
      then msg "%s:@," (Id.string_of_t x0);
      format_sort s0;
      msg "@ where@ ";
      format_exp e0;
      msg ")@]"

and format_pat p0 = match p0.desc with 
  | PWld -> msg "_"
  | PUnt -> msg "()"
  | PInt(n) -> msg "%d" n
  | PBol(b) -> msg "%b" b
  | PStr(s) -> msg "%s" s
  | PVar(x) -> msg "%s" (Id.string_of_t x)
  | PPar(p1,p2) -> 
      msg "@[<2>(";
      format_pat p1;
      msg ",@,";
      format_pat p2;
      msg ")@]";
  | PVnt(l,None) -> msg "%s" (Qid.string_of_t l)
  | PVnt(l,Some p1) ->  
      msg "@[<2>(%s@ " (Qid.string_of_t l);
      format_pat p1;
      msg ")@]"

and format_param p0 = match p0.desc with
  | Param(x,s) ->
      msg "@[(%s:" (Id.string_of_t x);
      format_sort s;
      msg ")@]"

and format_binding b0 = match b0.desc with
  | Bind (x,so,e) ->
      msg "@[<2>%s" (Id.string_of_t x);
      (match so with None -> () | Some s -> msg "@ :@ "; format_sort s);
      msg "@ =@ ";
      format_exp e;
      msg "@]"
        
and format_exp e0 = match e0.desc with 
  | EApp (e1,e2) ->
	msg "@[<2>(";
	format_exp e1;
	msg "@ ";
	format_exp e2;
	msg ")@]"

    | EVar(q) -> 
	msg "@[%s@]" (Qid.string_of_t q)

    | EOver(op,e1::rest) -> 
        msg "@[<2>(";
        format_exp e1;
        (match rest with 
           | [] -> 
               msg ")";
               format_op op;
               msg "@]"
           | _ -> 
               msg " ";
               format_op op;
               msg " ";
               Misc.format_list "" format_exp rest; (* HACK! *)
               msg ")@]")

    | EOver _ -> assert false
        
    | EFun (p,s,e) ->
	msg "@[<2>(fun@ ";
	format_param p;
	(match s with
	   | None -> ()
           | Some s -> msg "@ :@ "; format_sort s);
	msg "@ ->@ ";
	format_exp e;
	msg ")@]";

    | ELet (b,e) ->
	msg "@[<2>let@ ";
	format_binding b;
	msg "@ in@ ";
	format_exp e;
	msg "@]";

    | ETyFun(x,e) -> 
        msg "@[<2>(tyfun@ %s@ ->@ " (Id.string_of_t x);
        format_exp e;
        msg ")@]"
    
    | ETyApp(e,s) -> 
        msg "@[<2>";
        format_exp e;
        msg "<@["; 
        format_sort s; 
        msg "@]>@]"

    | EUnit -> msg "()"

    | EPair(e1,e2) -> 
        msg "@[<2>(";
        format_exp e1;
        msg ",";
        format_exp e2;
        msg ")@]"

    | ECase(e1,pl,s) -> 
        msg "@[<2>(match@ ";
        format_exp e1;
        msg "@ with@ ";
        Misc.format_list "@ |@ "
          (fun (p,e) -> 
             msg "@[<2>@ ";
             format_pat p;
             msg "@ ->@ ";
             format_exp e;
             msg "@]")
          pl;
        msg ")@ : @ ";
        format_sort s;
        msg "@]"

    | ECast(f,t,_,e) -> 
        msg "@[<2><|"; 
        format_sort t;
        msg "@ <=@ ";
        format_sort f;
        msg "|>@ ";
        format_exp e;
        msg "@]"

    | EBoolean (b) -> 
        msg "@[%b@]" b

    | EString (s) ->
	msg "@[\"%s\"@]" 
          (Bstring.escaped (Bstring.string_of_t s))

    | EInteger (i) ->
	msg "@[%d@]" i

    | ECSet (negated, ranges) ->
	msg "@[[";
	(if negated
	 then msg "^"
	 else ());
	Misc.format_list ""
	  (fun (first, last) ->
	     if Bstring.compare_sym first last = 0
	     then msg "%s" (Bstring.escaped_repr first)
	     else msg "%s-%s" 
	       (Bstring.escaped_repr first)
	       (Bstring.escaped_repr last))
	  ranges;
	  msg "]@]"

and format_op = function
  | OIter(0,-1) -> msg "*"
  | OIter(1,-1) -> msg "+"
  | OIter(0,1)  -> msg "?"
  | OIter(m,-1) -> msg "{%d,}" m
  | OIter(m,n)  -> 
      if m=n then 
        msg "{%d}" m
      else
        msg "{%d,%d}" m n
  | ODot -> msg "."
  | OBar -> msg "|"
  | OTilde -> msg "~"

and format_test_result tr =
  match tr with
    | TestValue e -> 
        msg "= @[<2>" ; 
        format_exp e;
        msg "@]";
    | TestError -> msg "= @[<2>error@]"
    | TestShow -> msg "= @[<2>?@]"
    | TestLensType(e1o,e2o) -> 
        let format_eo = function
          | None -> msg "?"
          | Some e1 -> format_exp e1 in 
        msg ": @[<2>";
        format_eo e1o;
        msg " <-> ";
        format_eo e2o;
        msg "@]"

and format_decl d0 =
  match d0.desc with
      DLet (b) ->
	msg "@[let@ ";
	format_binding b;
	msg "@]"

    | DType(xl,x,cl) ->  
        msg "@[type@ ";
        (match xl with 
          | [] -> ()
          | [x] -> msg "%s" (Id.string_of_t x)
          | _ -> 
              msg "(";
              Misc.format_list ",@ " (fun xi -> msg "%s" (Id.string_of_t xi)) xl;
              msg ")");
        msg "@ %s@ =@ " (Qid.string_of_t x);
        Misc.format_list " | "
          (fun (l,s) -> match s with
             | None -> msg "%s" (Id.string_of_t l)
             | Some s -> 
                 msg "(%s@ " (Id.string_of_t l);
                 format_sort s;
                 msg ")")
          cl;
        msg "@]"        

    | DMod (m, ds) ->
	format_module (mk_mod d0.info (Mod (m, [], ds)))

    | DTest (e, tr) ->
	msg"@[<2>test@ @[";
	format_exp e;
	msg "@ ";
	format_test_result tr;
	msg "@]@]"

and format_module m0 = match m0.desc with 
  | Mod (m, qs, ds) ->
      msg "@[module %s =@\n  @[" (Id.string_of_t m);
      if qs <> [] then 
        Misc.format_list "@\n" 
          (fun x -> msg "open %s" (Id.string_of_t x))
          qs;
      Misc.format_list "@\n" format_decl ds;
      msg "@\n@]@\n@]"

let string_of_sort s = Util.format_to_string (fun () -> format_sort s)
let string_of_param p = Util.format_to_string (fun () -> format_param p)
let string_of_pat p = Util.format_to_string (fun () -> format_pat p)
