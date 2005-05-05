%{

  open Lexing
    
  let error t info =
    let l,c1,c2 = info in
    let s = Printf.sprintf "%d:%d-%d" l c1 c2 in
    if t = "" then failwith s
    else failwith (s ^ ": " ^ t)

%}

%token <(int*int*int)>EOF
%token <string*string> KWORD
%token <string*(int*int*int)> VERSION
%token <(string*V.t) list*(int*int*int)> DNVAL


%type <V.t> ldap

%start ldap

%%

  ldap :
  | VERSION entlist EOF    { V.set (V.set V.empty "VERSION" (Some (V.new_value (fst $1))))
			       "LDAP" (Some (V.structure_from_list $2)) }
  | entlist EOF            { V.set (V.set V.empty "VERSION" (Some (V.new_value "1")))
			       "LDAP" (Some (V.structure_from_list $1)) }
  | error                  { error "Pb2" (1,1,1) }
  ;

  entlist :
  | DNVAL kwlist entlist      { [V.set V.empty "DN" (Some ( V.set (V.set V.empty "DATA" (Some (V.structure_from_list $2)))
							  "KEY" (Some (V.from_list (fst $1)))
						 ))] @ $3 }
  | error                  { error "Pb1" (1,1,1) }
  |                        { [] }
  ;

  kwlist :
  | KWORD kwlist           { [V.set V.empty (fst $1) (Some (V.new_value (snd $1)))]@$2 }
  | error                  { error "Pb3" (1,1,1) }
  |                        { [] }
  ;

