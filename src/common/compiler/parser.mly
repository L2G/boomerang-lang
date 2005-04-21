/* parser.mly - Focal parser generator */

%{

open Error
open Syntax

let error t info =
  let (l,c1),(_,c2) = info in
  let s = Printf.sprintf "%d:%d-%d" l c1 c2 in
    if t = "" then raise (Parse_error (s,info))
    else raise (Parse_error (s^ ": " ^ t,info))
      
%}

%token <Error.info> Eof 
%token <Error.info> Equal Semi Comma Arr Fun
%token <Error.info> Lbrace Rbrace Lbrack Rbrack Lparen Rparen Langle Rangle
%token <Error.info> Let Do In Rec And
%token <string * Error.info> Ident String
%token <Error.info> Type Empty Bang Star Dot Amp Bar QMark Minus Slash

%start prog view ptyp pdefs
%type <Syntax.prog> prog
%type <Syntax.expr> view
%type <Syntax.ptyp> ptyp
%type <Syntax.pdef list> pdefs

%nonassoc below_Semi
%left Semi

/* precedence for infix type constructors */
%right Minus              /* lowest precedence */
%right Amp                /* medium-low precedence */
%right Bar                /* medium-high precedence */
%right Dot                /* highest precedence */

%%

prog:
| seq Eof                        { ([],$1) }
| def Do seq Eof                 { ($1,$3) }
| def Do Eof                     { error "Missing sequence after 'do'" $2 }
| def Do error                   { error "Incorrect sequence after 'do'" $2 }
| error                          { error "Incorrect beginning of the program" bogusInfo }
| Eof                            { error "empty program" bogusInfo }

def: 
| Let ident ident_list Equal seq def           { (Deflet($2,$3,$5,merge_inc $1 (getInfo $5)))::($6) }
| Let Rec ident Equal seq defrec               { (Defletrec(($3,[],$5,merge_inc $1 (getInfo $5))::(fst $6))::(snd $6)) }
| Let ident ident_list Equal seq error         { error "Missing 'do' after the definition" (merge_inc $1 (getInfo $5)) }
| Let ident ident_list Equal error             { error "Incorrect expression after '='" (merge_inc $1 $4) }
| Let error                                    { error "Expecting an identifier and a '=' for a definition" ($1) }
| Let Rec ident Equal seq error                { error "Missing 'do' after the definition" (merge_inc $1 (getInfo $5)) }
| Let Rec ident Equal error                    { error "Incorrect expression after '='" (merge_inc $1 $4) }
| Let Rec ident ident error                    { error "The recursive definition doesn't accept parameters" (merge_inc $1 $2) }
| Let Rec ident error                          { error "Expecting a '=' after the identifier" (merge_inc $1 $2) }
| Let Rec error                                { error "Expecting an identifier and a '=' for a definition" (merge_inc $1 $2) }
| Rec                                          { error "Expecting a 'let' before the 'rec'" ($1) }
| And                                          { error "Expecting a recursive definition before 'and'" ($1) }
|                                              { [] }

defrec:
| Let ident ident_list Equal seq def           { ([],(Deflet($2,$3,$5,merge_inc $1 (getInfo $5)))::($6)) }
| And ident Equal seq defrec                   { ((fst $5)@[$2,[],$4,merge_inc $1 (getInfo $4)],snd $5) }
| Let Rec ident Equal seq defrec               { ([],(Defletrec(($3,[],$5,merge_inc $1 (getInfo $5))::(fst $6)))::(snd $6)) }
| Let ident ident_list Equal seq error         { error "Missing 'do' after the definition" (merge_inc $1 (getInfo $5)) }
| Let ident ident_list Equal error             { error "Incorrect expression after '='" (merge_inc $1 $4) }
| Let error                                    { error "Expecting an identifier and a '=' for a definition" ($1) }
| And ident Equal seq error                    { error "Missing 'do' after the definition" (merge_inc $1 (getInfo $4)) }
| And ident Equal error                        { error "Incorrect expression after '='" (merge_inc $1 $3) }
| And ident ident error                        { error "The recursive definition doesn't accept parameters" ($1) }
| And ident error                              { error "Expecting a '=' after the identifier" ($1) }
| And error                                    { error "Expecting an identifier and a '=' for a definition" ($1) }
| Let Rec ident Equal seq error                { error "Missing 'do' after the definition" (merge_inc $1 (getInfo $5)) }
| Let Rec ident Equal error                    { error "Incorrect expression after '='" (merge_inc $1 $4) }
| Let Rec ident ident error                    { error "The recursive definition doesn't accept parameters" (merge_inc $1 $2) }
| Let Rec ident error                          { error "Expecting a '=' after the identifier" (merge_inc $1 $2) }
| Let Rec error                                { error "Expecting an identifier and a '=' for a definition" (merge_inc $1 $2) }
| Rec                                          { error "Expecting a 'let' before the 'rec'" ($1) }
|                                              { ([],[]) }

/*** expressions ***/
expr:
| uexpr                               { $1 }
| pexpr                               { $1 }

/*** unparenthesized expressions ***/
uexpr:
| simpleExpr                          { $1 }
| expr pexpr                          { AstApp ($1, $2, merge_inc (getInfo $1) (getInfo $2)) } 
| expr simpleExpr                     { AstApp ($1, $2, merge_inc (getInfo $1) (getInfo $2)) } 

/*** parenthesized expressions ***/
pexpr:
| Lparen seq Rparen                   { setInfo (merge_inc $1 $3) $2 }
| Lparen error                        { error "Unmatched '('" ($1) }

/*** simple (non-App) expressions ***/
simpleExpr:
| String                              { let (s,i) = $1 in AstName (s,i) }
| Ident                               { let (s,i) = $1 in AstVar (s,i) }
| view                                { $1 }
| map                                 { $1 }

/*** sugar for sequences ***/
seq:
| Let ident ident_list Equal seq In seq   { AstLet ($2,$3,$5,$7,merge_inc $1 (getInfo $7)) }
| Let Rec ident Equal seq seqletrec       { AstLetrec ([$3,[],$5,merge_inc $1 (getInfo $5)]@(fst $6),snd $6,merge_inc $1 (getInfo (snd $6)))  }
| Fun Ident Arr seq                     { AstFun(fst $2,$4,merge_inc $1 (getInfo $4)) }
| expr %prec below_Semi                   { $1 }
| expr Semi seq                           { AstApp(AstApp(AstVar("compose2",$2),$1,merge_inc (getInfo $1) $2),$3,(merge_inc (getInfo $1) (getInfo $3)))}
| Fun Ident Arr error                     { error "Incorrect expression after '->'" (merge_inc $1 $3) } 
/*
| Fun Ident Rangle                        { error "Ambiguous '>' after the identifier: please correct into ' ->'" (merge_inc (snd $2) $3) } 
*/
| Fun Ident error                         { error "Expecting an '->' after the identifier" (merge_inc $1 (snd $2)) }
| Fun error                               { error "Expecting an identifier and a '->'" ($1) }
| Let ident ident_list Equal seq In error { error "Incorrect sequence after 'in'" (merge_inc $1 $6) }
| Let ident ident_list Equal seq error    { error "Missing 'in' after the definition" (merge_inc $1 (getInfo $5)) }
| Let ident ident_list Equal error        { error "Incorrect expression after '='" (merge_inc $1 $4) }
| Let error                               { error "Expecting an identifier and a '=' for a definition" ($1) }
| Let Rec ident Equal seq error           { error "Missing 'in' after the definition" (merge_inc $1 (getInfo $5)) }
| Let Rec ident Equal error               { error "Incorrect sequence after '='" (merge_inc $1 $4) }
| Let Rec ident ident error               { error "The recursive definition doesn't accept parameters" (merge_inc $1 $2) }
| Let Rec ident error                     { error "Expecting a '=' after the identifier" (merge_inc $1 $2) }
| Let Rec error                           { error "Expecting an identifier and a '=' for a definition" (merge_inc $1 $2) }
| Rec                                     { error "Expecting a 'let' before the 'rec'" ($1) }
| And                                     { error "Expecting a recursive definition before 'and'" ($1) }
| expr Semi error                         { error "Expecting a lens expression after ';'" ($2) }
| Semi error                              { error "Expecting a lens expression before ';'" ($1) }

seqletrec:
| And ident Equal seq seqletrec     { ([$2,[],$4,merge_inc $1 (getInfo $4)]@(fst $5),snd $5) }
| In seq                            { ([],$2) }
| And ident Equal seq error         { error "Missing 'in' after the definition" (merge_inc $1 (getInfo $4)) }
| And ident Equal error             { error "Incorrect expression after '='" (merge_inc $1 $3) }
| And ident ident error             { error "The recursive definition doesn't accept parameters" ($1) }
| And ident error                   { error "Expecting a '=' after the identifier" ($1) }
| And error                         { error "Expecting an identifier and a '=' for a definition" ($1) }
| Let error                         { error "Expecting an 'in' before another definition" ($1) }
| Fun error                         { error "Expecting an 'in' before a function declaration" ($1) }
| In error                          { error "Incorrect expression after 'in'" ($1) }

/*** maps ***/
map:
| Langle mapBody Rangle               { AstMap ($2, merge_inc $1 $3) }
| Langle error                        { error "Unmatched '<'" ($1) }

mapBody:
| string Arr expr Comma mapBody        { (fst $1,$3)::$5 }
| string Arr expr mapBody              { (fst $1,$3)::$4 }
|                                        { [] }
| string Arr expr Comma error          { error "Incorrect expression after ','" (merge_inc (snd $1) $4) }
| string Arr expr Semi error           { error "This sequence has to be inside parenthesis" (merge_inc (snd $1) $4) }
| string Arr error                     { error "Incorrect expression after '->'" (merge_inc (snd $1) $2) }
| string error                         { error "Expecting a '->' after the identifier" (snd $1) }


/*** views ***/
view:
| Lbrace viewBody Rbrace                 { AstView ($2, "", false, merge_inc $1 $3) }
| Lbrack viewinside_list Rbrack          { AstView ($2, "",true, merge_inc $1 $3) }
| Lbrace error                           { error "Unmatched '{'" ($1) }
| Lbrack error                           { error "Unmatched '['" ($1) }
| Rbrack error                           { error "Unexpected ']'" ($1) }
| Rbrace error                           { error "Unexpected '}'" ($1) }

viewinside:
| Ident                                  { let (s,i) = $1 in AstVar (s,i) }
| Lbrace viewBody Rbrace                 { AstView ($2, "", false, merge_inc $1 $3) }
| Lbrack viewinside_list Rbrack          { AstView ($2, "",true, merge_inc $1 $3) }
| Lbrace error                           { error "Unmatched '{'" ($1) }
| Lbrack error                           { error "Unmatched '['" ($1) }
| Equal error                            { error "Unexpected '='" ($1) }
/*
| Rbrack error                           { error "Unexpected ']'" ($1) }
| Rbrace error                           { error "Unexpected '}'" ($1) }
*/

viewBody:
| string Equal viewinside Comma viewBody { (fst $1, $3)::$5 }
| string Equal viewinside viewBody       { (fst $1, $3)::$4 }
| string Equal String Comma viewBody     {let (s,i) = $3 in (fst $1, AstView([(AstName (s,i), emptyView (snd $3))], "", false,i))::$5 }
| string Equal String viewBody           {let (s,i) = $3 in (fst $1, AstView([(AstName (s,i), emptyView (snd $3))], "", false,i))::$4 }
| string Comma viewBody                  { (fst $1, emptyView (snd $1))::$3 }
| string viewBody                        { (fst $1, emptyView (snd $1))::$2 }
| string Equal viewinside Comma error    { error "Expecting an end to the view description (1)" (merge_inc (snd $1) $4) }
| string Equal viewinside error          { error "Expecting an end to the view description (2)" (merge_inc (snd $1) (getInfo $3)) }
| string Equal String Comma error        { error "Expecting an end to the view description (3)" (merge_inc (snd $1) $4) }
| string Equal String error              { error "Expecting an end to the view description (4)" (merge_inc (snd $1) (snd $3)) }
| string Equal error                     { error "Expecting a view after '='" (merge_inc (snd $1) $2) }
| string error                           { error "Expecting an end to the view description (5)" (snd $1) }
|                                        { [] }

viewinside_list:
| viewinside viewinside_list         { (AstName("",bogusInfo),$1)::$2 }
| viewinside Comma viewinside_list   { (AstName("",bogusInfo),$1)::$3 }
|                                    { [] }

/*** strings & ident ***/
string:
| Ident                               { let (s,i) = $1 in AstVar (s,i),i }
| String                              { let (s,i) = $1 in AstName (s,i),i }

ident:
| Ident                               { let (s,_) = $1 in s }

ident_list:
| ident ident_list                    { $1::$2 }
|                                     { [] }

/* TYPES */
pdefs:
  | Eof                                 { [] }
  | Type Ident Equal ptyp pdefs         { let (x,i) = $2 in (x,i,$4)::$5 }

ptyp:
  | Empty                               { PTEmpty($1) }
  | Lbrace Rbrace                       { PTEmptyView($1) }
  | Ident                               { let (x,i) = $1 in PTVar(x,i) }
  | Bang omits opt Lbrack ptyp Rbrack   { PTAny($3,$2,$5,$1) }
  | Star omits opt Lbrack ptyp Rbrack   { PTAll($3,$2,$5,$1) }
  | Ident opt Lbrack ptyp Rbrack        { let (x,i) = $1 in PTName($2,x,$4,i) }
  | Lparen ptyp Rparen                  { $2 }
  | ptyp Dot ptyp                       { PTCat($1,$3,$2) }
  | ptyp Amp ptyp                       { PTInter($1,$3,$2) }
  | ptyp Bar ptyp                       { PTUnion($1,$3,$2) }
  | ptyp Minus ptyp                     { PTDiff($1,$3,$2) }
  | Eof                                 { error "type expected, found EOF" $1 }

opt:
  |                                     { false }
  | QMark                               { true }

omits:
  |                                     { [] }
  | Slash Lbrace omit2 Rbrace           { $3 }

omit2:
  |                                     { [] }
  | Ident omit2                         { let (x,i) = $1 in x::$2 }
  | Ident Comma omit2                   { let (x,i) = $1 in x::$3 }
