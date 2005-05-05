%{
  open Input
  let parse_error _ =
    raise (Syntax_error "Parse error in command-line directive")
%}
%token <string> STRING
%token SYNC SHOW UPDOWN UPCREATE IDENTIFY TEST CAPABILITIES
%token AS AT
%token COLON SEMICOLON COMMA LPAREN RPAREN LBRACK RBRACK
%token EOF

%start main
%type <Input.t> main

%%

main:
  input EOF                 { $1 }
;

input:
    SYNC replica replica replica_opt as_decl_opt { Sync ($2, $3, $4, $5) }
  | SHOW replica as_decl                         { Show ($2, $3) }
  | UPDOWN replica as_decl                       { Updown ($2, $3) }
  | UPCREATE replica as_decl                     { Upcreate ($2, $3) }
  | IDENTIFY replica                             { Identify ($2) }
  | CAPABILITIES                                 { Capabilities }
  | TEST                                         { Test }
;

/* replica parses one entire replica */
replica:
    path types { Replica ($1, fst $2, snd $2) }
;

/* path parses a Path (string * location) */
path:
    remote_host COLON STRING  { Path($3, $1) }
  | STRING                    { Path($1, Local) }
;

/* remote_host parses a Remote (user option * hostname) */
remote_host:
    STRING AT STRING           { Remote (Some ($1), $3) }
  | STRING                    { Remote (None, $1) }
;

/* types parses a (spec option * view_type option) */
types:
    LPAREN STRING COMMA view_type RPAREN    { (Some ($2), Some ($4)) }
  | LPAREN STRING RPAREN                    { (Some ($2), None) }
  |                                         { (None, None) }
;

/* view_type parses a view_type */
view_type:
    LBRACK str_seq RBRACK                   { $2 }
  | str_seq                                 { $1 }
;

/* str_seq parses a list of strings */
/* precedence here? */
str_seq:
    STRING                                  { [$1] }
  | STRING SEMICOLON                        { [$1] }
  | STRING SEMICOLON str_seq                { $1 :: $3 }
;

/* replica_opt parses a replica option */
replica_opt:
    replica                                 { Some ($1) }
  |                                         { None }
;

/* as_decl_opt parses a view_type option */
as_decl_opt:
    as_decl                                 { Some ($1) }
  |                                         { None }
;

/* as_decl parses "as view_type" */
as_decl:
    AS view_type                            { $2 }
;
