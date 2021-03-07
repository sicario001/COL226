(* User  declarations *)

%%
(* required declarations *)
%name Calc

%term
  ID of string | IF | THEN | ELSE | IMPLIES | AND | OR | XOR | EQUALS | NOT | TRUE | FALSE | RPAREN | LPAREN | TERM | EOF

%nonterm
  program | statement | START | formula | formula_without_ite | formula_without_imp | formula_without_binop | formula_ID_CONST | CONST

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

(* %left SUB PLUS *)
(* %left TIMES DIV *)
(* %right *)
(* %nonassoc*)
%start START

%verbose

%%

START: program ()
program: statement program ()
statement: formula TERM ()
  
formula: IF formula THEN formula ELSE formula ()
  | formula_without_ite ()
  | LPAREN formula RPAREN ()

formula_without_ite: formula_without_imp IMPLIES formula_without_ite ()
  | formula_without_imp ()
  | LPAREN formula RPAREN ()

formula_without_imp: formula_without_imp binop formula_without_binop ()
  | formula_without_binop ()
  | LPAREN formula RPAREN ()

formula_without_binop: NOT formula_without_binop ()
  | formula_ID_CONST ()
  | LPAREN formula RPAREN ()

formula_ID_CONST: ID ()
  | CONST ()

CONST: TRUE ()
  | FALSE ()




  
