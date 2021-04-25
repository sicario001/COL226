(* User  declarations *)

%%
(* required declarations *)
%name A2

%term
  ID of string | IF | THEN | ELSE | FI | IMPLIES | AND | OR | XOR | EQUALS | NOT | CONST of bool | RPAREN | LPAREN | TERM | NUM of int | PLUS | MINUS | TIMES | LESSTHAN | GREATERTHAN | NEGATE | LET | IN | END | VAR | EQ | EOF


%nonterm
  program of AST.exp list | statement of AST.exp | START of AST.exp list | EXP of AST.exp | DECL of AST.decl
%pos int*int	

(*optional declarations *)
%eop EOF
%noshift EOF

(* %right ELSE
%left EQ *)
%right IMPLIES
%left AND OR XOR EQUALS
%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%right NOT NEGATE


%start START

%verbose

%%

START: program (program) | ([])
program: statement program (statement::program) | EXP ([EXP])
statement: EXP TERM (EXP)
  
DECL: ID EQ EXP (AST.ValDecl(ID, EXP))
EXP: 
	IF EXP THEN EXP ELSE EXP FI (AST.IteExp(EXP1, EXP2, EXP3))|
	EXP PLUS EXP (AST.BinExp(AST.Plus, EXP1, EXP2))|
	EXP MINUS EXP (AST.BinExp(AST.Minus, EXP1, EXP2))|
	EXP TIMES EXP (AST.BinExp(AST.Times, EXP1, EXP2))|
	EXP GREATERTHAN EXP (AST.BinExp(AST.GreaterThan, EXP1, EXP2))|
	EXP LESSTHAN EXP (AST.BinExp(AST.LessThan, EXP1, EXP2))|
	NEGATE EXP (AST.UnExp(AST.Negate, EXP))|
	EXP AND EXP (AST.BinExp(AST.And, EXP1, EXP2))|
	EXP OR EXP (AST.BinExp(AST.Or, EXP1, EXP2))|
	EXP XOR EXP (AST.BinExp(AST.Xor, EXP1, EXP2))|
	EXP EQUALS EXP (AST.BinExp(AST.Equals, EXP1, EXP2))|
	EXP IMPLIES EXP (AST.BinExp(AST.Implies, EXP1, EXP2))|
	NOT EXP (AST.UnExp(AST.Not, EXP))|
	LET DECL IN EXP END (AST.LetExp(DECL, EXP))|
	LPAREN EXP RPAREN (EXP)|
	ID (AST.VarExp(ID))|
	NUM (AST.NumExp(NUM))|
	CONST (AST.BoolExp(CONST))
	
	
	

  
