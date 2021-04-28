(* User  declarations *)

%%
(* required declarations *)
%name A2

%term
  ID of string | IF | THEN | ELSE | FI | IMPLIES | AND | OR | XOR | EQUALS | NOT | CONST of bool | RPAREN | LPAREN | TERM | NUM of int | PLUS | MINUS | TIMES | LESSTHAN | GREATERTHAN | NEGATE | LET | IN | END | VAR | EQ | FUN | COLON | FN | DEF | ARROW | INT | BOOL | EOF


%nonterm
  program of AST.formula list | statement of AST.formula | START of AST.formula list | EXP of AST.exp | DECL of AST.decl | formula of AST.formula | FUN_DEF of AST.fun_def | FN_DEF of AST.exp | TYPE of AST.typ | APPEXP of AST.exp
%pos int*int	

(*optional declarations *)
%eop EOF
%noshift EOF

(* %right ELSE
%left EQ *)
%right ARROW
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
program: statement program (statement::program) | formula ([formula])
statement: formula TERM (formula)

formula:
	EXP (AST.FormulaExp(EXP))|
	FUN_DEF (AST.FormulaFunDef(FUN_DEF))

FUN_DEF:
	FUN ID LPAREN ID COLON TYPE RPAREN COLON TYPE DEF EXP (AST.Fun(ID1, ID2, AST.ARROW(TYPE1, TYPE2), EXP))

TYPE:
	TYPE ARROW TYPE (AST.ARROW(TYPE1, TYPE2))|
	INT (AST.INT)|
	BOOL (AST.BOOL)|
	LPAREN TYPE RPAREN (TYPE)

DECL: ID EQ EXP (AST.ValDecl(ID, EXP))

APPEXP: 
	LPAREN EXP EXP RPAREN(AST.AppExp(EXP1, EXP2))
	
	
FN_DEF:
	FN LPAREN ID COLON TYPE RPAREN COLON TYPE DEF EXP (AST.Fn(ID, AST.ARROW(TYPE1, TYPE2), EXP))
	

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
	CONST (AST.BoolExp(CONST))|
	APPEXP (APPEXP)|
	FN_DEF (FN_DEF)
	
	
	

  
