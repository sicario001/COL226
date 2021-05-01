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
	EXP (AST.FormulaExp(EXP, EXPleft, EXPright))|
	FUN_DEF (AST.FormulaFunDef(FUN_DEF))

FUN_DEF:
	FUN ID LPAREN ID COLON TYPE RPAREN COLON TYPE DEF EXP (AST.Fun(ID1, ID2, AST.ARROW(TYPE1, TYPE2), EXP, FUNleft, EXPright))

TYPE:
	TYPE ARROW TYPE (AST.ARROW(TYPE1, TYPE2))|
	INT (AST.INT)|
	BOOL (AST.BOOL)|
	LPAREN TYPE RPAREN (TYPE)

DECL: ID EQ EXP (AST.ValDecl(ID, EXP))

APPEXP: 
	LPAREN EXP EXP RPAREN(AST.AppExp(EXP1, EXP2, LPARENleft, RPARENright))
	
	
FN_DEF:
	FN LPAREN ID COLON TYPE RPAREN COLON TYPE DEF EXP (AST.Fn(ID, AST.ARROW(TYPE1, TYPE2), EXP, FNleft, EXPright))
	

EXP: 
	IF EXP THEN EXP ELSE EXP FI (AST.IteExp(EXP1, EXP2, EXP3, IFleft, FIright))|
	EXP PLUS EXP (AST.BinExp(AST.Plus, EXP1, EXP2, EXP1left, EXP2right))|
	EXP MINUS EXP (AST.BinExp(AST.Minus, EXP1, EXP2, EXP1left, EXP2right))|
	EXP TIMES EXP (AST.BinExp(AST.Times, EXP1, EXP2, EXP1left, EXP2right))|
	EXP GREATERTHAN EXP (AST.BinExp(AST.GreaterThan, EXP1, EXP2, EXP1left, EXP2right))|
	EXP LESSTHAN EXP (AST.BinExp(AST.LessThan, EXP1, EXP2, EXP1left, EXP2right))|
	NEGATE EXP (AST.UnExp(AST.Negate, EXP, NEGATEleft, EXPright))|
	EXP AND EXP (AST.BinExp(AST.And, EXP1, EXP2, EXP1left, EXP2right))|
	EXP OR EXP (AST.BinExp(AST.Or, EXP1, EXP2, EXP1left, EXP2right))|
	EXP XOR EXP (AST.BinExp(AST.Xor, EXP1, EXP2, EXP1left, EXP2right))|
	EXP EQUALS EXP (AST.BinExp(AST.Equals, EXP1, EXP2, EXP1left, EXP2right))|
	EXP IMPLIES EXP (AST.BinExp(AST.Implies, EXP1, EXP2, EXP1left, EXP2right))|
	NOT EXP (AST.UnExp(AST.Not, EXP, NOTleft, EXPright))|
	LET DECL IN EXP END (AST.LetExp(DECL, EXP, LETleft, ENDright))|
	LPAREN EXP RPAREN (EXP)|
	ID (AST.VarExp(ID, IDleft, IDright))|
	NUM (AST.NumExp(NUM, NUMleft, NUMright))|
	CONST (AST.BoolExp(CONST, CONSTleft, CONSTright))|
	APPEXP (APPEXP)|
	FN_DEF (FN_DEF)
	
	
	

  
