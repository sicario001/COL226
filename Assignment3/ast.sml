structure AST =
struct

type id = string
type pos = int * int

datatype binop = Plus | Minus | Times | Equals | LessThan | GreaterThan | And | Or | Xor | Implies
datatype unop = Negate | Not

datatype decl = ValDecl of id * exp

and exp = NumExp of int * pos * pos
    	| BoolExp of bool * pos * pos
    	| VarExp of id * pos * pos
	    | BinExp of binop * exp * exp * pos * pos
        | UnExp of unop * exp * pos * pos
	    | LetExp of decl * exp * pos * pos
        | IteExp of exp * exp * exp * pos * pos
		| Fn of id * typ * exp * pos * pos
		| AppExp of exp * exp * pos * pos

and fun_def = Fun of id * id * typ * exp * pos * pos
and typ = INT
		| BOOL
		| ARROW of typ * typ

and formula = FormulaExp of exp * pos * pos
			| FormulaFunDef of fun_def

datatype value = IntVal of int
	            | BoolVal of bool
				| FunVal of id * exp * (id * value) list * (id * value) list
				| RecFunVal of id * id * exp * (id * value) list * (id * value) list


fun envAdd (var:id, v:value, env) =
    (var,v)::env

fun envLookup (var:id, env, env_global) =
    case List.find(fn (x, _) => x = var) env of
				       	SOME (x, v)	=> v
				    |   NONE 		=> (case List.find(fn (x, _) => x = var) env_global of
													SOME (x, v)	=>	v
												|	NONE 		=> (print("Couldn't find \""^var^"\" in the environment\n");raise Fail "Environment lookup error"))



type typEnv = (id * typ) list


fun typEnvAdd (var:id, t:typ, env) =
    (var,t)::env

fun typEnvLookup (var:id, env, env_global) =
    case List.find(fn (x, _) => x = var) env of
				       	SOME (x, t)	=> t
				    |   NONE 		=> (case List.find(fn (x, _) => x = var) env_global of
													SOME (x, t)	=>	t
												|	NONE 		=> (print("Couldn't find \""^var^"\" in the environment\n");raise Fail "Environment lookup error"))			    



fun toStringF(f:formula):string =
	case f of 
		FormulaExp (e, _, _)	=>	"FormulaExp ("^toStringE(e)^")"
	|	FormulaFunDef (Fun (i1, i2, t, e, _, _)) => "FormulaFunDef (Fun (\""^i1^"\", \""^i2^"\", "^toStringT(t)^", "^toStringE(e)^"))"

and toStringT(t:typ):string = 
	case t of 
		INT				=> "INT"
	|	BOOL			=> "BOOL"
	|	ARROW(t1, t2)	=> "ARROW ("^toStringT(t1)^", "^toStringT(t2)^")"

and toStringE(e:exp):string = 
	case e of 
		NumExp (i, _,  _)			=> "NumExp "^Int.toString(i)
    | 	BoolExp (b, _, _)			=> "BoolExp "^Bool.toString(b)
    |	VarExp (i, _, _)			=> "VarExp \""^i^"\""
	| 	BinExp (b, e1, e2, _, _)	=> "BinExp ("^toStringBop(b)^", "^toStringE(e1)^", "^toStringE(e2)^")"
    | 	UnExp (u, e, _, _)			=> "UnExp ("^toStringUop(u)^", "^toStringE(e)^")"
	| 	LetExp (d, e, _, _)			=> "LetExp ("^toStringDec(d)^", "^toStringE(e)^")"
    | 	IteExp (e1, e2, e3, _, _)	=> "IteExp ("^toStringE(e1)^", "^toStringE(e2)^", "^toStringE(e3)^")"
	| 	Fn (i, t, e, _, _)			=> "Fn (\""^i^"\", "^toStringT(t)^", "^toStringE(e)^")"
	| 	AppExp (e1, e2, _, _)		=> "AppExp ("^toStringE(e1)^", "^toStringE(e2)^")"

and toStringBop(b: binop):string = 
	case b of
		Plus		=> "Plus"
	|	Minus		=> "Minus"
	|	Times		=> "Times"
	|	Equals		=> "Equals"
	|	LessThan	=> "LessThan"
	|	GreaterThan	=> "GreaterThan"
	|	And			=> "And"
	|	Or			=> "Or"
	|	Xor			=> "Xor"
	|	Implies		=> "Implies"
and toStringUop(u:unop):string = 
	case u of
		Negate		=> "Negate"
	|	Not			=> "Not"

and toStringDec(d:decl):string = 
	case d of 
		ValDecl(i, e)	=> "ValDecl (\""^i^"\", "^toStringE(e)^")"


end


