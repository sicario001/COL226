structure AST =
struct

type id = string

datatype binop = Plus | Minus | Times | Equals | LessThan | GreaterThan | And | Or | Xor | Implies
datatype unop = Negate | Not

datatype decl = ValDecl of id * exp

and exp = NumExp of int
    	| BoolExp of bool
    	| VarExp of id
	    | BinExp of binop * exp * exp
        | UnExp of unop * exp
	    | LetExp of decl * exp
        | IteExp of exp * exp * exp
		| Fn of id * typ * exp
		| AppExp of exp * exp

and fun_def = Fun of id * id * typ * exp
and typ = INT
		| BOOL
		| ARROW of typ * typ

and formula = FormulaExp of exp
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
												|	NONE 		=> (print(var^"\n");raise Fail "Environment lookup error"))



type typEnv = (id * typ) list


fun typEnvAdd (var:id, t:typ, env) =
    (var,t)::env

fun typEnvLookup (var:id, env, env_global) =
    case List.find(fn (x, _) => x = var) env of
				       	SOME (x, t)	=> t
				    |   NONE 		=> (case List.find(fn (x, _) => x = var) env_global of
													SOME (x, t)	=>	t
												|	NONE 		=> (print(var^"\n");raise Fail "Environment lookup error"))			    
end


