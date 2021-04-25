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
				       
datatype value = IntVal of int
	            | BoolVal of bool
				
type environment = (id * value) list

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							    
end


