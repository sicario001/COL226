structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"
val typeMismatchITE = Fail "Incorrect types in if then else statement!"

fun logical_implies (s1:bool, s2:bool):bool =
    case (s1, s2) of
        (true, false)   =>  false
        |   _           =>  true

fun evalExp(e:exp, env:environment):value =
    case e of
	NumExp i            => IntVal i
      | BoolExp b         => BoolVal b
      | VarExp x            => envLookup (x, env) 				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | UnExp (u, e)        => evalUnExp(u, e, env)
      | LetExp(ValDecl(x, e1), e2)  =>
  	let
	    val v1 = evalExp (e1, env)
	in
	    evalExp(e2, envAdd (x, v1, env))
        end
      | IteExp (e1, e2, e3) =>
            case (evalExp(e1, env), evalExp(e2, env), evalExp(e3, env)) of
                (BoolVal true, IntVal i1, IntVal i2)    =>  IntVal (i1)
            |   (BoolVal false, IntVal i1, IntVal i2)   =>  IntVal (i2)
            |   (BoolVal true, BoolVal s1, BoolVal s2)  =>  BoolVal (s1)
            |   (BoolVal false, BoolVal s1, BoolVal s2)  =>  BoolVal (s2)
            |   _   =>  raise typeMismatchITE
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
    (Plus, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (Minus, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (Times, IntVal i1, IntVal i2) => IntVal (i1*i2)
  |   (LessThan, IntVal i1, IntVal i2)  => BoolVal (i1<i2)
  |   (GreaterThan, IntVal i1, IntVal i2)  => BoolVal (i1>i2)
  |   (Equals, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
  |   (Equals, BoolVal s1, BoolVal s2)  =>  BoolVal (s1 = s2)
  |   (And, BoolVal s1, BoolVal s2) => BoolVal (s1 andalso s2)
  |   (Or, BoolVal s1, BoolVal s2) => BoolVal (s1 orelse s2)
  |   (Xor, BoolVal s1, BoolVal s2) => BoolVal (s1 <> s2)
  |   (Implies, BoolVal s1, BoolVal s2) => BoolVal (logical_implies(s1,s2))
  |   _  => raise brokenTypes  		

and 
evalUnExp(u:unop, e:exp, env:environment):value = 
case (u, evalExp(e, env)) of
    (Negate, IntVal i)  => IntVal (~i)
    |   (Not, BoolVal s)    => BoolVal (not s)
    |   _   =>  raise brokenTypes
end
