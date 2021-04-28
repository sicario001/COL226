structure TYPE_CHECKER  =
struct
open AST

val brokenTypes = Fail "Type checking failed!"

fun ExpArgTypeUnOp (u):string = 
    case u of 
        Not     => "BOOL"
    |   Negate  => "INT"

fun ExpArgTypeBinOp (u):string = 
    case u of 
        Plus		=> "INT * INT"
	|	Minus		=> "INT * INT"
	|	Times		=> "INT * INT"
	|	Equals		=> "(INT * INT) or (BOOL * BOOL)"
	|	LessThan	=> "INT * INT"
	|	GreaterThan	=> "INT * INT"
	|	And			=> "BOOL * BOOL"
	|	Or			=> "BOOL * BOOL"
	|	Xor			=> "BOOL * BOOL"
	|	Implies		=> "BOOL * BOOL"
        


fun UnExpError(u, t):string = 
    "UnExp Error!\nExpected argument type for "^toStringUop(u)^" : "^ExpArgTypeUnOp(u)^"\nActual argument type : "^toStringT(t)^"\n"


fun BinExpError(b, t1, t2):string = 
    "BinExp Error!\nExpected argument type for "^toStringBop(b)^" : "^ExpArgTypeBinOp(b)^"\nActual argument type : "^toStringT(t1)^" * "^toStringT(t2)^"\n"

fun IteErrorTyp1(t):string = 
    "ITE Error!\nThe first argument must be of type BOOL\nActual argument type : "^toStringT(t)^"\n"

fun IteErrorTyp2(t1, t2):string = 
    "ITE Error!\nThe second and third argument must be of the same type\nActual argument types are : "^toStringT(t1)^" and "^toStringT(t2)^"\n"

fun FnErrorReturnType(t1, t2):string = 
    "Function (fn type) expected return type and expression type don't match\nExpected return type : "^toStringT(t1)^"\nExpression type : "^toStringT(t2)^"\n"

fun FunErrorReturnType(i, t1, t2):string = 
    "Function \""^i^"\" expected return type and expression type don't match\nExpected return type : "^toStringT(t1)^"\nExpression type : "^toStringT(t2)^"\n"

fun AppExpError(t1,t2):string = 
    "Error in function application!\nExpected argument type : "^toStringT(t1)^"\nActual argument type : "^toStringT(t2)^"\n"

fun notFuncError():string = 
    "Error in function application!\nThe expression doesn't expect any arguments\n"
fun isSameTyp(t1:typ, t2:typ):bool = 
    case (t1, t2) of
        (INT, INT)                          =>  true
    |   (BOOL, BOOL)                        =>  true
    |   (ARROW(t11, t12), ARROW(t21, t22))  =>  isSameTyp(t11, t21) andalso isSameTyp(t12, t22)
    |   _                                   =>  false


fun checkExp(e:exp, env, env_global):typ =
    case e of
	        NumExp i            => INT
        |   BoolExp b           => BOOL
        |   VarExp x            => typEnvLookup (x, env, env_global)	  
        |   BinExp (b, e1, e2)  => checkBinExp(b, e1, e2, env, env_global)
        |   UnExp (u, e)        => checkUnExp(u, e, env, env_global)
        |   LetExp(ValDecl(x, e1), e2)  =>
  	        (let
	            val t1 = checkExp (e1, env, env_global)
	        in
	            checkExp(e2, typEnvAdd (x, t1, env), env_global)
            end)
        (* type check IteExp *)
        |   IteExp (e1, e2, e3) =>
                
                (case (checkExp(e1, env, env_global)) of 
                    (BOOL)              => if (isSameTyp(checkExp(e2, env, env_global), checkExp(e3, env, env_global))) then (checkExp(e2, env, env_global)) else (print(IteErrorTyp2(checkExp(e2, env, env_global), checkExp(e3, env, env_global))) ;raise brokenTypes)
                |   _                   => (print(IteErrorTyp1(checkExp(e1, env, env_global))) ;raise brokenTypes))
                
        (* type check fn function *)
        |   Fn (i1, t, e)       =>  (case t of
                                        ARROW(t1, t2)   => if (isSameTyp(t2, checkExp(e, typEnvAdd(i1, t1, env), env_global))) then (t) else (print(FnErrorReturnType(t2, checkExp(e, typEnvAdd(i1, t1, env), env_global))); raise brokenTypes)
                                    |   _               => raise brokenTypes)


        |   AppExp (e1, e2)     => 
                    (let
                        val t = checkExp(e1, env, env_global)
                    in 
                        
                        case t of
                                ARROW(t1, t2)           => if (isSameTyp(t1, checkExp(e2, env, env_global))) then (t2) else (print(AppExpError(t1, checkExp(e2, env, env_global))); raise brokenTypes)
                            |   _                       => (print(notFuncError()); raise brokenTypes)
                    end)

and
checkBinExp(b:binop, e1:exp, e2:exp, env, env_global):typ =
case (b, checkExp(e1, env, env_global), checkExp(e2, env, env_global))  of
        (Plus, INT, INT)                        => INT
  |     (Minus, INT, INT)                       => INT
  |     (Times, INT, INT)                       => INT
  |     (LessThan, INT, INT)                    => BOOL
  |     (GreaterThan, INT, INT)                 => BOOL
  |     (Equals, INT, INT)                      => BOOL
  |     (Equals, BOOL, BOOL)                    => BOOL
  |     (And, BOOL, BOOL)                       => BOOL
  |     (Or, BOOL, BOOL)                        => BOOL
  |     (Xor, BOOL, BOOL)                       => BOOL
  |     (Implies, BOOL, BOOL)                   => BOOL
  |     _                                       => (print(BinExpError(b, checkExp(e1, env, env_global), checkExp(e2, env, env_global))); raise brokenTypes) 		

and 
checkUnExp(u:unop, e:exp, env, env_global):typ = 
case (u, checkExp(e, env, env_global)) of
        (Negate, INT)   => INT
    |   (Not, BOOL)     => BOOL
    |   _               => (print(UnExpError(u, checkExp(e, env, env_global))) ; raise brokenTypes)
                    
                

fun checkProgram(p:formula list, env_global:typEnv):typ list = 
    if (null p) then ([])
    else(
        case (hd p) of 
                FormulaFunDef(Fun (i1, i2, t, e))       =>  ((case t of
                                                                ARROW(t1, t2)   => (if (isSameTyp(t2, checkExp(e, typEnvAdd(i2, t1, []), typEnvAdd(i1, t, env_global)))) then (t) else (print(FunErrorReturnType(i1, t2, checkExp(e, typEnvAdd(i2, t1, []), typEnvAdd(i1, t, env_global))));raise brokenTypes))
                                                            |   _               => raise brokenTypes)::checkProgram(tl p, typEnvAdd(i1, t, env_global)))
                        
            |   FormulaExp e                            => (checkExp(e, [], env_global)::(checkProgram(tl p, env_global)))
    ) 

end