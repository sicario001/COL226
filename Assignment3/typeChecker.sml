structure TYPE_CHECKER  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"


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
                    (BOOL)              => if (isSameTyp(checkExp(e2, env, env_global), checkExp(e3, env, env_global))) then (checkExp(e2, env, env_global)) else (raise brokenTypes)
                |   _                   => raise brokenTypes)
                
        (* type check fn function *)
        |   Fn (i1, t, e)       =>  (case t of
                                        ARROW(t1, t2)   => if (isSameTyp(t2, checkExp(e, typEnvAdd(i1, t1, env), env_global))) then (t) else (raise brokenTypes)
                                    |   _               => raise brokenTypes)


        |   AppExp (e1, e2)     => 
                    (let
                        val t = checkExp(e1, env, env_global)
                    in 
                        
                        case t of
                                ARROW(t1, t2)           => if (isSameTyp(t1, checkExp(e2, env, env_global))) then (t2) else (raise brokenTypes)
                            |   _                       => raise brokenTypes
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
  |     _                                       => raise brokenTypes  		

and 
checkUnExp(u:unop, e:exp, env, env_global):typ = 
case (u, checkExp(e, env, env_global)) of
        (Negate, INT)   => INT
    |   (Not, BOOL)     => BOOL
    |   _               => raise brokenTypes
                    
                

fun checkProgram(p:formula list, env_global:typEnv):typ list = 
    if (null p) then ([])
    else(
        case (hd p) of 
                FormulaFunDef(Fun (i1, i2, t, e))       =>  ((case t of
                                                                ARROW(t1, t2)   => (if (isSameTyp(t2, checkExp(e, typEnvAdd(i2, t1, []), typEnvAdd(i1, t, env_global)))) then (t) else (raise brokenTypes))
                                                            |   _               => raise brokenTypes)::checkProgram(tl p, typEnvAdd(i1, t, env_global)))
                        
            |   FormulaExp e                            => (checkExp(e, [], env_global)::(checkProgram(tl p, env_global)))
    ) 

end