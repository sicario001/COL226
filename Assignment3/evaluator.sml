structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

fun logical_implies (s1:bool, s2:bool):bool =
    case (s1, s2) of
        (true, false)   =>  false
        |   _           =>  true

fun evalExp(e:exp, env, env_global):value =
    case e of
	        NumExp (i, _, _)            => IntVal i
        |   BoolExp (b, _, _)           => BoolVal b
        |   VarExp (x, _, _)            => envLookup (x, env, env_global)	  
        |   BinExp (b, e1, e2, _, _)    => evalBinExp(b, e1, e2, env, env_global)
        |   UnExp (u, e, _, _)          => evalUnExp(u, e, env, env_global)
        (* take into account the case when there is fn type in LetExp *)
        |   LetExp(ValDecl(x, e1), e2, _, _)  =>
  	        (let
	            val v1 = evalExp (e1, env, env_global)
	        in
	            evalExp(e2, envAdd (x, v1, env), env_global)
            end)
        |   IteExp (e1, e2, e3, _, _) =>
                (case (evalExp(e1, env, env_global)) of
                    BoolVal true    =>  evalExp (e2, env, env_global)
                |   BoolVal false   =>  evalExp (e3, env, env_global)
                |   _               =>  raise brokenTypes)

        |   Fn (i1, t , e, _, _)  => FunVal(i1, e, env, env_global)
        |   AppExp (e1, e2, _, _)     => 
                    (let
                        val v1 = evalExp(e1, env, env_global)
                    in 
                        
                        case v1 of 
                                FunVal (fp, e_fn, env1, env2)           => evalExp(e_fn, envAdd(fp, evalExp(e2, env, env_global), env1), env2)
                            |   RecFunVal (i, fp, e_fun, env1, env2)    => evalExp(e_fun, envAdd(fp, evalExp(e2, env, env_global), env1),  envAdd(i, v1, env2))
                            |       _               => raise brokenTypes
                    end)

                    
                

and
evalBinExp(b:binop, e1:exp, e2:exp, env, env_global):value =
case (b, evalExp(e1, env, env_global), evalExp(e2, env, env_global))  of
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
evalUnExp(u:unop, e:exp, env, env_global):value = 
case (u, evalExp(e, env, env_global)) of
    (Negate, IntVal i)  => IntVal (~i)
    |   (Not, BoolVal s)    => BoolVal (not s)
    |   _   =>  raise brokenTypes


and 
evalProgram(p:formula list, env_global):value list = 
    if (null p) then ([])
    else(
        case (hd p) of 
                FormulaFunDef(Fun (i1, i2, t, e, _, _))         => ((evalProgram(tl p, envAdd(i1, RecFunVal(i1, i2, e, [], env_global), env_global ))))
            |   FormulaExp (e, _, _)                            => (evalExp(e, [], env_global)::(evalProgram(tl p, env_global)))
    ) 

end