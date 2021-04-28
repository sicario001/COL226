functor A2LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : A2_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User  declarations *)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\019\000\002\000\018\000\006\000\030\000\007\000\029\000\
\\008\000\028\000\009\000\027\000\010\000\026\000\011\000\017\000\
\\012\000\016\000\013\000\055\000\014\000\015\000\016\000\014\000\
\\017\000\025\000\018\000\024\000\019\000\023\000\020\000\022\000\
\\021\000\021\000\022\000\013\000\023\000\012\000\030\000\010\000\000\000\
\\001\000\001\000\019\000\002\000\018\000\011\000\017\000\012\000\016\000\
\\014\000\015\000\016\000\014\000\022\000\013\000\023\000\012\000\
\\028\000\011\000\030\000\010\000\000\000\
\\001\000\001\000\019\000\002\000\018\000\011\000\017\000\012\000\016\000\
\\014\000\015\000\016\000\014\000\022\000\013\000\023\000\012\000\
\\030\000\010\000\000\000\
\\001\000\001\000\033\000\000\000\
\\001\000\001\000\035\000\000\000\
\\001\000\001\000\050\000\000\000\
\\001\000\001\000\058\000\000\000\
\\001\000\003\000\056\000\006\000\030\000\007\000\029\000\008\000\028\000\
\\009\000\027\000\010\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\\001\000\004\000\069\000\006\000\030\000\007\000\029\000\008\000\028\000\
\\009\000\027\000\010\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\\001\000\005\000\079\000\006\000\030\000\007\000\029\000\008\000\028\000\
\\009\000\027\000\010\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\\001\000\006\000\030\000\007\000\029\000\008\000\028\000\009\000\027\000\
\\010\000\026\000\013\000\061\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\\001\000\006\000\030\000\007\000\029\000\008\000\028\000\009\000\027\000\
\\010\000\026\000\017\000\025\000\018\000\024\000\019\000\023\000\
\\020\000\022\000\021\000\021\000\025\000\068\000\000\000\
\\001\000\013\000\071\000\032\000\070\000\000\000\
\\001\000\013\000\077\000\032\000\070\000\000\000\
\\001\000\013\000\078\000\032\000\070\000\000\000\
\\001\000\014\000\032\000\000\000\
\\001\000\014\000\051\000\000\000\
\\001\000\014\000\066\000\033\000\065\000\034\000\064\000\000\000\
\\001\000\024\000\052\000\000\000\
\\001\000\027\000\053\000\000\000\
\\001\000\029\000\057\000\000\000\
\\001\000\029\000\067\000\000\000\
\\001\000\029\000\076\000\000\000\
\\001\000\029\000\081\000\000\000\
\\001\000\031\000\082\000\032\000\070\000\000\000\
\\001\000\031\000\085\000\032\000\070\000\000\000\
\\001\000\035\000\000\000\000\000\
\\088\000\000\000\
\\089\000\001\000\019\000\002\000\018\000\011\000\017\000\012\000\016\000\
\\014\000\015\000\016\000\014\000\022\000\013\000\023\000\012\000\
\\028\000\011\000\030\000\010\000\000\000\
\\090\000\000\000\
\\091\000\015\000\020\000\000\000\
\\092\000\000\000\
\\093\000\006\000\030\000\007\000\029\000\008\000\028\000\009\000\027\000\
\\010\000\026\000\017\000\025\000\018\000\024\000\019\000\023\000\
\\020\000\022\000\021\000\021\000\000\000\
\\094\000\000\000\
\\095\000\006\000\030\000\007\000\029\000\008\000\028\000\009\000\027\000\
\\010\000\026\000\017\000\025\000\018\000\024\000\019\000\023\000\
\\020\000\022\000\021\000\021\000\000\000\
\\096\000\032\000\070\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\006\000\030\000\007\000\029\000\008\000\028\000\009\000\027\000\
\\010\000\026\000\017\000\025\000\018\000\024\000\019\000\023\000\
\\020\000\022\000\021\000\021\000\000\000\
\\101\000\000\000\
\\102\000\006\000\030\000\007\000\029\000\008\000\028\000\009\000\027\000\
\\010\000\026\000\017\000\025\000\018\000\024\000\019\000\023\000\
\\020\000\022\000\021\000\021\000\000\000\
\\103\000\000\000\
\\104\000\019\000\023\000\000\000\
\\105\000\019\000\023\000\000\000\
\\106\000\000\000\
\\107\000\017\000\025\000\018\000\024\000\019\000\023\000\000\000\
\\108\000\017\000\025\000\018\000\024\000\019\000\023\000\000\000\
\\109\000\000\000\
\\110\000\017\000\025\000\018\000\024\000\019\000\023\000\020\000\022\000\
\\021\000\021\000\000\000\
\\111\000\017\000\025\000\018\000\024\000\019\000\023\000\020\000\022\000\
\\021\000\021\000\000\000\
\\112\000\017\000\025\000\018\000\024\000\019\000\023\000\020\000\022\000\
\\021\000\021\000\000\000\
\\113\000\017\000\025\000\018\000\024\000\019\000\023\000\020\000\022\000\
\\021\000\021\000\000\000\
\\114\000\006\000\030\000\007\000\029\000\008\000\028\000\009\000\027\000\
\\010\000\026\000\017\000\025\000\018\000\024\000\019\000\023\000\
\\020\000\022\000\021\000\021\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\"
val actionRowNumbers =
"\028\000\060\000\061\000\033\000\
\\030\000\032\000\001\000\027\000\
\\015\000\003\000\004\000\002\000\
\\058\000\002\000\059\000\002\000\
\\002\000\057\000\031\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\029\000\005\000\016\000\
\\018\000\019\000\048\000\000\000\
\\054\000\007\000\046\000\047\000\
\\045\000\044\000\043\000\052\000\
\\051\000\050\000\049\000\053\000\
\\020\000\006\000\002\000\002\000\
\\010\000\056\000\002\000\017\000\
\\021\000\011\000\039\000\040\000\
\\008\000\012\000\037\000\036\000\
\\017\000\017\000\055\000\002\000\
\\017\000\022\000\013\000\014\000\
\\009\000\035\000\017\000\038\000\
\\023\000\042\000\024\000\017\000\
\\002\000\025\000\041\000\002\000\
\\034\000\026\000"
val gotoT =
"\
\\001\000\007\000\002\000\006\000\003\000\085\000\004\000\005\000\
\\006\000\004\000\007\000\003\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\029\000\002\000\006\000\004\000\005\000\006\000\004\000\
\\007\000\003\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\032\000\000\000\
\\004\000\034\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\004\000\035\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\004\000\036\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\037\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\038\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\039\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\040\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\041\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\042\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\043\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\044\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\045\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\046\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\047\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\052\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\057\000\008\000\002\000\010\000\001\000\000\000\
\\004\000\058\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\060\000\008\000\002\000\010\000\001\000\000\000\
\\009\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\070\000\000\000\
\\009\000\071\000\000\000\
\\000\000\
\\004\000\072\000\008\000\002\000\010\000\001\000\000\000\
\\009\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\081\000\000\000\
\\004\000\082\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\084\000\008\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 86
val numrules = 35
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int*int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | CONST of unit ->  (bool)
 | ID of unit ->  (string) | APPEXP of unit ->  (AST.exp)
 | TYPE of unit ->  (AST.typ) | FN_DEF of unit ->  (AST.exp)
 | FUN_DEF of unit ->  (AST.fun_def)
 | formula of unit ->  (AST.formula) | DECL of unit ->  (AST.decl)
 | EXP of unit ->  (AST.exp) | START of unit ->  (AST.formula list)
 | statement of unit ->  (AST.formula)
 | program of unit ->  (AST.formula list)
end
type svalue = MlyValue.svalue
type result = AST.formula list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 34) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "IF"
  | (T 2) => "THEN"
  | (T 3) => "ELSE"
  | (T 4) => "FI"
  | (T 5) => "IMPLIES"
  | (T 6) => "AND"
  | (T 7) => "OR"
  | (T 8) => "XOR"
  | (T 9) => "EQUALS"
  | (T 10) => "NOT"
  | (T 11) => "CONST"
  | (T 12) => "RPAREN"
  | (T 13) => "LPAREN"
  | (T 14) => "TERM"
  | (T 15) => "NUM"
  | (T 16) => "PLUS"
  | (T 17) => "MINUS"
  | (T 18) => "TIMES"
  | (T 19) => "LESSTHAN"
  | (T 20) => "GREATERTHAN"
  | (T 21) => "NEGATE"
  | (T 22) => "LET"
  | (T 23) => "IN"
  | (T 24) => "END"
  | (T 25) => "VAR"
  | (T 26) => "EQ"
  | (T 27) => "FUN"
  | (T 28) => "COLON"
  | (T 29) => "FN"
  | (T 30) => "DEF"
  | (T 31) => "ARROW"
  | (T 32) => "INT"
  | (T 33) => "BOOL"
  | (T 34) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (program as program1) = program1 ()
 in (program)
end)
 in ( LrTable.NT 2, ( result, program1left, program1right), rest671)

end
|  ( 1, ( rest671)) => let val  result = MlyValue.START (fn _ => ([]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.program program1, _, program1right)) :: ( _,
 ( MlyValue.statement statement1, statement1left, _)) :: rest671)) =>
 let val  result = MlyValue.program (fn _ => let val  (statement as 
statement1) = statement1 ()
 val  (program as program1) = program1 ()
 in (statement::program)
end)
 in ( LrTable.NT 0, ( result, statement1left, program1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.formula formula1, formula1left, 
formula1right)) :: rest671)) => let val  result = MlyValue.program (fn
 _ => let val  (formula as formula1) = formula1 ()
 in ([formula])
end)
 in ( LrTable.NT 0, ( result, formula1left, formula1right), rest671)

end
|  ( 4, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (formula as formula1) = formula1
 ()
 in (formula)
end)
 in ( LrTable.NT 1, ( result, formula1left, TERM1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  (EXP as EXP1)
 = EXP1 ()
 in (AST.FormulaExp(EXP))
end)
 in ( LrTable.NT 5, ( result, EXP1left, EXP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.FUN_DEF FUN_DEF1, FUN_DEF1left, 
FUN_DEF1right)) :: rest671)) => let val  result = MlyValue.formula (fn
 _ => let val  (FUN_DEF as FUN_DEF1) = FUN_DEF1 ()
 in (AST.FormulaFunDef(FUN_DEF))
end)
 in ( LrTable.NT 5, ( result, FUN_DEF1left, FUN_DEF1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.TYPE TYPE2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYPE TYPE1, _
, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let
 val  result = MlyValue.FUN_DEF (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.Fun(ID1, ID2, AST.ARROW(TYPE1, TYPE2), EXP))
end)
 in ( LrTable.NT 6, ( result, FUN1left, EXP1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.TYPE TYPE2, _, TYPE2right)) :: _ :: ( _, ( 
MlyValue.TYPE TYPE1, TYPE1left, _)) :: rest671)) => let val  result = 
MlyValue.TYPE (fn _ => let val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 in (AST.ARROW(TYPE1, TYPE2))
end)
 in ( LrTable.NT 8, ( result, TYPE1left, TYPE2right), rest671)
end
|  ( 9, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.TYPE (fn _ => (AST.INT))
 in ( LrTable.NT 8, ( result, INT1left, INT1right), rest671)
end
|  ( 10, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.TYPE (fn _ => (AST.BOOL))
 in ( LrTable.NT 8, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.TYPE TYPE1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.TYPE (fn _ => let val  (TYPE as TYPE1) = TYPE1 ()
 in (TYPE)
end)
 in ( LrTable.NT 8, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.ValDecl(ID, EXP))
end)
 in ( LrTable.NT 4, ( result, ID1left, EXP1right), rest671)
end
|  ( 13, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP2, _,
 _)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  
EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.AppExp(EXP1, EXP2))
end)
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.TYPE TYPE2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYPE TYPE1, _
, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, FN1left
, _)) :: rest671)) => let val  result = MlyValue.FN_DEF (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.Fn(ID, AST.ARROW(TYPE1, TYPE2), EXP))
end)
 in ( LrTable.NT 7, ( result, FN1left, EXP1right), rest671)
end
|  ( 15, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.EXP EXP3, _, _))
 :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP 
EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in (AST.IteExp(EXP1, EXP2, EXP3))
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Plus, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Minus, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Times, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.GreaterThan, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.LessThan, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 in (AST.UnExp(AST.Negate, EXP))
end)
 in ( LrTable.NT 3, ( result, NEGATE1left, EXP1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.And, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Or, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Xor, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Equals, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Implies, EXP1, EXP2))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (EXP as EXP1) = EXP1 ()
 in (AST.UnExp(AST.Not, EXP))
end)
 in ( LrTable.NT 3, ( result, NOT1left, EXP1right), rest671)
end
|  ( 28, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)
) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _)
) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (
DECL as DECL1) = DECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.LetExp(DECL, EXP))
end)
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 29, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 3, ( result, NUM1left, NUM1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (CONST
 as CONST1) = CONST1 ()
 in (AST.BoolExp(CONST))
end)
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.APPEXP APPEXP1, APPEXP1left, APPEXP1right))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (
APPEXP as APPEXP1) = APPEXP1 ()
 in (APPEXP)
end)
 in ( LrTable.NT 3, ( result, APPEXP1left, APPEXP1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.FN_DEF FN_DEF1, FN_DEF1left, FN_DEF1right))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (
FN_DEF as FN_DEF1) = FN_DEF1 ()
 in (FN_DEF)
end)
 in ( LrTable.NT 3, ( result, FN_DEF1left, FN_DEF1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : A2_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun DEF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
end
end
