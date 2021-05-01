functor A2LexFun(structure Tokens:A2_TOKENS)=
   struct
    structure UserDeclarations =
      struct
structure Tokens= Tokens

type pos = int*int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token

val line_num = ref 1;
val col_num = ref 1;
fun print_error(line_num, col_num, yytext) = 
    print("Unknown Token:"^Int.toString(line_num)^":"^Int.toString(col_num)^":"^yytext^"\n");
val eof = fn () => Tokens.EOF((!line_num,!col_num), (!line_num,!col_num));


val keywords =
  [
   ("if",  Tokens.IF),
   ("then",  Tokens.THEN),
   ("else",  Tokens.ELSE),
   ("fi",  Tokens.FI),
   ("end",  Tokens.END),
   ("in",  Tokens.IN),
   ("let",  Tokens.LET),
   ("PLUS",  Tokens.PLUS),
   ("MINUS",  Tokens.MINUS),
   ("TIMES",  Tokens.TIMES),
   ("NEGATE",  Tokens.NEGATE),
   ("EQUALS",  Tokens.EQUALS),
   ("LESSTHAN",  Tokens.LESSTHAN),
   ("GREATERTHAN",  Tokens.GREATERTHAN),
   ("IMPLIES", Tokens.IMPLIES),
   ("AND", Tokens.AND),
   ("OR", Tokens.OR),
   ("XOR", Tokens.XOR),
   ("NOT", Tokens.NOT),
   ("fun", Tokens.FUN),
   ("fn", Tokens.FN),
   ("int", Tokens.INT),
   ("bool", Tokens.BOOL)
   ]
  fun findKeywords (str:string, pos1:pos, pos2:pos) =
  case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos1, pos2)
  | NONE =>
  	(if (str="TRUE") then (Tokens.CONST(true, pos1, pos2))
  	else if (str = "FALSE") then (Tokens.CONST(false, pos1, pos2))
  	else (Tokens.ID(str, pos1, pos2)))


  
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\016\019\003\003\018\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\016\003\003\003\003\003\003\003\015\014\003\003\003\012\003\003\
\\010\010\010\010\010\010\010\010\010\010\009\008\003\006\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\017\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 30)], trans = 0},
{fin = [(N 28),(N 30)], trans = 4},
{fin = [(N 28)], trans = 4},
{fin = [(N 17),(N 30)], trans = 6},
{fin = [(N 25)], trans = 0},
{fin = [(N 15),(N 30)], trans = 0},
{fin = [(N 22),(N 30)], trans = 0},
{fin = [(N 9),(N 30)], trans = 10},
{fin = [(N 9)], trans = 10},
{fin = [(N 30)], trans = 12},
{fin = [(N 20)], trans = 0},
{fin = [(N 13),(N 30)], trans = 0},
{fin = [(N 11),(N 30)], trans = 0},
{fin = [(N 6),(N 30)], trans = 16},
{fin = [(N 6)], trans = 16},
{fin = [(N 30)], trans = 18},
{fin = [(N 3)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  11 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ; Tokens.LPAREN((!line_num, !col_num),(!line_num, !col_num)) end
| 13 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ; Tokens.RPAREN((!line_num, !col_num),(!line_num, !col_num)) end
| 15 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ; Tokens.TERM((!line_num, !col_num),(!line_num, !col_num)) end
| 17 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ; Tokens.EQ((!line_num, !col_num),(!line_num, !col_num)) end
| 20 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ; Tokens.ARROW((!line_num, !col_num),(!line_num, !col_num)) end
| 22 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ; Tokens.COLON((!line_num, !col_num),(!line_num, !col_num)) end
| 25 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ; Tokens.DEF((!line_num, !col_num),(!line_num, !col_num)) end
| 28 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ; findKeywords(yytext,(!line_num, !col_num),(!line_num, !col_num)) end
| 3 => (line_num := (!line_num) + 1; col_num := 1; lex())
| 30 => let val yytext=yymktext() in print_error(!line_num, !col_num, yytext); lex() end
| 6 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ; lex() end
| 9 => let val yytext=yymktext() in col_num := (!col_num)+size yytext ;Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      (!line_num, !col_num), (!line_num, !col_num)) end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
