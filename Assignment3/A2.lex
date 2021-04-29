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


  
%%
%header (functor A2LexFun(structure Tokens:A2_TOKENS));

alpha = [A-Za-z];
alphaNum = [A-Za-z0-9];
digit = [0-9];
ws = [\ \t];
%%
\n|"\r\n"	=> (line_num := (!line_num) + 1; col_num := 1; lex());
{ws}+		=> (col_num := (!col_num)+size yytext ; lex());
{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      (!line_num, !col_num), (!line_num, !col_num)));
"("		=> (col_num := (!col_num)+size yytext ; Tokens.LPAREN((!line_num, !col_num),(!line_num, !col_num)));
")"		=> (col_num := (!col_num)+size yytext ; Tokens.RPAREN((!line_num, !col_num),(!line_num, !col_num)));
";"		=> (col_num := (!col_num)+size yytext ; Tokens.TERM((!line_num, !col_num),(!line_num, !col_num)));
"="		=> (col_num := (!col_num)+size yytext ; Tokens.EQ((!line_num, !col_num),(!line_num, !col_num)));
"->"		=> (col_num := (!col_num)+size yytext ; Tokens.ARROW((!line_num, !col_num),(!line_num, !col_num))); 
":"		=> (col_num := (!col_num)+size yytext ; Tokens.COLON((!line_num, !col_num),(!line_num, !col_num)));
"=>"		=> (col_num := (!col_num)+size yytext ; Tokens.DEF((!line_num, !col_num),(!line_num, !col_num)));
{alpha}{alphaNum}*	=> (col_num := (!col_num)+size yytext ; findKeywords(yytext,(!line_num, !col_num),(!line_num, !col_num)));
 .		=> (print_error(!line_num, !col_num, yytext); lex());

