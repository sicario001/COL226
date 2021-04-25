structure Tokens= Tokens

type pos = int*int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token

val line_num = ref 1;
val col_num = ref 1;
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
   ("NOT", Tokens.NOT)
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

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n            => (line_num := (!line_num) + 1; col_num := 1; lex());
{ws}+         => (col_num := (!col_num)+size yytext ; lex());
{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      (!line_num, !col_num), (!line_num, !col_num)));
"("           => (col_num := (!col_num)+size yytext ; Tokens.LPAREN((!line_num, !col_num),(!line_num, !col_num)));
")"           => (col_num := (!col_num)+size yytext ; Tokens.RPAREN((!line_num, !col_num),(!line_num, !col_num)));
";"           => (col_num := (!col_num)+size yytext ; Tokens.TERM((!line_num, !col_num),(!line_num, !col_num)));
"="      => (Tokens.EQ((!line_num, !col_num),(!line_num, !col_num))); 
{alpha}+      => (col_num := (!col_num)+size yytext ; findKeywords(yytext,(!line_num, !col_num),(!line_num, !col_num)));
 .            => (lex());

