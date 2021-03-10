structure Tokens= Tokens

type pos = int*int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token

val line_num = ref 1;
val col_num = ref 1;
val eof = fn () => Tokens.EOF((0,0), (0,0));



  
%%
%header (functor A2LexFun(structure Tokens:A2_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n            => (line_num := (!line_num) + 1; col_num := 1; lex());
{ws}+         => (col_num := (!col_num)+size yytext ; lex());
"IF"          => (col_num := (!col_num)+size yytext ; Tokens.IF("IF \"IF\", ", (!line_num, !col_num), (!line_num, !col_num)));
"THEN"        => (col_num := (!col_num)+size yytext ; Tokens.THEN("THEN \"THEN\", ", (!line_num, !col_num), (!line_num, !col_num)));
"ELSE"        => (col_num := (!col_num)+size yytext ; Tokens.ELSE("ELSE \"ELSE\", ", (!line_num, !col_num),(!line_num, !col_num)));
"IMPLIES"     => (col_num := (!col_num)+size yytext ; Tokens.IMPLIES("IMPLIES \"IMPLIES\", ", (!line_num, !col_num),(!line_num, !col_num)));
"AND"         => (col_num := (!col_num)+size yytext ; Tokens.AND("AND \"AND\", ", (!line_num, !col_num),(!line_num, !col_num)));
"OR"          => (col_num := (!col_num)+size yytext ; Tokens.OR("OR \"OR\", ", (!line_num, !col_num),(!line_num, !col_num)));
"XOR"         => (col_num := (!col_num)+size yytext ; Tokens.XOR("XOR \"XOR\", ", (!line_num, !col_num),(!line_num, !col_num)));
"EQUALS"      => (col_num := (!col_num)+size yytext ; Tokens.EQUALS("EQUALS \"EQUALS\", ", (!line_num, !col_num), (!line_num, !col_num)));
"NOT"         => (col_num := (!col_num)+size yytext ; Tokens.NOT("NOT \"NOT\", ", (!line_num, !col_num),(!line_num, !col_num)));
"TRUE"        => (col_num := (!col_num)+size yytext ; Tokens.CONST("CONST "^"\""^yytext^"\", ", (!line_num, !col_num),(!line_num, !col_num)));
"FALSE"       => (col_num := (!col_num)+size yytext ; Tokens.CONST("CONST "^"\""^yytext^"\", ", (!line_num, !col_num),(!line_num, !col_num)));
"("           => (col_num := (!col_num)+size yytext ; Tokens.LPAREN("LPAREN \"(\", ",(!line_num, !col_num),(!line_num, !col_num)));
")"           => (col_num := (!col_num)+size yytext ; Tokens.RPAREN("RPAREN \")\", ",(!line_num, !col_num),(!line_num, !col_num)));
";"           => (col_num := (!col_num)+size yytext ; Tokens.TERM("TERM \";\", ",(!line_num, !col_num),(!line_num, !col_num)));
{alpha}+      => (col_num := (!col_num)+size yytext ; Tokens.ID("ID "^"\""^yytext^"\", ", (!line_num, !col_num),(!line_num, !col_num)));
 .            => (lex());

