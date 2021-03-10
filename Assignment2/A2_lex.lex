datatype lexresult =  ID of string | IF | THEN | ELSE | IMPLIES | AND | OR | XOR | EQUALS | NOT | CONST of string | RPAREN | LPAREN | TERM | EOF 

val line_num = ref 1;
val col_num = ref 1;
val eof = fn () => (line_num := 1; col_num := 1; EOF);
fun print_error(line_num, col_num, yytext) = 
    print("Unknown Token:"^Int.toString(line_num)^":"^Int.toString(col_num)^":"^yytext^"\n");

exception UnknownToken of string;
fun raise_exception(line_num, col_num, yytext) =
    raise UnknownToken("Unknown Token:"^Int.toString(line_num)^":"^Int.toString(col_num)^":"^yytext^"\n")

fun toString(ID(s))     = "ID \""^s^"\""
    | toString(IF)      = "IF \"IF\""
    | toString(THEN)    = "THEN \"THEN\""
    | toString(ELSE)    = "ELSE \"ELSE\""
    | toString(IMPLIES) = "IMPLIES \"IMPLIES\""
    | toString(AND)     = "AND \"AND\""
    | toString(OR)      = "OR \"OR\""
    | toString(XOR)     = "XOR \"XOR\""
    | toString(EQUALS)  = "EQUALS \"EQUALS\""
    | toString(NOT)     = "NOT \"NOT\""
    | toString(CONST(s))= "CONST \""^s^"\""
    | toString(LPAREN)  = "LPAREN \"(\""
    | toString(RPAREN)  = "RPAREN \")\""
    | toString(TERM)    = "TERM \";\""
  
%%
%structure A2Lex
alpha=[A-Za-z];
ws = [\ \t];
%%
\n            => (line_num := (!line_num) +1; col_num := 1; lex());
{ws}+         => (col_num := (!col_num)+size yytext ; lex());
"IF"          => (col_num := (!col_num)+size yytext ; IF);
"THEN"        => (col_num := (!col_num)+size yytext ; THEN);
"ELSE"        => (col_num := (!col_num)+size yytext ; ELSE);
"IMPLIES"     => (col_num := (!col_num)+size yytext ; IMPLIES);
"AND"         => (col_num := (!col_num)+size yytext ; AND);
"OR"          => (col_num := (!col_num)+size yytext ; OR);
"XOR"         => (col_num := (!col_num)+size yytext ; XOR);
"EQUALS"      => (col_num := (!col_num)+size yytext ; EQUALS);
"NOT"         => (col_num := (!col_num)+size yytext ; NOT);
"TRUE"        => (col_num := (!col_num)+size yytext ; CONST yytext);
"FALSE"       => (col_num := (!col_num)+size yytext ; CONST yytext);
"("           => (col_num := (!col_num)+size yytext ; LPAREN);
")"           => (col_num := (!col_num)+size yytext ; RPAREN);
";"           => (col_num := (!col_num)+size yytext ; TERM);
{alpha}+      => (col_num := (!col_num)+size yytext ; ID yytext );
 .            => (raise_exception(!line_num, !col_num, yytext));

