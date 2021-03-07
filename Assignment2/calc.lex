structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val line_num = ref 1
  val col_num = ref 1
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  
%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n            => (line_num := (!line_num) + 1; col_num := 1; lex());
{ws}+         => (lex());
"IF"          => (Tokens.IF(!pos,!pos); col_num := (!col_num)+1);
"THEN"        => (Tokens.THEN(!pos,!pos); col_num := (!col_num)+1);
"ELSE"        => (Tokens.ELSE(!pos,!pos); col_num := (!col_num)+1);
"IMPLIES"     => (Tokens.IMPLIES(!pos,!pos); col_num := (!col_num)+1);
"AND"         => (Tokens.AND(!pos,!pos)); col_num := (!col_num)+1;
"OR"          => (Tokens.OR(!pos,!pos); col_num := (!col_num)+1);
"XOR"         => (Tokens.XOR(!pos,!pos); col_num := (!col_num)+1);
"EQUALS"      => (Tokens.EQUALS(!pos,!pos); col_num := (!col_num)+1);
"NOT"         => (Tokens.NOT(!pos,!pos); col_num := (!col_num)+1);
"TRUE"        => (Tokens.TRUE(!pos,!pos); col_num := (!col_num)+1);
"FALSE"       => (Tokens.FALSE(!pos,!pos); col_num := (!col_num)+1);
"("           => (Tokens.LPAREN(!pos,!pos); col_num := (!col_num)+1);
")"           => (Tokens.RPAREN(!pos,!pos); col_num := (!col_num)+1);
";"           => (Tokens.TERM(!pos,!pos); col_num := (!col_num)+1);
{alpha}+      => (Tokens.ID(!pos,!pos); col_num := (!col_num)+1);
 .      => (error ("ignoring bad character "^yytext,!pos,!pos); lex());

