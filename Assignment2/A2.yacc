(* User  declarations *)

%%
(* required declarations *)
%name A2

%term
  ID of string | IF of string| THEN of string | ELSE of string| IMPLIES of string| AND of string| OR of string| XOR of string| EQUALS of string| NOT of string| CONST of string | RPAREN of string| LPAREN of string| TERM of string| EOF


%nonterm
  program of string| statement of string | START of string| formula of string
%pos int*int

(*optional declarations *)
%eop EOF
%noshift EOF

%right ELSE
%right IMPLIES
%left AND OR XOR EQUALS 
%right NOT


%start START

%verbose

%%

START: program (program^"START : program")
program: statement program (statement^program^"program : statement program, ") | ("program : epsilon, ")
statement: formula TERM (formula^TERM^"statement : formula TERM, ")
  

formula:
    IF formula THEN formula ELSE formula (IF^formula1^THEN^formula2^ELSE^formula3^"formula : IF formula THEN formula ELSE formula, ") 
  | formula AND formula (formula1^AND^formula2^"formula : formula AND formula, ")
  | formula OR formula (formula1^OR^formula2^"formula : formula OR formula, ")
  | formula XOR formula (formula1^XOR^formula2^"formula : formula XOR formula, ")
  | formula EQUALS formula (formula1^EQUALS^formula2^"formula : formula EQUALS formula, ")
  | formula IMPLIES formula (formula1^IMPLIES^formula2^"formula : formula IMPLIES formula, ")
  | NOT formula (NOT^formula^"formula : NOT formula, ")
  | LPAREN formula RPAREN (LPAREN^formula^RPAREN^"formula : LAPAREN formula RPAREN, ")
  | ID (ID^"formula : ID, ")
  | CONST (CONST^"formula : CONST, ")



  
