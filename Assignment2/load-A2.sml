structure A2LrVals = A2LrValsFun(structure Token = LrParser.Token)
structure A2_Lex = A2LexFun(structure Tokens = A2LrVals.Tokens);
structure A2Parser =
	  	Join(structure LrParser = LrParser
     	structure ParserData = A2LrVals.ParserData
       	structure Lex = A2_Lex)
     
exception ParseErrorFound of string*int*int
fun invoke lexstream =
    	let fun print_error (s,(line_num,col_num), _) = raise ParseErrorFound (s, line_num, col_num)
		in
		    A2Parser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  A2Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
		lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = A2LrVals.Tokens.EOF((0,0),(0,0))
    	val (result, lexer) = invoke lexer
		val (nextToken, lexer) = A2Parser.Stream.get lexer
    in
        if A2Parser.sameToken(nextToken, dummyEOF) then result
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer

fun stringToLexer_A2Lex str =
	let 
		val done = ref false
    	val lexer= A2Lex.makeLexer(fn _ => if (!done) then "" else (done := true; str))
    in
		lexer
	end

fun process_token_out (token_A2Lex) = 
	A2Lex.UserDeclarations.toString (token_A2Lex)
fun process_tokens (lexer, lex_out_str) =
	let
		val token_val = lexer()
	in
		case token_val of 
			A2Lex.UserDeclarations.EOF	=> (lex_out_str^"]")
		|	_							=> (process_tokens (lexer, lex_out_str^","^process_token_out(token_val)))
	end

fun lex_output (Infilename, Out)=
	let
		val In = TextIO.openIn Infilename
		val inp_str = TextIO.inputAll(In)
		val lexer = stringToLexer_A2Lex inp_str
		val lex_out_str = process_tokens (lexer, "")
	in
		TextIO.closeIn In;
		case String.sub(lex_out_str, 0) of
			#","	=>	TextIO.output(Out, "["^String.extract(lex_out_str, 1, NONE))
			| _		=>	TextIO.output(Out, "[]")
		
	end
	

fun parse_output (Infilename, Out)=
	let 
		val In = TextIO.openIn Infilename
		val inp_str = TextIO.inputAll(In)
		val out_parser = parseString(inp_str)
	in
		TextIO.closeIn In;
		TextIO.output(Out, "\n");
		TextIO.output(Out, "["^out_parser^"]")
	end
	handle ParseErrorFound (s, line_num, col_num) => print("Syntax Error:"^Int.toString(line_num)^":"^Int.toString(col_num)^":"^s^"\n")
fun lex_parse_fun (Infilename, Outfilename) = 
	let
		val Out = TextIO.openOut Outfilename
	in
		lex_output(Infilename, Out);
		parse_output(Infilename, Out);
		TextIO.closeOut Out
	end
	handle A2Lex.UserDeclarations.UnknownToken(s) => print(s)




