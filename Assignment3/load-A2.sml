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



fun parse_output (Infilename)=
	let 
		val In = TextIO.openIn Infilename
		val inp_str = TextIO.inputAll(In)
	in
		parseString(inp_str)
	end
	handle ParseErrorFound (s, line_num, col_num) => (print("Syntax Error:"^Int.toString(line_num)^":"^Int.toString(col_num)^":"^s^"\n"); [])
fun evaluateParsed (parsedVal : AST.exp list) = 
	if (null parsedVal) then ([])
	else ((EVALUATOR.evalExp(hd parsedVal, []))::(evaluateParsed(tl parsedVal)))

fun evaluate (Infilename) = 
	let 
		val parsedVal = parse_output(Infilename)
	in 
		evaluateParsed(parsedVal)
	end