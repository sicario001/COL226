1. Run the command `make all` to generate the required files.
2. SML/NJ interactive session opens on the terminal.
3. Use the function `lex_parse_fun (<infilename>, <outfilename>);` to take the input from the file <infilename> and generate the ouput of the lexer and parser in the file <outfilename>.
4. Note that <infilename> and <outfilename> should be within double quotes.
5. For eg., a sample input file "inp" has been been provided. To generate the output, type the following command in SML/NJ interactive session :
	`lex_parse_fun("inp", "out");`
6. Run the command `make clean` to remove the generated files.
