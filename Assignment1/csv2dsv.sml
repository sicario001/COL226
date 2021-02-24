fun convertDelimiters(infilename: string, delim1, outfilename: string, delim2) =
  let
    (* We open the input stream In  *)
    val In = TextIO.openIn infilename
    (* We open the output stream Out *)
    val Out = TextIO.openOut outfilename
    (* Exception ImproperFormat is raised when the input file doesn't have the correct format. This includes the following cases -
      1) When a field contains the beginning double quote but the ending double quote is absent.
      2) When a double quote is present inside a field but is not escaped by another double quote character
      3) When a field containing double quote is not enclosed within double quotes*)
    exception ImproperFormat
    (* Exception newline_misiing is raised when LF is not the last character of the file.*)
    exception newline_missing
    (* Exception emptyInputFile is raised when the input file is empty *)
    exception emptyInputFile
    (* Exception UnevenFields is raised when the number of fields in each line is not the same *)
    exception UnevenFields of string
    (* function raise_exception is used for raising the exception UnevenFields *)
    fun raise_exception(line_num, fields_num_curr, fields_num_prev) =
      raise UnevenFields("Expected: "^Int.toString(fields_num_prev)^" fields, Present: "^Int.toString(fields_num_curr)^" fields on Line "^Int.toString(line_num)^"\n")
    (* function handle_exception is used for handling the exception UnevenFields *)
    fun handle_exception(line_num, fields_num_curr, fields_num_prev) = 
      raise_exception(line_num, fields_num_curr, fields_num_prev)
      handle UnevenFields s => print(s)

    (* A number of different helper functions have been used while reading the input file character by character. On the basis of the characters read, a number of different possible states are possible. Each of these states are handled by a separate helper function as described below:
      1) S0 : When a new line begins (this doesn't consider the LF character inside a field) => Handled by helper_start_newline
      2) S1 : When a new field begins (in the same line) => Handled by helper_begin_new_field
      3) S2 : When the first character of a field not enclosed in double quotes has been read => Handled by helper_find_delimiters
      4) S3 : When the first character of a field enclosed in double quotes (i.e a double quote) has been read => Handled by helper_escape_delimiters
      5) S4 : When we encounter another double quote in state S3 => Handled by helper_found_double_quotes_in_escape_delimiters
    In addition to above functions, the function check_fields_num_newline checks whether the previous line and the current line have the same number of fields. If not, the exception UnevenFields is raised
    Each of the helper function has parameters copt, line_num, fields_num_curr and fields_num_prev
    # copt is of char option and is used to read the input from the file.
    # line_num is used to keep track of the line number currently being processed.
    # fields_num_curr is used to keep track of the number of fields in the current line being read.
    # fields_num_prev is used to keep track of the number of fields in the previous line.  
      
      *)
    fun helper_escape_delimiters(copt: char option, line_num, fields_num_curr, fields_num_prev) = 
      case copt of
        NONE =>
        (TextIO.closeIn In; TextIO.closeOut Out; raise ImproperFormat)|
        SOME(c) =>
        if (c = #"\"")
        then (TextIO.output1(Out, c); helper_found_double_quotes_in_escape_delimiters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
        else (TextIO.output1(Out, c); helper_escape_delimiters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))

    and
    helper_found_double_quotes_in_escape_delimiters(copt: char option, line_num, fields_num_curr, fields_num_prev) = 
      case copt of 
        NONE =>
        (TextIO.closeIn In; TextIO.closeOut Out; raise newline_missing)|
        SOME(c) =>
        if (c = delim1)
        then (TextIO.output1(Out, delim2); helper_begin_new_field(TextIO.input1(In), line_num, fields_num_curr+1, fields_num_prev))
        else if (c = #"\"")
        then (TextIO.output1(Out, c); helper_escape_delimiters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
        else if (c = #"\n")
        then (TextIO.output1(Out, c);check_fields_num_newline(line_num+1, fields_num_curr, fields_num_prev))
        else (TextIO.closeIn In; TextIO.closeOut Out;raise ImproperFormat)
    and
    helper_find_delimiters(copt: char option, line_num, fields_num_curr, fields_num_prev) =
      case copt of
        NONE =>
        (TextIO.closeIn In; TextIO.closeOut Out; raise newline_missing)|
        SOME(c) =>
        if (c = delim1)
        then (TextIO.output1(Out, #"\"");TextIO.output1(Out,delim2); helper_begin_new_field(TextIO.input1(In), line_num, fields_num_curr+1, fields_num_prev) )
        else if (c = #"\"")
        then (TextIO.closeIn In; TextIO.closeOut Out; raise ImproperFormat)
        else if (c = #"\n")
        then (TextIO.output1(Out, #"\"");TextIO.output1(Out, c); check_fields_num_newline(line_num+1, fields_num_curr, fields_num_prev))
        else (TextIO.output1(Out,c); helper_find_delimiters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
    and
    check_fields_num_newline (line_num, fields_num_curr, fields_num_prev) = 
      if (line_num = 1)
      then (helper_start_newline(TextIO.input1(In), line_num, 1, fields_num_curr))
      else if (fields_num_curr<>fields_num_prev)
      then(TextIO.closeIn In; TextIO.closeOut Out; handle_exception(line_num, fields_num_curr, fields_num_prev))
      else (helper_start_newline(TextIO.input1(In), line_num, 1, fields_num_curr))

    and
    helper_start_newline(copt: char option, line_num, fields_num_curr, fields_num_prev) = 
      case copt of
      NONE =>
      if (line_num = 0)
      then (TextIO.closeIn In; TextIO.closeOut Out; raise emptyInputFile)
      else (TextIO.closeIn In; TextIO.closeOut Out)|
      SOME(c) =>
      if (c = #"\"")
      then (TextIO.output1(Out, c); helper_escape_delimiters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
      else if (c = delim1)
      then (TextIO.output1(Out, #"\""); TextIO.output1(Out, #"\""); TextIO.output1(Out, delim2); helper_begin_new_field(TextIO.input1(In), line_num, fields_num_curr+1, fields_num_prev))
      else if (c = #"\n")
      then (TextIO.output1(Out, #"\""); TextIO.output1(Out, #"\""); TextIO.output1(Out, c); check_fields_num_newline(line_num+1, fields_num_curr, fields_num_prev))
      else (TextIO.output1(Out, #"\""); TextIO.output1(Out,c); helper_find_delimiters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
    and
    helper_begin_new_field(copt: char option, line_num, fields_num_curr, fields_num_prev) = 
      case copt of
        NONE =>
        (TextIO.closeIn In; TextIO.closeOut Out; raise newline_missing)|
        SOME(c) =>
        if (c = #"\"")
        then (TextIO.output1(Out, c); helper_escape_delimiters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
        else if (c = delim1)
        then (TextIO.output1(Out, #"\""); TextIO.output1(Out, #"\""); TextIO.output1(Out, delim2); helper_begin_new_field(TextIO.input1(In), line_num, fields_num_curr+1, fields_num_prev))
        else if (c= #"\n")
        then (TextIO.output1(Out, #"\""); TextIO.output1(Out, #"\""); TextIO.output1(Out, c); check_fields_num_newline(line_num+1, fields_num_curr, fields_num_prev))
        else (TextIO.output1(Out, #"\""); TextIO.output1(Out,c); helper_find_delimiters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
  
  in
    helper_start_newline(TextIO.input1(In), 0, 1, 0)
  end;
  
fun csv2tsv(infilename : string, outfilename : string) = convertDelimiters(infilename, #",", outfilename, #"\t");

fun tsv2csv(infilename : string, outfilename : string) = convertDelimiters(infilename, #"\t", outfilename, #",");
