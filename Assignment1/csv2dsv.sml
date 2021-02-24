fun convertDelimiters(infilename: string, delim1, outfilename: string, delim2) =
  let
    val In = TextIO.openIn infilename
    val Out = TextIO.openOut outfilename
    exception ImproperFormat
    exception newline_missing
    exception emptyInputFile
    exception UnevenFields of string
    
    fun raise_exception(line_num, fields_num_curr, fields_num_prev) =
      raise UnevenFields("Expected: "^Int.toString(fields_num_prev)^" fields, Present: "^Int.toString(fields_num_curr)^" fields on Line "^Int.toString(line_num)^"\n")

    fun handle_exception(line_num, fields_num_curr, fields_num_prev) = 
      raise_exception(line_num, fields_num_curr, fields_num_prev)
      handle UnevenFields s => print(s)

    fun helper_escape_delimeters(copt: char option, line_num, fields_num_curr, fields_num_prev) = 
      case copt of
        NONE =>
        (TextIO.closeIn In; TextIO.closeOut Out; raise ImproperFormat)|
        SOME(c) =>
        if (c = #"\"")
        then (TextIO.output1(Out, c); helper_found_double_quotes_in_escape_delimeters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
        else (TextIO.output1(Out, c); helper_escape_delimeters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))

    and
    helper_found_double_quotes_in_escape_delimeters(copt: char option, line_num, fields_num_curr, fields_num_prev) = 
      case copt of 
        NONE =>
        (TextIO.closeIn In; TextIO.closeOut Out; raise newline_missing)|
        SOME(c) =>
        if (c = delim1)
        then (TextIO.output1(Out, delim2); helper_begin_new_field(TextIO.input1(In), line_num, fields_num_curr+1, fields_num_prev))
        else if (c = #"\"")
        then (TextIO.output1(Out, c); helper_escape_delimeters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
        else if (c = #"\n")
        then (TextIO.output1(Out, c);check_fields_num_newline(line_num+1, fields_num_curr, fields_num_prev))
        else (TextIO.closeIn In; TextIO.closeOut Out;raise ImproperFormat)
    and
    helper_find_delimeters(copt: char option, line_num, fields_num_curr, fields_num_prev) =
      case copt of
        NONE =>
        (TextIO.closeIn In; TextIO.closeOut Out; raise ImproperFormat)|
        SOME(c) =>
        if (c = delim1)
        then (TextIO.output1(Out, #"\"");TextIO.output1(Out,delim2); helper_begin_new_field(TextIO.input1(In), line_num, fields_num_curr+1, fields_num_prev) )
        else if (c = #"\"")
        then (TextIO.closeIn In; TextIO.closeOut Out; raise ImproperFormat)
        else if (c = #"\n")
        then (TextIO.output1(Out, #"\"");TextIO.output1(Out, c); check_fields_num_newline(line_num+1, fields_num_curr, fields_num_prev))
        else (TextIO.output1(Out,c); helper_find_delimeters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
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
      then (TextIO.output1(Out, c); helper_escape_delimeters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
      else if (c = delim1)
      then (TextIO.output1(Out, #"\""); TextIO.output1(Out, #"\""); TextIO.output1(Out, delim2); helper_begin_new_field(TextIO.input1(In), line_num, fields_num_curr+1, fields_num_prev))
      else if (c = #"\n")
      then (TextIO.output1(Out, #"\""); TextIO.output1(Out, #"\""); TextIO.output1(Out, c); check_fields_num_newline(line_num+1, fields_num_curr, fields_num_prev))
      else (TextIO.output1(Out, #"\""); TextIO.output1(Out,c); helper_find_delimeters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
    and
    helper_begin_new_field(copt: char option, line_num, fields_num_curr, fields_num_prev) = 
      case copt of
        NONE =>
        (TextIO.closeIn In; TextIO.closeOut Out; raise ImproperFormat)|
        SOME(c) =>
        if (c = #"\"")
        then (TextIO.output1(Out, c); helper_escape_delimeters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
        else if (c = delim1)
        then (TextIO.output1(Out, #"\""); TextIO.output1(Out, #"\""); TextIO.output1(Out, delim2); helper_begin_new_field(TextIO.input1(In), line_num, fields_num_curr+1, fields_num_prev))
        else if (c= #"\n")
        then (TextIO.output1(Out, #"\""); TextIO.output1(Out, #"\""); TextIO.output1(Out, c); check_fields_num_newline(line_num+1, fields_num_curr, fields_num_prev))
        else (TextIO.output1(Out, #"\""); TextIO.output1(Out,c); helper_find_delimeters(TextIO.input1(In), line_num, fields_num_curr, fields_num_prev))
  
  in
    helper_start_newline(TextIO.input1(In), 0, 1, 0)
  end;
  
fun csv2tsv(infilename : string, outfilename : string) = convertDelimiters(infilename, #",", outfilename, #"\t");

fun tsv2csv(infilename : string, outfilename : string) = convertDelimiters(infilename, #"\t", outfilename, #",");
(* convertDelimiters("himym_uneven.csv", #",", "check.csv", #"|");
csv2tsv("himym.csv", "out");
tsv2csv("out", "inp.csv"); *)


