val In = TextIO.openIn "inp1";
val text  = TextIO.inputAll(In);
val char_list = explode(text);
TextIO.closeIn(In);
val Out = TextIO.openOut "out";
val str1 = "hello";
val str2 = "hi";
val pre1 = explode(str1);
val pre2 = explode(str2);

fun fn_drop(char_list : char list, n : int) = 
  if (n=0)
  then (char_list)
  else (fn_drop(tl(char_list), n-1))

fun check_prefix(char_list : char list, prefix : char list) =
  if (length(prefix)=0)
  then true
  else if (length(char_list)<length(prefix))
  then false
  else if (hd(char_list) = hd(prefix))
  then (check_prefix(tl(char_list), tl(prefix)))
  else false

fun find_and_replace_prefix(char_list : char list, prefix : char list, replace_with_prefix: char list) =
  let
    val len1 = length(prefix)
  in
    if (length(char_list)=0)
    then (char_list)
    else if (check_prefix(char_list, prefix) = true)
    then (replace_with_prefix @ find_and_replace_prefix(fn_drop(char_list, len1), prefix, replace_with_prefix))
    else (hd(char_list)::find_and_replace_prefix(tl(char_list), prefix, replace_with_prefix))
  end;
val out_string = implode(find_and_replace_prefix(char_list, pre1, pre2));
TextIO.output(Out, out_string);
TextIO.closeOut Out;


