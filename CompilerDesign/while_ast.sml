use "Tokenise.sml";
use "Parsing.sml";
use "Converter.sml";
use "Memory.sml";
use "VMC_Machine.sml";

(* program.txt file contain the program for which we have to create AST*)

fun String input = (TextIO.inputAll input); 
val stream = TextIO.openIn "program.txt";
val a = String stream;
val list = makeToken(String.explode(a));

fun create expr =
let
	val (ast,L) = Parse expr
in
	ast
end;

fun postfix ast =
let
	val (L1,L2) = Convert ast
in
	(L1,L2)
end;

val AbSyT = create(list); (*AbSyT is abstract syntax tree*)
val (List1, List2) = postfix(AbSyT);
val List2 = Indexing (List1, List2);
val Old_Memory_Array_Values = M;
val RunProgram = execute(List2);
val New_Memory_Array_Values = M;



