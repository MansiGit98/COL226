datatype Token = PROG | INT  | BOOL | TT | FF | NEG | AND | OR | LT | LEQ  | EQ  | GT | GEQ | NEQ | PLUS | MINUS | TIMES | DIV | MOD | SET
                | BLK | EOS | VAR | COLON | COMMA | LCURLY | RCURLY | LPAREN | RPAREN | READ | WRITE | IF | THEN | ELSE | ENDIF | WHILE | CMD
                | DO | ENDWH | NOT | NUMERAL of int | IDENTIFIER of string | ITE | WH | DEC | CmS | CmE | EOF;

exception LexerException;

fun tokNum num (c::S) = if Char.isDigit(c) then tokNum (num*10+(ord(c)-ord(#"0"))) S else (num,(c::S))
  | tokNum num [] = (num, []);

fun tokSTR str (c::S) = if Char.isAlpha(c) then tokSTR (str^Char.toString(c)) S else (str,(c::S))
  | tokSTR str [] = (str, []);


fun compare ([], []) = true
    |   compare (x::xs, y::ys) = (x = y) andalso compare(xs,ys)
    |   compare (_, _) = false

fun makeToken [] = [EOF]
    | makeToken (#"~"::S) =  NEG::(makeToken S)
    | makeToken (#"="::S) =  EQ::(makeToken S)
    | makeToken (#"+"::S) =  PLUS::(makeToken S)
    | makeToken (#"-"::S) =  MINUS::(makeToken S)
    | makeToken (#"*"::S) =  TIMES::(makeToken S)
    | makeToken (#"/"::S) =  DIV::(makeToken S)
    | makeToken (#"%"::S) =  MOD::(makeToken S)
    | makeToken (#";"::S) =  EOS::(makeToken S) 
    | makeToken (#","::S) =  COMMA::(makeToken S)
    | makeToken (#"{"::S) =  LCURLY::(makeToken S)
    | makeToken (#"}"::S) =  RCURLY::(makeToken S)
    | makeToken (#"("::S) =  LPAREN::(makeToken S)
    | makeToken (#")"::S) =  RPAREN::(makeToken S)
    | makeToken (#"!"::S) =  NOT::(makeToken S)
    | makeToken (#"<"::S) = if compare(List.take(S, 1),  String.explode("=")) then LEQ::(makeToken (List.drop(S, 1))) 
                            else if compare(List.take(S, 1),  String.explode(">")) then NEQ::(makeToken (List.drop(S, 1))) else LT::(makeToken S)
    | makeToken (#">"::S) = if compare(List.take(S, 1),  String.explode("=")) then GEQ::(makeToken (List.drop(S, 1))) else GT::(makeToken S)
    | makeToken (#":"::S) = if compare(List.take(S, 1),  String.explode("=")) then SET::(makeToken (List.drop(S, 1))) 
                            else if compare(List.take(S, 1),  String.explode(":")) then BLK::(makeToken (List.drop(S, 1))) else COLON::(makeToken S)
    | makeToken (#"|"::S) = if compare(List.take(S, 1),  String.explode("|")) then OR::(makeToken (List.drop(S, 1))) else raise LexerException
    | makeToken (#"&"::S) = if compare(List.take(S, 1),  String.explode("&")) then AND::(makeToken (List.drop(S, 1))) else raise LexerException
    | makeToken (c::S) = 
    if Char.isSpace(c) orelse (c = #"\n") orelse (c = #"\t") then makeToken S 
    else if List.length(S) > 5 andalso  compare(List.take((c::S), 7),  String.explode("program")) then PROG::(makeToken (List.drop((c::S), 7)))
    else if List.length(S) > 0 andalso  compare(List.take((c::S), 2),  String.explode("if")) then IF::(makeToken (List.drop((c::S), 2)))
    else if List.length(S) > 1 andalso  compare(List.take((c::S), 3),  String.explode("int")) then INT::(makeToken (List.drop((c::S), 3)))
    else if List.length(S) > 2 andalso  compare(List.take((c::S), 4),  String.explode("bool")) then BOOL::(makeToken (List.drop((c::S), 4)))
    else if List.length(S) > 3 andalso  compare(List.take((c::S), 5),  String.explode("while")) then WHILE::(makeToken (List.drop((c::S), 5)))
    else if List.length(S) > 3 andalso  compare(List.take((c::S), 5),  String.explode("write")) then WRITE::(makeToken (List.drop((c::S), 5)))
    else if List.length(S) > 2 andalso  compare(List.take((c::S), 4),  String.explode("then")) then THEN::(makeToken (List.drop((c::S), 4)))
    else if List.length(S) > 0 andalso  compare(List.take((c::S), 2),  String.explode("tt")) then TT::(makeToken (List.drop((c::S), 2)))
    else if List.length(S) > 0 andalso  compare(List.take((c::S), 2),  String.explode("ff")) then FF::(makeToken (List.drop((c::S), 2))) 
    else if List.length(S) > 3 andalso  compare(List.take((c::S), 5),  String.explode("endif")) then ENDIF::(makeToken (List.drop((c::S), 5)))
    else if List.length(S) > 3 andalso  compare(List.take((c::S), 5),  String.explode("endwh")) then ENDWH::(makeToken (List.drop((c::S), 5)))
    else if List.length(S) > 2 andalso  compare(List.take((c::S), 4),  String.explode("else")) then ELSE::(makeToken (List.drop((c::S), 4)))
    else if List.length(S) > 1 andalso  compare(List.take((c::S), 3),  String.explode("var")) then VAR::(makeToken (List.drop((c::S), 3)))
    else if List.length(S) > 2 andalso  compare(List.take((c::S), 4),  String.explode("read")) then READ::(makeToken (List.drop((c::S), 4)))
    else if List.length(S) > 0 andalso  compare(List.take((c::S), 2),  String.explode("do")) then DO::(makeToken (List.drop((c::S), 2)))
    else if Char.isDigit(c) then
	    let 
		    val (num, rest) = tokNum 0 (c::S)
	    in
		    (NUMERAL num) :: (makeToken rest)
	    end
    else if Char.isAlpha(c) then
	    let 
		    val (str, rest) = tokSTR "" (c::S)
	    in
		    (IDENTIFIER str) :: (makeToken rest)
	    end
    else raise LexerException;
