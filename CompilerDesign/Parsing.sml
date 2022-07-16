datatype AST = Void | Leaf of Token | St of (Token * AST) | Bt of (Token * AST * AST) | Tt of (Token * AST * AST * AST);

exception ParserException;

fun isIDT (IDENTIFIER a :: L) = true
    | isIDT L = false;

fun isNUM (NUMERAL a :: L) = true
    | isNUM L = false;

fun isRelOp (c :: L) = if c=LT orelse c=LEQ orelse c=GT orelse c=GEQ orelse c=EQ orelse c=NEQ then true else false;

fun isLgcOp (c :: L) = if c=AND orelse c=OR then true else false;

fun isAriOp (c :: L) = if c=PLUS orelse c=MINUS orelse c=TIMES orelse c=DIV orelse c=MOD then true else false;

(* P - program, DS - DeclarationSeq, CS - Command sequence, VR-Variable, VL-Variable list, TY-Types, CS-CommandSeq, CL-CmmdList*)

fun Parse list =
let
    fun TY (INT::L) = (Leaf(INT), L)
        | TY (BOOL::L) = (Leaf(BOOL), L)
        | TY L = raise ParserException

    and VR (COMMA :: L) = let val (tree, L1) = VR (List.drop(L, 1)) (*drop of comma*)
                        in if isIDT(L) then (St(List.nth(L, 0), tree), L1) else raise ParserException end
        | VR (COLON :: L) = (Void, (COLON :: L))
	    | VR L = raise ParserException

    
    and VL (IDENTIFIER m :: L) = let val (tree, L1) = VR L in (St(IDENTIFIER m, tree), L1) end
        | VL L = raise ParserException

    and DS (VAR::L) = let val (tree1, L1) = VL L
                          val (tree2, L2) = TY (List.drop(L1, 1))        (*drop of colon*)
                          val (tree3, L3) = DS (List.drop(L2, 1))        (*drop of eos i.e ; *)
                    in if isIDT(L) andalso List.nth(L1, 0) = COLON andalso List.nth(L2, 0) = EOS then (Tt(DEC , tree1, tree2, tree3), L3) else raise ParserException end
        | DS (LCURLY::L) = (Void, (LCURLY::L))
	    | DS L = raise ParserException

    and IntF (NUMERAL n::L) = (Leaf(NUMERAL n), L)
    | IntF (IDENTIFIER m::L) = (Leaf(IDENTIFIER m), L)
    | IntF (LPAREN::L) = let val (tree, L1) = IntEXP L in if List.nth(L1, 0) = RPAREN then (tree, List.drop(L1, 1)) else raise ParserException end
    | IntF (NEG::L) = let val (tree, L1) = IntF L in (St(NEG, tree), L1) end
    | IntF L =  raise ParserException

    and IntEXP L = let val (tree1, L1) = IntF L                       (*drop of arithmetic OP i.e ; *)
                    in if isAriOp(L1) then let val (tree2, L2) = IntF (List.drop(L1, 1)) in (Bt(List.nth(L1, 0), tree1, tree2), L2) end
                    else if List.nth(L1, 0) = EOS orelse List.nth(L1, 0) = RPAREN orelse isRelOp(L1) orelse isLgcOp(L1) then (tree1, L1)
                    else raise ParserException end


    and Compare L = let val (tree1, L1) = IntEXP L
                    val (tree2, L2) = IntEXP (List.drop(L1, 1))  (*drop of relational operator*)
                    in if isRelOp(L1) then (Bt(List.nth(L1, 0), tree1, tree2), L2)
                    else raise ParserException end

    and BoF (TT::L) = (Leaf(TT), L)
    | BoF (FF::L) = (Leaf(FF), L)
    | BoF (IDENTIFIER m::L) = (Leaf(IDENTIFIER m), L)
    | BoF (LPAREN::L) = let val (tree, L1) = BOOLEXP L in if List.nth(L1, 0) = RPAREN then (tree, List.drop(L1, 1)) else raise ParserException end
    | BoF (NOT::L) =  let val (tree, L1) = BoF L in (St(NOT, tree), L1) end
    | BoF L =  raise ParserException

    and BolTerm L = Compare L handle ParserException => (BoF L)

    and BOOLEXP L = let val (tree1, L1) = BolTerm L                   (*drop of logic OP i.e ; *)
                    in if isLgcOp(L1) then let  val (tree2, L2) = BolTerm (List.drop(L1, 1)) in (Bt(List.nth(L1, 0), tree1, tree2), L2) end
                    else if List.nth(L1, 0) = THEN orelse  List.nth(L1, 0) = DO orelse List.nth(L1, 0) = RPAREN orelse  List.nth(L1, 0) = EOS then (tree1, L1)
                    else raise ParserException end      

    and EXP L = BOOLEXP L handle ParserException => (IntEXP L)

    and Cmd (IDENTIFIER m:: L) = let val (tree, L1) = EXP (List.drop(L, 1)) (*drop of :=*)
                                in if List.nth(L, 0) = SET then(Bt(SET, Leaf(IDENTIFIER m), tree), L1) else raise ParserException end
        | Cmd (READ:: L) = if isIDT(L) then (St(READ, Leaf(List.nth(L, 0))), List.drop(L, 1)) else raise ParserException 
        | Cmd (WRITE:: L) = let val (tree, L1) = IntEXP L in (St(WRITE, tree), L1) end
        | Cmd (IF:: L) = let val (tree1, L1) = BOOLEXP L
                          val (tree2, L2) = CS (List.drop(L1, 1))        (*drop of then*)
                          val (tree3, L3) = CS (List.drop(L2, 1))        (*drop of else*)
                        in if List.nth(L1, 0) = THEN andalso List.nth(L2, 0) = ELSE andalso  List.nth(L3, 0) = ENDIF then (Tt(ITE, tree1, tree2, tree3), List.drop(L3, 1)) else raise ParserException end
        | Cmd (WHILE:: L) = let val (tree1, L1) = BOOLEXP L
                          val (tree2, L2) = CS (List.drop(L1, 1))        (*drop of do*)
                        in if List.nth(L1, 0) = DO andalso List.nth(L2, 0) = ENDWH then (Bt(WH, tree1, tree2), List.drop(L2, 1)) else raise ParserException end
        | Cmd L = raise ParserException

    and CL (RCURLY::L) = (Void, (RCURLY::L))
        | CL L = let val (tree1, L1) = Cmd L
                     val (tree2, L2) = CL (List.drop(L1, 1))        (*drop of eos*)
                    in if List.nth(L1, 0) = EOS then (Bt(CMD, tree1, tree2), L2) else raise ParserException end

    and CS (LCURLY::L) =  let val (tree, L1) = CL L in if List.nth(L1, 0) = RCURLY then (tree, List.drop(L1, 1)) else raise ParserException end
   
    fun P [] = (Void, [])
    | P (PROG::L) = let val (tree1, L1) = DS (List.drop(L, 2)) 
                        val (tree2, L2) = CS L1
                    in if isIDT(L) andalso List.nth(L, 1) = BLK andalso List.nth(L2, 0) = EOF then  (Bt(PROG , Leaf(List.nth(L, 0)), Bt(BLK, tree1, tree2)), L2) else raise ParserException end
	| P L = raise ParserException;
in
    P list
end


