exception TreeException;
exception Not_Include_Read_And_Write_Command;

fun isRelOp (c :: L) = if c=LT orelse c=LEQ orelse c=GT orelse c=GEQ orelse c=EQ orelse c=NEQ then true else false;
fun isLgcOp (c :: L) = if c=AND orelse c=OR then true else false;
fun isAriOp (c :: L) = if c=PLUS orelse c=MINUS orelse c=TIMES orelse c=DIV orelse c=MOD then true else false;

fun Convert ast  = 
let 
    fun GetId (Void) = []
    | GetId (St(IDENTIFIER n, tr)) = 
    let val L1 = GetId tr 
    in [IDENTIFIER n]@L1 end
    | GetId tr = raise TreeException
   

    and Decfun (Void) = []
    | Decfun (Tt(DEC, t1, Leaf(tk), t2)) = 
    let val L1 = GetId t1 
        val L2 = Decfun t2 
    in L1@L2 end
    | Decfun tr = raise TreeException

    and BoFaFun (Leaf(IDENTIFIER m)) = [IDENTIFIER m]
    | BoFaFun (Leaf(TT)) = [NUMERAL 1]
    | BoFaFun (Leaf(FF)) = [NUMERAL 0]
    | BoFaFun (St(NOT, tr)) = 
    let val Li = BoFaFun tr 
    in Li@[NOT] end
    | BoFaFun (Bt(tk, t1, t2)) = 
    if isLgcOp([tk]) then
    let val L1 = BoFaFun t1 
        val L2 = BoFaFun t2 
    in  L1@L2@[tk] end
    else if isRelOp([tk]) then
    let val L1 = IntFun t1 
        val L2 = IntFun t2 
    in  L1@L2@[tk] end
    else raise TreeException
    | BoFaFun tr = raise TreeException


    and IntFun (Leaf(IDENTIFIER m))= [IDENTIFIER m]
    | IntFun (Leaf(NUMERAL n))= [NUMERAL n]
    | IntFun (St(NEG, tr))= let val Li = IntFun tr in Li@[NEG] end
    | IntFun (Bt(tk, t1, t2))= 
    let val L1 = IntFun t1 
        val L2 = IntFun t2 
    in if isAriOp([tk]) then L1@L2@[tk] else raise TreeException end
    | IntFun tr = raise TreeException

    and ExprFun tr = BoFaFun tr handle TreeException => (IntFun tr)
    

    and CmdTyp (Bt(SET, Leaf(IDENTIFIER m), tr)) = let val Li = ExprFun tr in [SET, IDENTIFIER m]@Li end 
    | CmdTyp (St(READ, Leaf(IDENTIFIER m))) = raise Not_Include_Read_And_Write_Command
    | CmdTyp (St(WRITE, tr))  = raise Not_Include_Read_And_Write_Command
    | CmdTyp (Tt(ITE, t1, t2, t3))  =
    let val L1 = BoFaFun t1 
        val L2 = CmdFun t2 
        val L3 = CmdFun t3 
    in ITE::L1@L2@L3 end 
    | CmdTyp (Bt(WH, t1, t2)) =
    let val L1 = BoFaFun t1 
        val L2 = CmdFun t2 
    in WH::L1@L2 end
    | CmdTyp tr = raise TreeException

    and CmdFun (Void) = []
    | CmdFun ((Bt(CMD, t1, t2))) =
    let val L1 = CmdTyp t1 
        val L2 = CmdFun t2 
    in  [CmS]@L1@[CmE]@L2 end
    | CmdFun tr = raise TreeException
    
    fun P (Bt(PROG, Leaf(IDENTIFIER m), Bt(BLK, t1, t2))) = ( Decfun t1, CmdFun t2@[EOF]  )
    | P tr = raise TreeException;
in
    P ast 
end

