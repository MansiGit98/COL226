val M = Array.array(100, 0);  (*M is Memory Array, here values of all variable are stored in their respective array index *)

exception Unfined_Variable_Used;
exception CommandError;

signature STACK = 
    sig
    type 'a stack
    exception EmptyStack
    val create : 'a stack
    val push : ('a * 'a stack) -> 'a stack
    val pop : 'a stack -> 'a stack
    val top : 'a stack -> 'a
    val empty: 'a stack -> bool
    val poptop : 'a stack -> ('a * 'a stack) option
    val nth : 'a stack * int -> 'a
    val drop : 'a stack * int -> 'a stack
    val depth : 'a stack -> int
    val app : ('a -> unit) -> 'a stack -> unit
    val map : ('a -> 'b) -> 'a stack -> 'b stack
    val mapPartial : ('a -> 'b option) -> 'a stack -> 'b stack
    val find : ('a -> bool) -> 'a stack -> 'a option
    val filter : ('a -> bool) -> 'a stack -> 'a stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
    val exists : ('a -> bool) -> 'a stack -> bool
    val all : ('a -> bool) -> 'a stack -> bool
    val list2stack : 'a list -> 'a stack (* Convert a list into a stack *)
    val stack2list: 'a stack -> 'a list (* Convert a stack into a list *)
    val toString: ('a -> string) -> 'a stack -> string
end 

(*Implemention of Stack signature*)

structure FunStack :> STACK = 
    struct
    type 'a stack = 'a list
    exception EmptyStack
    val create = []
    fun push (x, l) = x::l
    fun pop l = 
        (case l of 
        [] => raise Empty
        | (x::xs) => xs)
    fun top l = 
        (case l of
        [] => raise Empty
        | (x::xs) => x)
    fun empty [] = true
        | empty _ = false
    fun poptop l = List.getItem(l)
    fun nth (l, n) = List.nth(l, n)        (*What if l not contain n number of element*)
    fun drop (l,n) = List.drop(l, n)               (*returns what is left after dropping top n elements of the stack*)
    fun depth l = List.length(l)
    fun app f l = List.app f l
    fun map f l = List.map f l
    fun mapPartial f l = List.mapPartial f l
    fun find p l = List.find p l
    fun filter p l = List.filter p l
    fun foldr f e l = List.foldr f e l
    fun foldl f e l = List.foldl f e l
    fun exists p l = List.exists p l
    fun all p l = List.all p l
    fun list2stack l = l
    fun stack2list l = l
    fun toString f l =
    let 
        val newStack = map f l
        val li = stack2list(newStack)
        fun ToSt [] = ""
        | ToSt (x::L) = let val s = ToSt(L) in x^" "^s end
    in ToSt(li) end
end

(*Defining VMC Signature for VMC Machine*)
signature VMC = sig
    structure V : STACK
    structure C : STACK
    val RULE1: int V.stack * Token C.stack -> int V.stack * Token C.stack
    val RULE2: int V.stack * Token C.stack -> int V.stack * Token C.stack
    val RULE3: int V.stack * Token C.stack -> int V.stack * Token C.stack
    val RULE4: int V.stack * Token C.stack -> int V.stack * Token C.stack
    val RULE5: int V.stack * Token C.stack -> int V.stack * Token C.stack
    val RULE6: int V.stack * Token C.stack -> unit * int V.stack * Token C.stack
    val RULE7: Token C.stack * int -> Token C.stack
    val RULE8: Token C.stack * int -> Token list
    val RULE9: Token list * Token C.stack -> Token C.stack
    val C_toString: Token C.stack -> string
    val V_toString: int V.stack -> string
end 

(*Implemention of VMC signatue*)
structure Vmc : VMC = struct
    structure V : STACK =  FunStack
    structure C : STACK =  FunStack
    fun RULE1 (vst, cst) = let val NUMERAL n = C.top(cst) in (V.push(n,vst),C.pop(cst)) end;  (* Rule 1 store numeral from stack C to stack V*)
    fun RULE2 (vst, cst) =  (* Rule 2 store pop variable from stack C and store it value to stack V*)
    let val IDENTIFIER x = C.top(cst)
        val n = 
        case Int.fromString(x) of
        SOME n => n|
        NONE => raise Unfined_Variable_Used
    in (V.push((Array.sub(M, n)),vst),C.pop(cst)) end

    fun RULE3 (vst, cst) =   (* rule 3 operate for unary operator like NEG(~) and NOT*)
    let 
        val s1 = C.pop(cst)
        val n1 = V.top(vst)
        val s2 = V.pop(vst)
    in case C.top(cst) of
        NOT => if n1 = 0 then (V.push(1,s2), s1) else (V.push(0,s2), s1)
        | NEG => (V.push(~n1,s2), s1) 
    end

    fun RULE4 (vst, cst) =   (* rule 4 operate for binary operators*)
    let 
        val s1 = C.pop(cst)
        val n2 = V.top(vst)
        val s2 = V.pop(vst)
        val n1 = V.top(s2)
        val s3 = V.pop(s2)
    in case C.top(cst) of
        LT => if n1 < n2 then (V.push(1,s3), s1) else (V.push(0,s3), s1)
        | LEQ => if n1 <= n2 then (V.push(1,s3), s1) else (V.push(0,s3), s1)
        | GT => if n1 > n2 then (V.push(1,s3), s1) else (V.push(0,s3), s1)
        | GEQ => if n1 >= n2 then (V.push(1,s3), s1) else (V.push(0,s3), s1)
        | EQ => if n1 = n2 then (V.push(1,s3), s1) else (V.push(0,s3), s1)
        | NEQ => if n1 <> n2 then (V.push(1,s3), s1) else (V.push(0,s3), s1)
        | OR => if (n1=0) andalso (n2=0) then (V.push(0,s3), s1) else (V.push(1,s3), s1)
        | AND => if (n1=0) orelse (n2=0) then (V.push(0,s3), s1) else (V.push(1,s3), s1)
        | PLUS => (V.push((n1 + n2),s3), s1)
        | MINUS => (V.push((n1 - n2),s3), s1)
        | TIMES => (V.push((n1*n2),s3), s1)
        | MOD => (V.push((n1 mod n2),s3), s1)
        | DIV => (V.push((n1 div n2),s3), s1)
    end

    fun RULE5 (vst,cst) =     (* rule 5 used for expression calculation*)
    case FunStack.top(cst) of
    CmE => (vst,cst)
    | CmS => (vst,cst)
    | NUMERAL n => let val (v,c) = RULE1(vst,cst) in RULE5(v,c) end
    | IDENTIFIER m => let val (v,c) = RULE2(vst,cst) in RULE5(v,c) end
    | NEG =>  let val (v,c) = RULE3(vst,cst) in RULE5(v,c) end
    | NOT =>  let val (v,c) = RULE3(vst,cst) in RULE5(v,c) end
    | LT =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | LEQ =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | GT =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | GEQ =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | EQ =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | NEQ =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | OR =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | AND =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | PLUS =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | MINUS =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | TIMES =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | DIV =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end
    | MOD =>  let val (v,c) = RULE4(vst,cst) in RULE5(v,c) end

    fun RULE6 (vst,cst) =   (* rule 6 used for SET operation*)
    let 
        val IDENTIFIER x = C.top(cst) 
        val cs = C.pop(cst)
        val (vs,c) = RULE5(vst, cs)
        val m = V.top(vs)
        val v = V.pop(vs)
        val n = 
        case Int.fromString(x) of
        SOME n => n|
        NONE => raise Unfined_Variable_Used
    in (Array.update(M, n, m), v, c) end

    fun RULE7 (cst,k) =
    let 
        val vl = C.top(cst)
        val cs = C.pop(cst)                              (**)
    in case vl of 
        CmS => RULE7(cs, k+1)
        | CmE => if k=1 then cs else RULE7(cs, k-1)
        | _ => RULE7(cs, k)  end

    fun RULE8 (cst,k) =
    let 
        val vl = C.top(cst)
        val cs = C.pop(cst)                              (**)
    in case vl of 
        CmS => let val L1 = RULE8(cs, k+1) in L1@[CmS] end
        | CmE => if k=1 then [CmE] else let val L1 = RULE8(cs, k-1) in L1@[CmE] end
        | _ => let val L1 = RULE8(cs, k) in L1@[vl] end  end

    fun RULE9 ([], cst) = cst
        | RULE9 (x::L, cst) = let val cs = C.push(x,cst) in RULE9(L,cs) end 

    fun C_toString cst =               (* This function used for print Conrol stack C*)
    let 
        fun ToS k =
    case k of
    IDENTIFIER m => " VariableIndex("^ m ^ ") "
    | NUMERAL n => Int.toString(n)
    | NOT => " NOT "
    | AND => " AND "
    | OR => " OR "
    | LT => " LT "
    | LEQ => " LEQ "
    | GT => " GT "
    | GEQ => " GEQ "
    | EQ => " EQ "
    | NEQ => " NEQ "
    | PLUS => " PLUS "
    | MINUS => " MINUS "
    | TIMES => " TIMES "
    | DIV => " DIV "
    | MOD => " MOD "
    | NEG => " NEG "
    | SET => " SET "
    | ITE => " ITE "
    | WH => " WH "
    | CmS => "CmS "
    | CmE => " CmE "
    | EOF => " EOF "

    val newStack = C.map ToS cst
    val li = C.stack2list(newStack)
    fun CtS [] = ""
    | CtS (x::L) = let val s = CtS(L) in x^s end
    in CtS(li) end

    fun V_toString vst =                  (* This function used for print Value stack V*)
    let 
        fun IoS k = Int.toString(k)
        val newStack = V.map IoS vst
        val li = V.stack2list(newStack)
        fun CtS [] = ""
        | CtS (x::L) = let val s = CtS(L) in x^" "^s end
    in CtS(li) end

end

fun execute Li =
let
    fun CmndS (vst, cst) =
    let 
        val SOME (s1, cs) = FunStack.poptop(cst)    (*s1 is Commmand type*)
    in 
    if (s1 = SET) then let val (a,b,c) = Vmc.RULE6 (vst, cs) in  (b,c) end
    else if (s1 = ITE) then 
        let val (a,b) = Vmc.RULE5(vst, cs) 
            val SOME(l, vs1) = FunStack.poptop(a)
        in if l = 0 then let val cs1 = Vmc.RULE7(b,0) in NewCmd(vs1, cs1) end
           else let val (vs2,cn) = NewCmd(vs1, b) 
                    val cs2 = Vmc.RULE7(cn,0) in (vs2, cs2) end
        end
    else if (s1 = WH) then 
        let val (a,b) = Vmc.RULE5(vst, cs) 
            val SOME(l, vs1) = FunStack.poptop(a)
        in if l = 0 then let val cs1 = Vmc.RULE7(b,0) in (vs1,cs1) end
            else let
                val Li = Vmc.RULE8(cst,0)
                val (vs,cs) = NewCmd(vst, b)
                val cs1 = Vmc.RULE9(Li, cs) 
            in CmndS(vs, cs1) end
        end
    else (vst, cst)
    end

    and NewCmd (vst, cst) = 
    let
        val SOME(x, cs) = FunStack.poptop(cst)
        val (vs, cs1) = CmndS(vst, cs)
        val SOME(y, cs2) = FunStack.poptop(cs1)
    in if x = CmS andalso y = CmE then (vs, cs2) else raise CommandError end

    fun CmdSeq (vst,cst) =
    let val (v,c) = NewCmd(vst,cst)
        val x = FunStack.top(c)
    in if x = EOF then "Program execution succesfull" else CmdSeq(v,c) end

    val v = FunStack.create
    val c = FunStack.list2stack(Li)

in CmdSeq(v,c) end

