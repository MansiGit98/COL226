fun Indexing (L1,L2) =
let
    fun change(IDENTIFIER m, cnt , []) = []
    | change(IDENTIFIER m, cnt , (IDENTIFIER n::Li)) = 
    if String.compare(m,n) = EQUAL
    then let val Lx = change(IDENTIFIER m, cnt , Li) in [IDENTIFIER (Int.toString(cnt))]@Lx end
    else let val Lx = change(IDENTIFIER m, cnt , Li) in [IDENTIFIER n]@Lx end
    | change(IDENTIFIER m, cnt , (xs::Li)) = let val Lx = change(IDENTIFIER m, cnt , Li) in xs::Lx end

    fun UpD([], Li, k) = Li
    | UpD((IDENTIFIER m::Li), Lo, k) = let val Lj = change(IDENTIFIER m, k, Lo) in UpD (Li, Lj, k+1) end 
in
    UpD (L1, L2, 0)
end;