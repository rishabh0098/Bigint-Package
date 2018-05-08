functor BigInt (Bn : BIGNAT) :> BIGINT =
struct

    datatype SIGN = NEG|POS

    exception division_by_zero

    type bigint = SIGN * Bn.bignat

    val bigzero = (POS, Bn.zero)

    fun normalize (N:bigint) =
        let
            val s = (#1 N)
            val n = Bn.normalize((#2 N))
        in
            (s, n)
        end

    fun bigint N =
        let
            val S = Int.toString(N)
            val L = Bn.fromString(S)
        in
            if(N < 0) then (NEG,L)
            else (POS,L)
        end

    fun fromString S =
        let
            val l = String.explode(S)
            val m = List.tl(l)
            val n = String.implode(m)
        in
            if(Char.isDigit(List.hd(l))) then (POS,Bn.fromString(S))
            else (NEG,Bn.fromString(n))
        end

    fun toString (N:bigint) =
        let
            val n = Bn.toString((#2 N))
            val s = (#1 N)
            val l = ["-"]@[n]
        in
            if(s = NEG) then String.concat(l)
            else n
        end
    
    fun int (N:bigint) =
        let
            val s = Bn.toString((#2 N))
            val SOME sp = Int.fromString(s)
        in
            Int.~(sp)
        end

    fun abs (N:bigint) = (POS,(#2 N))

    infix ++
    fun (N1:bigint) ++ (N2:bigint) =
        let
            val n1 = (#2 N1)
            val n2 = (#2 N2)
            val s1 = (#1 N1)
            val s2 = (#1 N2)
        in
            if(s1 = POS andalso s2 = POS) then (POS, Bn.++(n1,n2))
            else if(s1 = POS andalso s2 = NEG) then
                let
                    val b = Bn.compare(n1,n2)
                in
                    if(b = LESS) then (NEG,Bn.--(n2,n1))
                    else (POS,Bn.--(n1,n2))
                end
            else if(s1 = NEG andalso s2 = POS) then
                let
                    val b = Bn.compare(n1,n2)
                in
                    if(b = LESS) then (POS,Bn.--(n2,n1))
                    else (NEG,Bn.--(n1,n2))
                end
            else (NEG,Bn.++(n1,n2))
        end
    
    fun succ (N:bigint) =
        if(Bn.compare((#2 N),Bn.succ(Bn.zero)) = EQUAL andalso (#1 N) = NEG) then bigzero
        else if((#1 N) = NEG) then (NEG, Bn.pred((#2 N)))
        else (POS, Bn.succ((#2 N)))

    fun min ((N1:bigint),(N2:bigint)) =
        if((#1 N1) = POS andalso (#1 N2) = POS) then (POS, Bn.min((#2 N1),(#2 N2)))
        else if((#1 N1) = POS andalso (#1 N2) = NEG) then N2
        else if((#1 N1) = NEG andalso (#1 N2) = POS) then N1
        else (NEG, Bn.max((#2 N1),(#2 N2)))

    fun max ((N1:bigint),(N2:bigint)) =
        if((#1 N1) = POS andalso (#1 N2) = POS) then (POS, Bn.max((#2 N1),(#2 N2)))
        else if((#1 N1) = POS andalso (#1 N2) = NEG) then N1
        else if((#1 N1) = NEG andalso (#1 N2) = POS) then N2
        else (NEG, Bn.min((#2 N1),(#2 N2)))

    fun sign (N:bigint) = if((#1 N) = NEG) then ~1 else 1

    fun sameSign ((N1:bigint),(N2:bigint)) =
        if((#1 N1) = (#1 N2)) then true
        else false

    infix **
    fun (N1:bigint) ** (N2:bigint) =
        if((#1 N1) = POS andalso (#1 N2) = POS) then (POS, Bn.**((#2 N1),(#2 N2)))
        else if((#1 N1) = NEG andalso (#1 N2) = NEG) then (POS, Bn.**((#2 N1),(#2 N2)))
        else (NEG, Bn.**((#2 N1),(#2 N2)))

    fun compare ((N1:bigint),(N2:bigint)) =
        let
            val b1 = Bn.compare((#2 N1),(#2 N2))
        in
            if(sameSign(N1,N2) andalso b1 = EQUAL) then EQUAL
            else if(sign(N1) = 1 andalso sign(N2) = ~1) then b1
            else if(sign(N1) = 1 andalso sign(N2) = ~1) then GREATER
            else if(sign(N1) = ~1 andalso sign(N2) = 1) then LESS
            else if(b1 = LESS) then GREATER
            else LESS
        end

    infix <<
    fun (N1:bigint) << (N2:bigint) =
        if(compare(N1,N2) = LESS) then true
        else false

    infix <<=
    fun (N1:bigint) <<= (N2:bigint) =
        if(compare(N1,N2) = LESS orelse compare(N1,N2) = EQUAL) then true
        else false

    infix ==
    fun (N1:bigint) == (N2:bigint) =
        if(compare(N1,N2) = EQUAL) then true
        else false

    infix >>=
    fun (N1:bigint) >>= (N2:bigint) =
        if(compare(N1,N2) = GREATER orelse compare(N1,N2) = EQUAL) then true
        else false

    infix >>
    fun (N1:bigint) >> (N2:bigint) =
        if(compare(N1,N2) = GREATER) then true
        else false

    fun len (N:bigint) = Bn.len((#2 N))

    fun lenCompare ((N1:bigint),(N2:bigint)) = Bn.lenCompare((#2 N1),(#2 N2))

    fun lenLt ((N1:bigint),(N2:bigint)) = Bn.lenLt((#2 N1),(#2 N2))

    fun lenLeq ((N1:bigint),(N2:bigint)) = Bn.lenLeq((#2 N1),(#2 N2))

    fun lenEq ((N1:bigint),(N2:bigint)) = Bn.lenEq((#2 N1),(#2 N2))

    fun lenGt ((N1:bigint),(N2:bigint)) = Bn.lenGt((#2 N1),(#2 N2))

    fun lenGeq ((N1:bigint),(N2:bigint)) = Bn.lenGeq((#2 N1),(#2 N2))

    infix --
    fun (N1:bigint) -- (N2:bigint) =
        let
            val n1 = (#2 N1)
            val n2 = (#2 N2)
            val s1 = (#1 N1)
            val s2 = (#1 N2)
        in
            if(s1 = POS andalso s2 = POS) then
                let
                    val b = Bn.compare(n1,n2)
                in
                    if(b = LESS) then (NEG,Bn.--(n2,n1))
                    else (POS,Bn.--(n1,n2))
                end
            else if(s1 = POS andalso s2 = NEG) then (POS, Bn.++(n1,n2))
            else if(s1 = NEG andalso s2 = POS) then (NEG, Bn.++(n1,n2))
            else
                let
                    val b = Bn.compare(n1,n2)
                in
                    if(b = LESS) then (POS,Bn.--(n2,n1))
                    else (NEG,Bn.--(n1,n2))
                end
        end

    fun pred (N:bigint) =
        if(Bn.compare((#2 N),Bn.succ(Bn.zero)) = EQUAL andalso (#1 N) = POS) then bigzero
        else if((#1 N) = NEG) then (NEG, Bn.succ((#2 N)))
        else (POS, Bn.pred((#2 N)))

    infix %%
    fun (N1:bigint) %% (N2:bigint) =
        let
            val (q,r) = Bn.%%((#2 N1),(#2 N2))
        in
            if((#1 N1) = POS andalso (#1 N2) = POS) then ((POS, q),(POS, r))
            else if((#1 N1) = NEG andalso (#1 N2) = NEG) then ((POS, q),(POS, r))
            else ((NEG, Bn.succ(q)),(POS, Bn.--((#2 N2),r)))
        end

    fun ~~ (N:bigint) =
        if((#1 N) = NEG) then (POS,(#2 N))
        else (NEG,(#2 N))

    infix div
    fun (N1:bigint) div (N2:bigint) =
        let
            val (q,r) = N1 %% N2
        in
            q
        end

    infix mod
    fun (N1:bigint) mod (N2:bigint) =
        let
            val (q,r) = N1 %% N2
        in
            r
        end

    fun quo ((N1:bigint),(N2:bigint)) =
        let
            val (q,r) = N1 %% N2
        in
            q
        end

    fun rem ((N1:bigint),(N2:bigint)) =
        let
            val (q,r) = N1 %% N2
        in
            r
        end
end