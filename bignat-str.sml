structure Bignat : BIGNAT =
struct
      type bignat = int list

      exception overflow
      exception underflow
      exception division_by_zero
      exception emptyList
    
      val zero = ([0] : bignat)
    
      fun normalize [] = zero
      |   normalize ((L as h::t):bignat) = 
            if (h <> 0) then L
            else normalize (t)

      fun fromString S =
            let
                  val CL = String.explode(S)
                  fun convert (L, []) = L
                  |   convert ((L : bignat), (b:char)::M) =
                        let
                              val bn = Char.toString(b)
                              val SOME i =  Int.fromString(bn)
                        in
                              convert (i::L, M)
                        end
            in
                  List.rev(convert([], CL))
            end

      fun toString (N:bignat) =
            let
                  fun convert (L, []) = String.implode(List.rev(L))
                  |   convert ((L : char list), b::M) =
                        let
                              val bn = Int.toString(b)
                              val SOME cn = Char.fromString(bn)
                        in
                              convert (cn::L, M)
                        end
            in
                  convert([], N)
            end

      fun addLeadingZeros ((L:bignat), (n:int), (k:int)) =
            if(k >= n) then L
            else addLeadingZeros((0:int)::L, n, k+1)

      fun addZeros ((L1:bignat),(L2:bignat)) =
            let
                  val l1 = List.length(L1)
                  val l2 = List.length(L2)
            in
                  if(l1 <= l2) then (addLeadingZeros(L1,l2-l1,0),L2)
                  else (L1,addLeadingZeros(L2,l1-l2,0))
            end

      infix ++
      fun (N1:bignat) ++ (N2:bignat) =
            let
                  val (L1,L2) = addZeros(N1,N2)
                  val n1 = List.rev(L1)
                  val n2 = List.rev(L2)

                  fun add([],[],l,c) =
                        if(c=1) then ((1:int)::l)
                        else l
                  |   add([],l2,l,c) = raise emptyList
                  |   add(l1,[],l,c) = raise emptyList
                  |   add(((l1 as h1::t1):bignat),((l2 as h2::t2):bignat),(l:bignat),c) =
                        if((h1+h2+c) < 10) then add(t1,t2,((h1+h2+c)::l),0)
                        else add(t1,t2,((h1+h2+c-10)::l),1)
            in
                  add(n1,n2,[],0)
            end

      infix --
      fun (N1:bignat) -- (N2:bignat) =
            let
                  val (L1,L2) = addZeros(N1,N2)
                  val n1 = List.rev(L1)
                  val n2 = List.rev(L2)

                  fun subtract([],[],l,c) =
                        if(c=1) then raise underflow
                        else l
                  |   subtract([],l2,l,c) = raise emptyList
                  |   subtract(l1,[],l,c) = raise emptyList
                  |   subtract(((l1 as h1::t1):bignat),((l2 as h2::t2):bignat),(l:bignat),c) =
                        if((h1-(h2+c)) >= 0) then subtract(t1,t2,((h1-(h2+c))::l),0)
                        else subtract(t1,t2,(((10+h1)-(h2+c))::l),1)
            in
                  subtract(n1,n2,[],0)
            end
            
      fun succ (N:bignat) = N ++ [1]

      fun pred (N:bignat) = N -- [1]      

      fun len (N:bignat) = List.length(N)

      fun lenCompare ((N1:bignat),(N2:bignat)) =
            if(len(N1) < len(N2)) then LESS
            else if(len(N1) = len(N2)) then EQUAL
            else GREATER

      fun lenLt ((N1:bignat),(N2:bignat)) =
            if(len(N1) < len(N2)) then true
            else false

      fun lenLeq ((N1:bignat),(N2:bignat)) =
            if(len(N1) <= len(N2)) then true
            else false

      fun lenGt ((N1:bignat),(N2:bignat)) = 
            if(len(N1) > len(N2)) then true
            else false

      fun lenGeq ((N1:bignat),(N2:bignat)) =
            if(len(N1) >= len(N2)) then true
            else false

      fun lenEq ((N1:bignat),(N2:bignat)) =
            if(len(N1) = len(N2)) then true
            else false

      fun compare([],[]) = EQUAL
      |   compare([],N2) = LESS
      |   compare(N1,[]) = GREATER
      |   compare(((N1 as h1::t1):bignat),((N2 as h2::t2):bignat)) =
            if(lenCompare(normalize(N1),normalize(N2)) = LESS) then LESS
            else if(lenCompare(normalize(N1),normalize(N2)) = GREATER) then GREATER
            else if(h1 > h2) then GREATER
            else if(h1 < h2) then LESS
            else compare(t1,t2)

      infix <<
      fun (N1:bignat) << (N2:bignat) =
            if(compare(N1,N2) = LESS) then true
            else false

      infix <<=
      fun (N1:bignat) <<= (N2:bignat) =
            if(compare(N1,N2) = LESS) then true
            else if(compare(N1,N2) = EQUAL) then true
            else false

      infix >>
      fun (N1:bignat) >> (N2:bignat) =
            if(compare(N1,N2) = GREATER) then true
            else false

      infix >>=
      fun (N1:bignat) >>= (N2:bignat) =
            if(compare(N1,N2) = GREATER) then true
            else if(compare(N1,N2) = EQUAL) then true
            else false

      infix ==
      fun (N1:bignat) == (N2:bignat) =
            if(compare(N1,N2) = EQUAL) then true
            else false
      
      fun mul(L, n, [], c) = 
            if(c=0) then L
            else (c::L)
      |   mul((L:bignat), n, ((L1 as h::t):bignat), c) = 
            if(((h*n) + c) < 10) then mul((h*n+c)::L, n, t, 0)
            else mul(Int.rem((h*n)+c, 10)::L, n, t, Int.quot((h*n)+c, 10))

      fun plus([],[],l,c) =
            if(c=0) then l
            else c::l
      |   plus([],l2,l,c) = plus([c],l2,l,0)
      |   plus(l1,[],l,c) = plus(l1,[c],l,0)
      |   plus(((l1 as h1::t1):bignat),((l2 as h2::t2):bignat),(l:bignat),c) =
            if((h1+h2+c) < 10) then plus(t1,t2,((h1+h2+c)::l),0)
            else plus(t1,t2,((h1+h2+c-10)::l),1)

      infix **
      fun (N1:bignat) ** (N2:bignat) =
            let
                  fun multiply(n1,[]) = []
                  |   multiply((n1:bignat),((n2 as h2::t2):bignat)) =
                        let
                              val p = mul([],h2,n1,0)
                              val q = List.rev(p)
                        in
                              List.rev(plus(multiply(0::n1,t2), q, [], 0))
                        end
            in
                  if(normalize(N1) = zero) then zero
                  else if(normalize(N2) = zero) then zero
                  else if(lenGeq(N1,N2)) then List.rev(multiply(List.rev(N1),List.rev(N2)))
                  else List.rev(multiply(List.rev(N2),List.rev(N1)))
            end

      fun minus ((n:bignat),(m:bignat),(v:bignat)) = 
            if(n << (m**(v))) then (pred(v), n -- (m**(pred(v))))
            else if(n == (m**(v))) then (v, zero)
            else minus(n,m,succ(v))

      infix %%
      fun (N1:bignat) %% (N2:bignat) =
            let
                  fun divide([],n2,r,q) = (q,r)
                  |   divide(((n1 as h1::t1):bignat), (n2:bignat), (r:bignat), (q:bignat)) =
                        if(r@[h1] >>= n2) then
                              let
                                    val (k,l) = minus(r@[h1], n2, [1])
                              in
                                    divide(t1,n2,l,q@k)
                              end
                        else
                              divide(t1,n2,r@[h1],q@zero)
            in
                  if(normalize(N1) = zero) then (zero,zero)
                  else if(normalize(N2) = zero) then raise division_by_zero
                  else divide(N1,N2,[],[])
            end

      fun quo ((N1:bignat),(N2:bignat)) =
            let
                  val (q,r) = (N1 %% N2)
            in
                  q
            end

      fun rem ((N1:bignat),(N2:bignat)) =
            let
                  val (q,r) = (N1 %% N2)
            in
                  r
            end

      fun min ((N1:bignat),(N2:bignat)) =
            if(compare(N1,N2) = LESS orelse compare(N1,N2) = EQUAL) then N1
            else N2

      fun max ((N1:bignat),(N2:bignat)) =
            if(compare(N1,N2) = GREATER orelse compare(N1,N2) = EQUAL) then N1
            else N2

end