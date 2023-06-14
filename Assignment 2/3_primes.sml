datatype 'a lazyList = nullList
                    | cons of 'a * (unit -> 'a lazyList)

(* (i) *)
fun seq(first, last) = 
  let
    fun stop(n) = 
      if n>last
      then nullList
      else cons(n,fn()=>stop(n+1))
  in
    stop(first)
  end

fun infSeq(first) : int lazyList = 
  cons(first,fn()=>infSeq(first+1))

fun firstN(lazyListVal,n) (*: 'a list*) =
  case lazyListVal of
    nullList => []
    | cons(a,f) => if n=0 then [] else a::firstN(f(),n-1)

fun Nth(lazyListVal,n) : int option = 
  let
    fun findNth(intList,m,n) (*: 'a list*) = 
      if null intList
      then []
      else if m=n then [hd(intList)] else findNth(tl(intList),m+1,n)
  in
    if List.length(firstN(lazyListVal,n)) < n
    then NONE
    else if null(findNth(firstN(lazyListVal,n),1,n))
        then NONE
        else SOME(hd(findNth(firstN(lazyListVal,n),1,n)))
  end

fun filterMultiples(lazyListVal,n) (*: int lazyList*) =
  if n = 1
  then nullList
  else
    case lazyListVal of
      nullList => nullList
      | cons(a,f) => if a mod n = 0
                    then filterMultiples(f(),n)
                    else cons(a,fn()=>filterMultiples(f(), n))

(* (ii) *)
fun primes() =
  let
    fun sieve(lazyListVal) =
      case lazyListVal of
        nullList => nullList
        | cons(a,f) => cons(a,fn()=>sieve(filterMultiples(f(),a)))
  in
    sieve(infSeq(2))
  end

