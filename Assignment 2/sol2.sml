(* 1. Simple Eval (10 pts) *)

datatype expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr

datatype formula = TRUE
                | FALSE
                | NOT of formula
                | ANDALSO of formula * formula
                | ORELSE of formula * formula
                | IMPLY of formula * formula
                | LESS of expr * expr

fun eval(f:formula) : bool =
    let
        fun ex(i:expr) : int = 
            case i of
                NUM(pi:int) => pi
                | PLUS(i1,i2) => ex(i1)+ex(i2)
                | MINUS(i1,i2) => ex(i1)-ex(i2)
    in
        case f of
            TRUE => true
            | FALSE => false
            | NOT(f) => if eval(f) then false else true
            | ANDALSO(f1,f2) => if eval(f1) then eval(f2) else false
            | ORELSE(f1,f2) => if eval(f1) then true else eval(f2)
            | IMPLY(f1,f2) => if eval(f1) then eval(f2) else true
            | LESS(e1,e2) => if ex(e1)<ex(e2) then true else false
    end

(* 2. Check MetroMap *)

type name = string
datatype metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro

fun checkMetro(m:metro) =
    let
        val areaList = []
        fun find(alist, station) = 
            if null alist
            then false
            else if hd(alist) = station
                then true
                else find(tl(alist), station)
        fun check(metro, alist) = 
            case metro of
                STATION(n) => find(alist, n)
                | AREA(n,m) => check(m, n::alist)
                | CONNECT(m1, m2) => check(m1, alist) andalso check(m2, alist)
    in
        check(m, areaList)
    end


(* 3. Lazy List (40 pts) *)

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