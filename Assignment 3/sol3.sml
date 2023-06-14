datatype pattern = Wildcard | Variable of string | UnitP
                | ConstP of int | TupleP of pattern list
                | ConstructorP of string * pattern
datatype valu = Const of int | Unit | Tuple of valu list
                | Constructor of string * valu

fun fold (f,acc,xs)= 
   case xs of
      [] => acc
      | x::xs' => fold(f, f(x), xs')

(* 1. check_pat (20pts) *)
fun check_pat(p: pattern) : bool =
    let
        val sl = []
        fun help(pat : pattern) : string list =
            case pat of
                Wildcard => sl
                | Variable(s) => s::sl
                | UnitP => sl
                | ConstP(i) => sl
                | TupleP(pl) => fold (help, sl, pl)
                | ConstructorP(s,p) => (s::help(p)) @ sl
            
        fun is_repeat(sl : string list) : bool =
            if null sl
            then false
            else if List.exists (fn y => (hd(sl)=y)) (tl(sl))
                then true
                else is_repeat(tl(sl))
    in
        if is_repeat(help(p))
        then false
        else true
    end

(* 2. match(20 pts) *)
fun match (v, p) =
  let
    fun helper (ConstructorP (s1, p), Constructor (s2, v)) =
          if s1 = s2 then match (v, p) else NONE
      | helper (TupleP ps, Tuple vs) =
          if List.length ps = List.length vs then
            let
              val bindings = List.mapPartial (fn (p, v) => match (v, p)) (ListPair.zip (ps, vs))
            in
              if List.length bindings = List.length ps then SOME (List.concat bindings) else NONE
            end
          else NONE
      | helper (Wildcard, _) = SOME []
      | helper (Variable s, v) = SOME [(s, v)]
      | helper (ConstP i, Const j) = if i = j then SOME [] else NONE
      | helper (UnitP, Unit) = SOME []
      | helper _ = NONE
  in
    helper (p, v)
  end

(* 3 *)
type name = string 
datatype RSP = ROCK | SCISSORS | PAPER
datatype 'a strategy = Cons of 'a * (unit -> 'a strategy) 
datatype tournament = PLAYER of name * (RSP strategy ref)
                    | MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one)) 
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) =
    let val Cons(rsp, func) = !strategyRef
    in 
        strategyRef := func();
        rsp 
    end

fun whosWinner(t: tournament) =
    let
        fun playMatch(a: RSP strategy ref, b: RSP strategy ref) =
            let
                val move1 = next a
                val move2 = next b
            in
                case (move1, move2) of
                    (ROCK, PAPER) => b
                    | (ROCK, SCISSORS) => a
                    | (PAPER, ROCK) => a
                    | (PAPER, SCISSORS) => b
                    | (SCISSORS, ROCK) => b
                    | (SCISSORS, PAPER) => a
                    | _ => playMatch(a, b)  (* play again *)
            end
    in
        case t of
            PLAYER(name, stratRef) => t
        | MATCH(t1, t2) =>
            let
                val winner1 = whosWinner(t1)
                val winner2 = whosWinner(t2)
            in
                case (winner1, winner2) of
                    (PLAYER(name1, a), PLAYER(name2, b)) =>
                            if (a = playMatch(a, b))
                            then PLAYER(name1, a)
                            else PLAYER(name2, b)
                | _ => raise Fail "Invalid tournament structure"
            end
    end

val winner = whosWinner(MATCH(PLAYER("s", ref s), MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r))));