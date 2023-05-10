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
  case (v, p) of
    (_, Wildcard) => SOME []
  | (_, UnitP) => if v = Unit then SOME [] else NONE
  | (Const i, ConstP j) => if i = j then SOME [] else NONE
  | (Tuple vs, TupleP ps) =>
      if length ps = length vs then
                let
                  val lst = List.foldr (fn (p', v') =>
                    fn acc =>
                      (case match (v', p') of
                        NONE => raise MatchFail
                      | SOME lst' => lst' @ acc)) [] (ListPair.zip (ps, vs))
                in
                  SOME (lst @ helper xs ys)
                end
              else NONE
  | (Constructor (s1, v'), ConstructorP (s2, p')) =>
      if s1 = s2 then match (v', p')
      else NONE
  | (Constructor _, _) => NONE
  | (Const _, _) => NONE
  | (Tuple _, _) => NONE
  | (Uint, _) => NONE

(* 3 *)
type name = string 
datatype RSP = ROCK | SCISSORS | PAPER
datatype 'a strategy = Cons of 'a * (unit -> 'a strategy) 
datatype tournament = PLAYER of name * (RSP strategy ref) | MATCH of tournament * tournament

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
        fun playMatch(stratRef1: RSP strategy ref, stratRef2: RSP strategy ref) =
            let
                val move1 = next stratRef1
                val move2 = next stratRef2
            in
                case (move1, move2) of
                    (ROCK, PAPER) => stratRef2
                | (ROCK, SCISSORS) => stratRef1
                | (PAPER, ROCK) => stratRef1
                | (PAPER, SCISSORS) => stratRef2
                | (SCISSORS, ROCK) => stratRef2
                | (SCISSORS, PAPER) => stratRef1
                | _ => playMatch(stratRef1, stratRef2)  (* Tie, play again *)
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
                    (PLAYER(name1, stratRef1), PLAYER(name2, stratRef2)) =>
                        PLAYER(
                            if (stratRef1 = playMatch(stratRef1, stratRef2))
                            then name1
                            else name2,
                            stratRef1
                        )
                | _ => raise Fail "Invalid tournament structure"
            end
    end

val winner = whosWinner(MATCH(PLAYER("s", ref s), MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r))));