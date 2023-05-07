datatype pattern = Wildcard | Variable of String | UnitP
                | ConstP of int | TupleP of pattern list
                | ConstructorP of string * pattern
datatype valu = Const of int | Uint | Tuple of valu list
                | Constructor of string * valu

fun check_pat(p: pattern) =
    let
        fun help(pat : pattern) : string list =
            case pat of
                Wildcard => []
                | Variable(s) => [s]
                | UnitP => []
                | Const(i) => [i]
                | TupleP(pl) => check_pat(hd(pl))::check_pat(tl(pl))
                | ConstructorP(sl,p) => sl::check_pat(p)
        (* 이렇게 하면 안됨. *)    
        fun is_repeat(sl : string list) : bool =
            let
                fun check_repeat(s, sll) : bool =
                    if null sll
                    then false
                    else if s = hd(sll)
                        then true
                        else check_repeat(s, tl(sll))
            in
                if null sl
                then false
                else check_repeat(hd(sl), tl(sl))
            end
    in
    end