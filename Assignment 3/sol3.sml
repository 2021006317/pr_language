datatype pattern = Wildcard | Variable of String | UnitP
                | ConstP of int | TupleP of pattern list
                | ConstructorP of string * pattern
datatype valu = Const of int | Uint | Tuple of valu list
                | Constructor of string * valu

fun check_pat(p: pattern)=
    let
        fun help(pat : pattern) : string list =
            case pat of
                Wildcard => []
                | Variable(s) => [s]
                | UnitP => []
                | Const(i) => [i]
                | TupleP(pl) => check_pat(hd(pl))::check_pat(tl(pl))
                | ConstructorP(sl,p) => sl::check_pat(p)
        fun help2(sl : string list) : bool=
            let
                fun check_exist()
            if List.exists(fn(hd(sl)))
    in
    end