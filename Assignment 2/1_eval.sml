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
            | NOT(f1) => if eval(f1) then false else true
            | ANDALSO(f1,f2) => if eval(f1) then eval(f2) else false
            | ORELSE(f1,f2) => if eval(f1) then true else eval(f2)
            | IMPLY(f1,f2) => if eval(f1) then eval(f2) else true
            | LESS(e1,e2) => if ex(e1)<ex(e2) then true else false
    end