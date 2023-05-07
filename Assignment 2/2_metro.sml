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
