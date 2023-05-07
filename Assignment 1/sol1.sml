(* 1 *)
fun merge(list_a: int list, list_b: int list) =
  if null list_a then list_b
  else if null list_b then list_a
  else if hd(list_a) < hd(list_b) then hd(list_a)::merge(tl(list_a), list_b)
  else hd(list_b)::merge(list_a,tl(list_b))

(* 2 *)
fun reverse(list_a:int list)=
  let
    fun rev(cur_list:int list, res_list:int list)=
      if null cur_list then res_list
      else rev(tl(cur_list), hd(cur_list)::res_list)
  in
    rev(list_a, [])
  end

(* 3 *)
fun pi(a:int, b:int, f:int->int)=
 if a>b then 1
 else f(a) * pi(a+1, b, f)

(* 4 *)
fun digits(num:int)=
  let
    fun digitList(a:int, list_a:int list)=
        if a<10 then a::list_a
    else digitList(a div 10, (a mod 10)::list_a)
  in
    digitList(num, [])
  end

(* 5 *)
fun additivePersistence(n:int)=
  let
    fun addDigits(n:int)=
        if n<10 then n
        else addDigits(n div 10) + (n mod 10)
  in
    if n<10 then 0
    else 1 + additivePersistence(addDigits(n))
  end

fun digitalRoot(n:int)=
  let
    fun addDigits(n:int)=
        if n<10 then n
        else addDigits(n div 10) + (n mod 10)
  in
    if n<10 then n
    else digitalRoot(addDigits(n))
  end