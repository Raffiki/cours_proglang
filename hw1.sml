(*val is_older = fn : (int * int * int) * (int * int * int) -> bool*)
fun is_older (dt1: int * int * int, dt2: int * int * int) = 
   let 
       val y1 = (#1 dt1)
       val y2 = (#1 dt2)
       val m1 = (#2 dt1)
       val m2 = (#2 dt2)
       val d1 = (#3 dt1)
       val d2 = (#3 dt2) 
   in
       
       if y1 > y2 then false else 
       if y2 > y1 then true else
       if m1 > m2 then false else 
       if m2 > m1 then true else
       if d1 < d2 then true else false

   end


(*val number_in_month = fn : (int * int * int) list * int -> int*)
fun number_in_month (dts: (int * int * int) list, m: int) =
    if null dts 
    then 0 
    else 
        if (#2 (hd dts) <> m) 
        then number_in_month (tl dts, m) 
        else 1 + number_in_month (tl dts, m)


(*val number_in_months = fn : (int * int * int) list * int list -> int*)
fun number_in_months (dts: (int * int * int) list, ms: int list) =
    if null ms 
    then 0
    else number_in_month (dts, hd ms) + number_in_months (dts, tl ms)
                             

(*val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list*)
fun dates_in_month (dts: (int * int * int) list, m: int) =
    if null dts 
    then []
    else 
        if (#2 (hd dts) <> m) 
        then dates_in_month (tl dts, m) 
        else [hd dts] :: dates_in_month (tl dts, m)


(*val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list*)
fun dates_in_months (dts: (int * int * int) list, ms: int list) =
    if null ms then []
    else dates_in_month (dts, hd ms) @ dates_in_months (dts, tl ms)

(*val get_nth = fn : string list * int -> string*)
fun get_nth (xs: string list, pos: int) =
    if pos = 1 
    then hd xs 
    else get_nth (tl xs, pos - 1)
                                       

(*val date_to_string = fn : int * int * int -> string*)
fun date_to_string (dt: (int * int * int)) = 

    let 
        val year = Int.toString(#1 dt)
        val day = Int.toString(#3 dt)

        val months = [
            "January",
            "February",
            "March", 
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November", 
            "December"
        ]
    in
        get_nth(months, #2 dt) ^ " " ^ day ^ " " ^ year
    end


fun number_before_reaching_sum_helper(sum: int, xs: int list, cnt: int) =
    if sum - hd xs <= 0 
    then cnt
    else number_before_reaching_sum_helper(sum - hd xs, tl xs, cnt + 1)


(*val number_before_reaching_sum = fn : int * int list -> int*)
fun number_before_reaching_sum(sum: int, xs: int list) =
    number_before_reaching_sum_helper(sum, xs, 0)
                                   

(*val what_month = fn : int -> int*)
fun what_month(day: int) =
    1 + number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])


(*val month_range = fn : int * int -> int list*)
fun month_range(day1: int, day2: int) =
    if day1 > day2 
    then []
    else what_month(day1) :: month_range(1 + day1, day2)


fun oldest_helper (ds: (int * int * int) list, oldest : (int * int * int)) =
    if null ds 
    then oldest
    else 
        if is_older (hd ds, oldest) 
        then oldest_helper(tl ds, hd ds)
        else oldest_helper(tl ds, oldest)
    
 
 datatype 'a option = NONE | SOME of 'a;                        

(*val oldest = fn : (int * int * int) list -> (int * int * int) option*)
fun oldest (ds: (int * int * int) ) option =
    if null ds 
    then NONE
    else SOME oldest_helper(tl ds, hd ds)
    










