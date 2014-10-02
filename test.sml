val x = 3+4;

fun swap (pr: int*bool) = 
    (#2 pr, #1 pr)

(*int * int -> int* int*) 
fun div_mod (x: int, y: int) =
    (x div y, x mod y)
