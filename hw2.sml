(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
              
fun all_except_option (st, xs) =
    let 
        fun loop (prev, []) = NONE
          | loop (prev, x::xs) = 
            if same_string (x, st)
            then SOME (prev @ xs) 
            else loop (prev @ [x], xs)
    in 
        loop ([], xs)
    end 

fun get_substitutions1 ([], st) = []
  | get_substitutions1 (x::xs, st) = 
            case all_except_option(st, x) of NONE => get_substitutions1 (xs, st)
                                           | SOME (ys) => ys @ get_substitutions1 (xs, st)

fun get_substitutions2 (lists, st) =
    let
        fun loop (acc, []) = acc
          | loop (acc, x::xs) = 
            case all_except_option(st, x) of NONE => loop (acc, xs)
                                           | SOME (ys) => loop (acc @ ys, xs)
    in 
        loop ([], lists)
    end 
    

fun similar_names (lists, {first=f,middle=m,last=l}) = 
    let 
        fun loop ([]) = [{first=f,middle=m,last=l}]
          | loop (x::xs) = loop xs @ [{first=x, middle=m, last=l}]
    in
        loop (get_substitutions2(lists, f))
    end
        


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color Clubs = Black
  | card_color Diamonds = Red
  | card_color Hearts = Red
  | card_color Spades = Black
                            
fun card_value c = 
    case c of
        Ace => 10
     | King => 10
     | Queen => 10
     | Jack => 10
     | Num(n) => n

fun remove_card (cs, c, e) =
    let
        fun loop (prev, []) = raise e
          | loop (prev, c'::cs) = if c' = c
                                 then prev @ cs
                                 else loop (prev @ [c'],  cs)
    in 
        loop ([], cs)
    end
        

fun all_same_color [] = true
  | all_same_color (c'::cs) = 
    let 
        fun loop ([], color) = true
          | loop (c::cs', color) = if card_color c = color 
                                   then loop (cs', color)
                                   else false
    in 
        loop (cs, card_color(c'))
    end 
