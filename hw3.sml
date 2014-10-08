(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern


datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*val only_capitals = fn : string list -> string list*)
fun only_capitals (xs) = List.filter (fn arr => Char.isUpper (String.sub(arr, 0))) xs

(*val longest_string1 = fn : string list -> string*)
fun longest_string1 (ls: string list) = 
    foldl (fn (x, acc) => if String.size(acc) > String.size(x) then acc else x) "" ls

(*val longest_string2 = fn : string list -> string*)
fun longest_string2 (ls: string list) = 
    foldl (fn (x, acc) => if String.size(x) > String.size(acc) then x else acc) "" ls

(*val longest_string_helper = fn : (int * int -> bool) -> string list -> string*)
fun longest_string_helper f = 
    List.foldl (fn (x, acc) => if f (String.size(x), String.size(acc)) then x else acc)

(*val longest_string3 = fn : string list -> string*)
val longest_string3  = longest_string_helper (fn (x, y) => x > y)

(*val longest_string4 = fn : string list -> string*)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(*val longest_capitalized = fn : string list -> string*)
val longest_capitalized = longest_string1 o only_capitals

(*val rev_string = fn : string -> string*)
val rev_string = implode o rev o explode 
        
(*val first_answer = fn : (’a -> ’b option) -> ’a list -> ’b*)
fun first_answer f [] = raise NoAnswer
  | first_answer f (x::xs') = 
    let 
        val x' = f x
    in 
        if isSome x' then x' else first_answer f xs'
    end 

(*val all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option*)
fun all_answers f [] = SOME []
  | all_answers f (xs) = 
    let fun loop (acc, []) = SOME acc
          | loop (acc, NONE::ys) = NONE
          | loop (acc, SOME(y)::ys) = loop(acc @ y, ys)
    in 
        loop([], map f xs)
    end 

(*val count_wildcards = fn : pattern -> int*)
fun count_wildcards xs = g (fn () => 1) (fn x =>0) xs

(*val count_wild_and_variable_lengths = fn : pattern -> int*)
fun count_wild_and_variable_lengths xs = g(fn () => 1 )(fn x => String.size (x)) xs

(*val count_some_var = fn : string * pattern -> int*)
fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

(*val check_pat = fn : pattern -> bool*)
fun check_pat p' = 
    let 
        fun getVars p = 
            case p of
	        Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (p,i) => (getVars p) @ i) [] ps
	      | _                 => []
        fun isDistinct [] = true
          | isDistinct (x::xs) = if (List.exists (fn y => x =y) xs) then false else isDistinct xs
    in 
        isDistinct(getVars p')
    end

(*val match = fn : valu * pattern -> (string * valu) list option*)
fun match (v, p) = 
    case (v, p) of
        (_, Wildcard) => SOME[]
      | (_, Variable p) => SOME[(p, v)] 
      | (Unit , UnitP ) =>  SOME[]
      | (Const c1, ConstP c2) => if c1 = c2 then SOME[] else NONE
      | (Tuple vs, TupleP ps) => 
        if List.length vs = List.length ps 
        then all_answers (fn (v', p') => match (v', p')) (ListPair.zip(vs,ps))
        else NONE
      | _ => NONE

(*val first_match = fn : valu -> pattern list -> (string * valu) list option*)
