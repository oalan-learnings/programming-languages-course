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

fun only_capitals(strings : string list) = List.filter(fn x => Char.isUpper(String.sub(x, 0))) strings

(* c : current, f: former *)
fun longest_string1(strings : string list) = foldl (fn (c, f) => if String.size c <= String.size f then f else c) "" strings

fun longest_string2(strings : string list) = foldl (fn (c, f) => if String.size c <  String.size f then f else c) "" strings

fun longest_string_helper bool_fun strings = foldl (fn (c, f) => 
  if bool_fun(String.size c, String.size f) then f else c) "" strings

val longest_string3 = fn strings => longest_string_helper(fn (x, y) => (x <= y)) strings

val longest_string4 = fn strings => longest_string_helper(fn (x, y) => (x < y)) strings

val longest_capitalized = fn strings => (longest_string1 o only_capitals) strings

fun rev_string a_string = (String.implode o List.rev o String.explode) a_string

fun first_answer f a_list = case a_list of 
                                [] => raise NoAnswer
                              | head::tail => let val result = f head in
                                  if isSome result then valOf result else first_answer f tail
                                              end  

fun all_answers f a_list = 
  let fun with_acc f acc a_list = 
        case a_list of 
                [] => SOME acc
              | head::tail => let val result = f head 
                                in
                                        if isSome result then with_acc f  (acc @ valOf result) tail
                                        else NONE
                                end
  in
    if null  a_list then SOME[]  
    else with_acc f [] a_list
  end

fun count_wildcards p = let val acc = 0 
  in 
        g (fn () => acc + 1) (fn x => 0) p 
end

fun count_wild_and_variable_lengths p = let val acc = 0 
in
        g (fn() => acc + 1) (fn x => acc + String.size x) p
  end

fun count_some_var(a_string, p) = let val acc = 0 
  in
        g (fn() => 0) (fn x => if x = a_string then acc + 1 else acc) p
  end

fun check_pat p =
        let 
            fun retrieve_strings p =  
              case p of
	         Wildcard          => []
	       | Variable x        => [x]
	       | TupleP ps         => List.foldl (fn (p,i) => retrieve_strings p  @ i) [] ps
	       | ConstructorP(_,p) => retrieve_strings p
	       | _                 => []

            fun find_if_distinct (str_list : string list) = case str_list of 
                                     [] => true
                                   | head::tail => if List.exists (fn x => head = x) tail then false
                                                   else find_if_distinct tail
        in
               (find_if_distinct o retrieve_strings) p
        end

fun match(v : valu, p : pattern) = 
  case p of 
    Wildcard => SOME[]
  | Variable x => SOME[(x, v)]
  | UnitP => if v = Unit then SOME[] else NONE
  | ConstP x => if v = Const x then SOME[] else NONE
  | TupleP ps => (case v of   
                  Tuple vs => if List.length vs = List.length ps
                              then all_answers (fn(x,y) => match(x,y)) (ListPair.zip(vs,ps)) 
                              else NONE
                 | _ => NONE)
  | ConstructorP (s1, p) => (case v of
                               Constructor (s2, v) => if s1=s2 then match(v, p) else NONE
                             | _ => NONE)

fun first_match v p_list = case p_list of 
                              [] => NONE
                            | head::tail => let val result = match(v, head) in
                                                if isSome result then result else first_match v tail
                                            end
