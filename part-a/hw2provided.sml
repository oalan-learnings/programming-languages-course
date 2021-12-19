(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun list_without_pattern (pattern : string, a_list : string list) = 
        case a_list of  
          [] => []
        | beginning :: rest => if same_string(pattern, beginning) then  rest
                                  else beginning :: list_without_pattern(pattern, rest)


fun all_except_option(pattern : string, string_list : string list) = 
  let fun list_without_pattern (pattern : string, a_list : string list) = 
        case a_list of  
          [] => []
        | beginning :: rest => if same_string(pattern, beginning) then  rest
                                  else beginning :: list_without_pattern(pattern, rest)
      val result = list_without_pattern(pattern, string_list) 
  in
      if length result < length string_list  then SOME result  else NONE 
  end

fun get_substitutions1(subs : string list list, s : string) =  
  case subs of 
    [] => [] 
  | head :: tail => if length (list_without_pattern(s, head)) < length head 
                    then 
                        list_without_pattern(s, head) @ get_substitutions1(tail, s)
                    else
                        get_substitutions1(tail, s)


fun get_substitutions2(subs : string list list, s : string) =  
  let fun subs_with_acc (l, s, acc)  =  
        case l of 
         [] => acc
        | head :: tail => if length (list_without_pattern(s, head)) < length head 
                          then 
                                subs_with_acc(tail, s, (list_without_pattern(s, head) @ acc)) 
                          else
                                subs_with_acc(tail, s, acc)
  in
        subs_with_acc(subs, s, [])
  end


fun similar_names(names : string list list, {first : string, middle : string, last : string}) = 
  let fun build_names(extracted_names : string list) =
        case extracted_names of 
          [] => []
        | head :: tail => [{first=head, middle=middle, last=last}] @ build_names(tail)
  in
        {first=first, middle=middle, last=last} ::  build_names(get_substitutions2(names, first))
  end


(* put your solutions for problem 2 here *)

fun card_color(a_card : card) = 
   case a_card of 
      (Spades, _) => Black
    | (Clubs,_) => Black
    | (Diamonds,_) => Red
    | (Hearts,_) => Red

fun card_value(a_card : card) = 
  case a_card of 
      (_, Num x) => x
    | (_, Ace) => 11
    | (_, _) => 10

fun remove_card(cards : card list, a_card : card, ex : exn) = 
  let fun result_finder (cards, a_card) = 
    case cards of 
       [] => []
     | head :: tail => if(head = a_card) then tail
                      else head :: remove_card(tail, a_card, ex)
    val result = result_finder(cards, a_card)
  in
    if (length cards > length result) then result else raise ex 
  end

fun all_same_color(cards : card list) =
  case cards of
    [] => true
  | head :: tail => let val current_color = card_color(head)
                    in
                      case tail of 
                        [] => true
                      | new_head :: new_tail => if card_color(new_head) = current_color 
                                                then all_same_color(new_tail)
                                                else false
                    end

fun sum_cards(cards : card list) = 
  let fun accumulation(cards, acc) = 
        case cards of
          [] => acc
        | head :: tail => accumulation(tail, card_value(head) + acc)
  in
    accumulation(cards, 0)
  end


fun score(cards : card list, goal : int) = 
  let
    val sum = sum_cards cards 
    val preliminary_score = 
        if sum > goal then 3 * (sum - goal)
        else if sum < goal then goal - sum
             else 0
    val isAllSame = all_same_color cards
  in
    if isAllSame then preliminary_score div 2
    else preliminary_score
  end 

fun officiate(cards : card list, moves : move list, goal : int) = 
  let 
    fun play(c, m, held_cards) = 
    case m of 
      [] => score(held_cards, goal)
    | Discard a_card :: remaining_moves => play(c, remaining_moves, remove_card(held_cards, a_card, IllegalMove))    
    | Draw :: remaining_moves  => 
        case c of
          [] => score(held_cards, goal)
        | head :: remaining_cards => if(sum_cards(head::held_cards) > goal) then score((head::held_cards), goal)
                                     else play(remaining_cards, remaining_moves, (head::held_cards))
  in 
    play(cards, moves, [])
  end

