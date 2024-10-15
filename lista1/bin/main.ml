 (** zad 1 *)
let reverse4 (a, b, c, d) = (d, c, b, a);;

let t = reverse4 (5, 4, 3, 2);;
let t = reverse4 (5, "wrap wege awokado", "aloes", ("camelCase (worse)", "snake_case (better)"));;


 (** zad 2 *)
let both_lists_empty list1 list2 = List.is_empty list1 && List.is_empty list2
let at_least_one_list_empty list1 list2 = List.is_empty list1 || List.is_empty list2
let heads_equal list1 list2 = List.nth list1 0 = List.nth list2 0
let get_safe_tail list = if List.is_empty list then [] else List.tl list

let rec differences list1 list2 = 
  if both_lists_empty list1 list2 then 0 else
  if at_least_one_list_empty list1 list2 then 1 + differences (get_safe_tail list1) (get_safe_tail list2) else
  if heads_equal list1 list2 then 0 + differences (List.tl list1) (List.tl list2)
  else 1 + differences (List.tl list1) (List.tl list2);;

let list1 = 2 :: 3 :: 4 :: [];;
let list2 = 2 :: 3 :: 4 :: [];;

differences list1 list2

let list1 = "maslo" :: "woz" :: "albo przewoz" :: [];;
let list2 = "maslo" :: "zow" :: "zowezrp obla" :: [];;

differences list1 list2

let list1 = 2.2 :: 3. :: 4. :: [];;
let list2 = 0. :: -2. :: 4.0001 :: [];;

differences list1 list2

let list1 = 2 :: 3 :: 4 :: [];;
let list2 = 2 :: 3 :: 4 :: 6 :: 9 :: 13 :: [];;

differences list1 list2

let list1 = None :: None :: None :: [];;
let list2 = None :: None :: None :: None :: [];;

differences list1 list2


let rec add_list_elements list = 
  if List.is_empty list then [] else List.nth list 0 :: add_list_elements (List.tl list) 

let rec insert list new_elem pos = 
  if pos > 0 then 
    if List.is_empty list then [new_elem] else List.hd list :: insert (List.tl list) new_elem (pos - 1) 
  else
    new_elem :: add_list_elements list


(* append *)
let list = 2 :: []
let new_elem = 4;;
insert list new_elem (1);;


(* out of range (< 0 or > length) *)
let list = 2 :: []
let new_elem = -2;;
insert list new_elem (-3);;
insert list new_elem (3);;

(* normal case *)
let list = 1 :: 2 :: 3 :: 4 :: 5 :: []
let new_elem = 100;;
insert list new_elem 3;;
insert list new_elem 2;;

(* empty list *)
let list = []
let new_elem = 1.0;;
insert list new_elem (-1);;
insert list new_elem (0);;
insert list new_elem (1);;
insert list new_elem (2);;


(* zad 4 *)
let time_to_str hours minutes =
  string_of_int (if hours = 12 then 12 else hours mod 12) ^ ":" ^ string_of_int minutes

let englishTime hours minutes = 
  if hours < 0 || hours > 24 then raise (Invalid_argument "Time must be between 00:00 and 24:00") else
  if hours = 0 && minutes = 0 then "12:00 AM" else 
  if hours - 12 >= 0 then time_to_str hours minutes ^ " PM" else time_to_str hours minutes ^ " AM";;
  

englishTime 20 20;;
englishTime 0 20;;
englishTime 11 20;;
englishTime 12 20;;
englishTime 13 20;;
englishTime 23 59;;
englishTime 24 00;;