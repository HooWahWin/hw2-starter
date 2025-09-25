let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) = 
  match tup with
  | (first, second) -> (second, first)
let rev_triple (tup : 'a * 'b * 'c) = 
  match tup with
  | (first, second, third) -> (third, second, first)

let is_odd x = abs(x) mod 2 == 1

let is_older (date1: int * int * int) (date2: int * int * int) =
  match (date1, date2) with
  | (y1, m1, d1), (y2, m2, d2) ->
    if y1 < y2 then true
    else if y1 > y2 then false
    else if m1 < m2 then true
    else if m1 > m2 then false
    else if d1 < d2 then true
    else false

let to_us_format (date1: int * int * int) = 
  match date1 with
  | (year, month, day) -> (month, day, year)
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = 
  if p == 0 then 1
  else if p == 1 then x
  else x * pow (x) (p - 1)


let rec fac n = 
  if n == 0 || n == 1 then 1
  else n * fac (n - 1) 

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = 
  if idx == 0 then List.hd lst
  else get_nth(idx - 1, List.tl lst)


let larger lst1 lst2 = 
  if List.length lst1 > List.length lst2 then lst1
  else if List.length lst1 < List.length lst2 then lst2
  else []

let rec sum_list (lst: 'a list): int = 
  if List.is_empty lst then 0
  else List.hd lst + sum_list(List.tl lst)

let sum lst1 lst2 = 
  sum_list lst1 + sum_list lst2
  
