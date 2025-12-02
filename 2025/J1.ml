
let list_of_enter (s:string) : int list =
  let rec int_of_enter (l:string list) : int list =
    match l with
    | [] -> []
    | x::q when x.[0]='R' -> (int_of_string (String.sub x 1 (String.length x - 1)))::(int_of_enter q)
    | x::q  -> (- int_of_string (String.sub x 1 (String.length x - 1)))::(int_of_enter q)
  in int_of_enter (String.split_on_char '\n' s)

let zero_in_cercle (l:int list) : int =
  let rec move_in_cercle (l':int list)(n:int) : int =
    match l' with
    | [] -> 0
    | x::q when (x+n) mod 100 = 0 -> 1 + move_in_cercle q 0
    | x::q -> move_in_cercle q (x+n)
  in move_in_cercle l 50

let zero_in_cercle_2 (l:int list) : int =
  let rec move_in_cercle (l':int list)(n:int) : int =
    match l' with
    | [] -> 0
    | x::q when x = 0                      -> move_in_cercle q n
    | x::q when x > 0 && (n+1) mod 100 = 0 -> 1 + move_in_cercle ((x-1)::q) 0
    | x::q when x > 0                      -> move_in_cercle ((x-1)::q) (n+1)
    | x::q when          (n-1) mod 100 = 0 -> 1 + move_in_cercle ((x+1)::q) 0
    | x::q                                 -> move_in_cercle ((x+1)::q) (n-1)
  in move_in_cercle l 50
