
let list_of_enter (s:string) : (int*int) list * int list =
  let rec split (l:string list) : (int*int) list * int list =
    match l with
    | [] -> [], []
    | x::q when x = "" -> [], List.map int_of_string q
    | x::q -> let l1, l2 = split q in
              match String.split_on_char '-' x with
              | x1::x2::[] -> (int_of_string x1,int_of_string x2)::l1, l2
              | _ -> failwith "format incorrect"
  in split (String.split_on_char '\n' s)

let in_range (l1, l2:(int*int) list * int list) : int =
  let rec in_range_unit (l:int list) : int =
    match l with
    | [] -> 0
    | x::q when List.fold_left (fun acc (y,y') -> (y <= x && x <= y') || acc) false l1 -> 1 + in_range_unit q
    | x::q -> in_range_unit q
  in in_range_unit l2

let in_range2 (l1, l2:(int*int) list * int list) : int =
  let rec in_range_unit (l':(int*int) list)(i:int) : int =
    match l' with
    | [] -> 0
    | (x,y)::q when y<i -> in_range_unit q i
    | (x,y)::q when x<i -> y-i+1 + in_range_unit q (y+1)
    | (x,y)::q -> y-x+1 + in_range_unit q (y+1)
  in in_range_unit (List.sort_uniq compare l1) 0
