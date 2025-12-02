
let list_of_enter (s:string) : (int * int) list =
  let rec int_of_enter (l:string list) : (int * int) list =
    match l with
    | [] -> []
    | x::q -> match String.split_on_char '-' x with
              | a::b::[] -> (int_of_string a, int_of_string b)::(int_of_enter q)
              | _ -> failwith "Entrée non valide"
  in int_of_enter (String.split_on_char ',' s)

let sum_invalide ((mi,ma):int * int) : int =
  let n = ref 0 in
  for i = mi to ma do
    let s = string_of_int i in
    let s' = String.sub s 0 ((String.length s)/2) in
    if s = s' ^ s'
    then n := !n + i
  done;
  !n

let list_sum_invalide (l:(int * int) list) : int = List.fold_left (+) 0 (List.map sum_invalide l)

let sum_invalide2 ((mi,ma):int * int) : int =
  let n = ref 0 in
  for i = mi to ma do
    let est_valide = ref false in
    let s = string_of_int i in
    let len_s = String.length s in
    for k = 2 to len_s do
      let s' = String.sub s 0 ((String.length s)/k) in
      let ls = List.init k (fun _ -> s') in
      let sk = String.concat "" ls in
      if sk = s
      then est_valide := true
    done;
    if !est_valide
    then n := !n + i
  done;
  !n

let list_sum_invalide2 (l:(int * int) list) : int = List.fold_left (+) 0 (List.map sum_invalide2 l)
