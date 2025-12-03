
let list_of_enter (s:string) : string list =
  String.split_on_char '\n' s

let max_sous_mots (len:int)(s:string) : int =
  let rec max_aux (i:int)(n:int) : string =
    if n = 0
    then ""
    else let j_max = ref i in
    for j = i to String.length s - n do
      if s.[j] > s.[!j_max]
      then j_max := j
    done;
    (String.make 1 s.[!j_max]) ^ (max_aux (!j_max+1) (n-1))
  in int_of_string (max_aux 0 len)

let sum_max_sous_mots (len:int)(l:string list) : int = List.fold_left (+) 0 (List.map (max_sous_mots len) l)
