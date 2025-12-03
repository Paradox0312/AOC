
let list_of_enter (s:string) : string list =
  String.split_on_char '\n' s

let max_jont (s:string) : int =
  let n_max = ref 0 in
  for i = 0 to String.length s - 1 do
    for j = i+1 to String.length s - 1 do
      let n = (int_of_char s.[i] * 10) + int_of_char s.[j] - 528 in
      if n > !n_max
      then n_max := n
    done
  done;
  !n_max

let max_jont_list (l:string list) : int = List.fold_left (+) 0 (List.map max_jont l)

let max_jont2 (s:string) : int =
  let rec max_aux (i:int)(n:int) : string =
    if n = 0
    then ""
    else let j_max = ref i in
    for j = i to String.length s - n do
      if s.[j] > s.[!j_max]
      then j_max := j
    done;
    (String.make 1 s.[!j_max]) ^ (max_aux (!j_max+1) (n-1))
  in int_of_string (max_aux 0 12)

let max_jont_list2 (l:string list) : int = List.fold_left (+) 0 (List.map max_jont2 l)
