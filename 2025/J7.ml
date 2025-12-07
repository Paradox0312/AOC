
let list_of_enter (s:string) : char array array =
  Array.map (fun x -> Array.init (String.length x)(fun i -> x.[i])) (Array.of_list (String.split_on_char '\n' s))

let simulation (a:char array array) : int =
  let k = ref 0 in
  for i = 0 to Array.length a - 2 do
    for j = 0 to Array.length a.(0) - 1 do
      match a.(i).(j), a.(i+1).(j) with
      | 'S', '|'
      | '|', '|' -> a.(i+1).(j) <- '|'
      | 'S', '^'
      | '|', '^' -> a.(i+1).(j-1) <- '|';
                    a.(i+1).(j+1) <- '|';
                    k := !k + 1
      | _ -> ()
    done
  done;
  !k

let simulation2 (a:char array array) : int =
  let tab = Array.make_matrix (Array.length a) (Array.length a.(0)) (-1) in
  tab.(Array.length a - 1) <- Array.make (Array.length a.(0)) 1;
  let rec sim_aux (i:int)(j:int) : int =
    if tab.(i).(j) <> -1
    then tab.(i).(j)
    else match a.(i+1).(j) with
    | '^' -> let k = sim_aux (i+1) (j-1) + sim_aux (i+1) (j+1) in
             tab.(i).(j) <- k;
             k
    | _ -> let k = sim_aux (i+1) j in
           tab.(i).(j) <- k;
           k
  in match Array.find_index (fun x -> x='S') a.(0) with
  | None -> 0
  | Some j -> sim_aux 0 j
