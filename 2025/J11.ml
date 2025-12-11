
let string_to_index (s:string) : int =
  ((int_of_char s.[0] - 97) * 26 + (int_of_char s.[1] - 97)) * 26 + (int_of_char s.[2] - 97)

let list_of_enter (s:string) : string list array =
  let m = Array.make (26*26*26) [] in
  let add (s':string) : unit =
    match String.split_on_char ' ' s' with
    | x::q -> m.(string_to_index x) <- q
    | _ -> failwith "erreur format"
  in List.iter add (String.split_on_char '\n' s);
  m

let simulation (a:string list array) : int =
  let tab = Array.make (Array.length a) (-1) in
  tab.(string_to_index "out") <- 1;
  let rec sim_aux (i:int) : int =
    if tab.(i) = -1
    then tab.(i) <- List.fold_left (fun acc x -> acc + (sim_aux (string_to_index x))) 0 a.(i);
    tab.(i)
  in sim_aux (string_to_index "you")

let simulation2 (a:string list array) : int * int * int * int =
  let tab = Array.make (Array.length a) (-1,-1,-1,-1) in
  tab.(string_to_index "out") <- (1,0,0,0);
  let rec sim_aux (i:int) : int * int * int * int =
    if tab.(i) = (-1,-1,-1,-1)
    then begin
      tab.(i) <- List.fold_left (fun (c1,c2,c3,c4) x -> let x1,x2,x3,x4 = (sim_aux (string_to_index x)) in c1+x1,c2+x2,c3+x3,c4+x4) (0,0,0,0) a.(i);
      if i = string_to_index "fft"
      then (let t1,t2,t3,t4 = tab.(i) in tab.(i) <- (0,t1+t2,0,t3+t4));
      if i = string_to_index "dac"
      then (let t1,t2,t3,t4 = tab.(i) in tab.(i) <- (0,0,t1+t3,t2+t4))
    end;
    tab.(i)
  in sim_aux (string_to_index "svr")
