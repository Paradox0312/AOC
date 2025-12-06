
let list_of_enter (s:string) : int =
  let split (a:string array) : int =
    let k = ref 0 in
    let i = ref 0 in
    let op = ref ' ' in
    while !i < String.length a.(0) do
      if Array.map (fun x -> x.[!i]) a = Array.make (Array.length a) ' '
      then i := !i + 1
      else begin
        op := a.(Array.length a - 1).[!i];
        let a' = Array.make (Array.length a - 1) "" in
        while !i < String.length a.(0) && Array.map (fun x -> x.[!i]) a <> Array.make (Array.length a) ' ' do
           for j = 0 to Array.length a - 2 do
             a'.(j) <- a'.(j) ^ (String.make 1 a.(j).[!i])
           done;
           i := !i + 1
        done;
        match !op with
        | '+' -> k := !k + (Array.fold_left ( + ) 0 (Array.map (fun x -> int_of_string @@ String.trim x) a'))
        | '*' -> k := !k + (Array.fold_left ( * ) 1 (Array.map (fun x -> int_of_string @@ String.trim x) a'))
        | _ -> failwith "erreur format"
      end
    done;
    !k
  in split @@ Array.of_list @@ String.split_on_char '\n' s

let list_of_enter2 (s:string) : int =
  let split (a:string array) : int =
    let k = ref 0 in
    let i = ref 0 in
    let op = ref ' ' in
    while !i < String.length a.(0) do
      if Array.map (fun x -> x.[!i]) a = Array.make (Array.length a) ' '
      then i := !i + 1
      else begin
        op := a.(Array.length a - 1).[!i];
        let read i' = int_of_string @@ String.trim @@ String.init (Array.length a - 1) (fun j -> a.(j).[i']) in
        let k' = ref (read !i) in
        i := !i + 1;
        while !i < String.length a.(0) && Array.map (fun x -> x.[!i]) a <> Array.make (Array.length a) ' ' do
           match !op with
           | '+' -> k' := !k' + (read !i); i := !i + 1
           | '*' -> k' := !k' * (read !i); i := !i + 1
           | _ -> failwith "erreur format"
        done;
        k := !k + !k'
      end
    done;
    !k
  in split @@ Array.of_list @@ String.split_on_char '\n' s
