
let list_of_enter (s:string) : char array array =
  Array.map (fun x -> Array.init (String.length x)(fun i -> x.[i])) (Array.of_list (String.split_on_char '\n' s))

let inf_voisin (a:char array array) : int * (char array array) =
  let a' = Array.copy a in
  let sum = ref 0 in
  for i = 0 to Array.length a - 1 do
    for j = 0 to Array.length a.(0) - 1 do
      if a.(i).(j) = '@'
      then begin
        let k = ref (-1) in
        for i' = -1 to 1 do
          for j' = -1 to 1 do
            if i+i' >= 0 && i+i' < Array.length a
              && j+j' >= 0 && j+j' < Array.length a.(0)
              && a.(i+i').(j+j') = '@'
            then k := !k + 1
          done
        done;
        if !k < 4
        then (sum := !sum + 1; a'.(i).(j) <- 'x')
      end
    done
  done;
  !sum, a'

let loop_voisin (a:char array array) : int =
  let a' = ref a in
  let sum = ref 0 in
  let k = ref (-1) in
  while !k <> 0 do
    let k0, a0 = inf_voisin a in
    k := k0;
    a' := a0;
    sum := !sum + !k
  done;
  !sum
