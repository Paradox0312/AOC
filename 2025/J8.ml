
let list_of_enter (s:string) : (int * int * int) array =
  let split (s':string) : int * int * int =
    match String.split_on_char ',' s' with
    | x::y::z::q -> (int_of_string x, int_of_string y, int_of_string z)
    | _ -> failwith "erreur format"
  in Array.map split (Array.of_list (String.split_on_char '\n' s))

let dist_sq ((x,y,z):(int * int * int))((x',y',z'):(int * int * int)) : int =
  (x-x')*(x-x') + (y-y')*(y-y') + (z-z')*(z-z')

let connexe (a:(int * int * int) array)(n:int) : int array =
  let cc = Array.init (Array.length a) Fun.id in
  let dist_min = ref 0 in
  for _ = 0 to n - 1 do
    let p = ref 0 in
    let p' = ref 0 in
    for i = 0 to Array.length a - 1 do
      for j = i + 1 to Array.length a - 1 do
        if dist_sq a.(i) a.(j) > !dist_min && (dist_sq a.(i) a.(j) < dist_sq a.(!p) a.(!p') || dist_sq a.(!p) a.(!p') <= !dist_min)
        then (p := i; p' := j)
      done
    done;
    dist_min := dist_sq a.(!p) a.(!p');
    if cc.(!p) <> cc.(!p')
    then let cp = cc.(!p') in
    for i = 0 to Array.length a - 1 do
      if cc.(i) = cp
      then cc.(i) <- cc.(!p)
    done
  done;
  cc

let time_3 (a:int array) : int =
  let count = Array.make (Array.length a) 0 in
  for i = 0 to Array.length a - 1 do
    count.(a.(i)) <- count.(a.(i)) + 1
  done;
  let m1 = ref 0 in
  let m2 = ref 0 in
  let m3 = ref 0 in
  for i = 1 to Array.length count - 1 do
    if count.(i) > count.(!m1)
    then m1 := i
  done;
  for i = 1 to Array.length count - 1 do
    if i <> !m1 && (count.(i) > count.(!m2) || !m2 = !m1)
    then m2 := i
  done;
  for i = 1 to Array.length count - 1 do
    if i <> !m1 && i <> !m2 && (count.(i) > count.(!m3) || !m3 = !m2 || !m3 = !m1)
    then m3 := i
  done;
  count.(!m1) * count.(!m2) * count.(!m3)

let connexe2 (a:(int * int * int) array) : (int * int * int) * (int * int * int) =
  let cc = Array.init (Array.length a) Fun.id in
  let p = ref 0 in
  let p' = ref 0 in
  while cc <> Array.make (Array.length a) cc.(0) do
    for i = 0 to Array.length a - 1 do
      for j = i + 1 to Array.length a - 1 do
        if cc.(i) <> cc.(j) && (dist_sq a.(i) a.(j) < dist_sq a.(!p) a.(!p') || cc.(!p) = cc.(!p'))
        then (p := i; p' := j)
      done
    done;
    let cp = cc.(!p') in
    for i = 0 to Array.length a - 1 do
      if cc.(i) = cp
      then cc.(i) <- cc.(!p)
    done
  done;
  a.(!p), a.(!p')
