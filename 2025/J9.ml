
let list_of_enter (s:string) : (int * int) array =
  let rec split (s':string) : int * int =
    match String.split_on_char ',' s' with
    | x1::x2::[] -> int_of_string x1,int_of_string x2
    | _ -> failwith "format incorrect"
  in Array.map split (Array.of_list (String.split_on_char '\n' s))

let rectangle (a:(int * int) array) : int =
  let max_size = ref 0 in
  for i = 0 to Array.length a - 1 do
    for j = i + 1 to Array.length a - 1 do
      let size = (abs (fst a.(i) - fst a.(j)) + 1) * (abs (snd a.(i) - snd a.(j)) + 1) in
      if size > !max_size
      then max_size := size
    done
  done;
  !max_size

let point_in_segment ((x,y):int * int)(((xa,ya),(xb,yb)):(int * int) * (int * int)) : bool =
  (x-xa)*(y-yb) = (x-xb)*(y-ya)
  && (x > xa || x > xb)
  && (x < xa || x < xb)

let segment_in_segment (((xa,ya),(xb,yb)):(int * int) * (int * int))(((xc,yc),(xd,yd)):(int * int) * (int * int)) : bool =
  ((xc-xa)*(yc-yb) > (xc-xb)*(yc-ya) || (xd-xa)*(yd-yb) > (xd-xb)*(yd-ya))
  && ((xc-xa)*(yc-yb) < (xc-xb)*(yc-ya) || (xd-xa)*(yd-yb) < (xd-xb)*(yd-ya))
  && ((xa-xc)*(ya-yd) > (xa-xd)*(ya-yc) || (xb-xc)*(yb-yd) > (xb-xd)*(yb-yc))
  && ((xa-xc)*(ya-yd) < (xa-xd)*(ya-yc) || (xb-xc)*(yb-yd) < (xb-xd)*(yb-yc))

let rectangle_valide (((xa,ya),(xb,yb)):(int * int) * (int * int))(a:(int * int) array) : bool =
  let valide = ref true in
  for i = 0 to Array.length a - 1 do
    let j = (i + 1) mod Array.length a in
    if segment_in_segment (a.(i),a.(j)) ((xa,ya),(xb,ya))
       || segment_in_segment (a.(i),a.(j)) ((xa,yb),(xb,yb))
       || segment_in_segment (a.(i),a.(j)) ((xa,ya),(xa,yb))
       || segment_in_segment (a.(i),a.(j)) ((xb,ya),(xb,yb))
    then valide := false
  done;
  if !valide
  then for i = 0 to Array.length a - 1 do
    let j = (i + 1) mod Array.length a in
    let x, y = (fst a.(i) + fst a.(j))/2, (snd a.(i) + snd a.(j))/2 in
    if (x > xa || x > xb)
       && (x < xa || x < xb)
       && (y > ya || y > yb)
       && (y < ya || y < yb)
    then valide := false
  done;
  !valide

let is_inside (((xa,ya),(xb,yb)):(int * int) * (int * int))(a:(int * int) array) : bool =
  let x, y = (xa + xb)/2, (ya + yb)/2 in
  let x0, y0 = ref (-1), ref 0 in
  let i = ref 0 in
  while !i < Array.length a do
    if point_in_segment a.(!i) ((x,y),(!x0,!y0))
    then (y0 := !y0 + 1; i := 0)
  done;
  let valide = ref false in
  for i = 0 to Array.length a - 1 do
    let j = (i + 1) mod Array.length a in
    if segment_in_segment (a.(i),a.(j)) ((x,y),(!x0,!y0))
    then valide := not !valide
  done;
  !valide

let rectangle2 (a:(int * int) array) : int =
  let max_size = ref 0 in
  for i = 0 to Array.length a - 1 do
    for j = i + 1 to Array.length a - 1 do
      let size = (abs (fst a.(i) - fst a.(j)) + 1) * (abs (snd a.(i) - snd a.(j)) + 1) in
      if size > !max_size && rectangle_valide (a.(i),a.(j)) a && is_inside (a.(i),a.(j)) a
      then max_size := size
    done
  done;
  !max_size
