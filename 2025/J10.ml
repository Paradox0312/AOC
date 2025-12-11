
type machine = {
  panel : bool array;
  bouton : int array array;
  volt : int array;
}

let list_of_enter (s:string) : machine array =
  let rec split_aux_aux (l:string list) : int list =
    match l with
    | [] -> []
    | x::q -> (int_of_string x)::(split_aux_aux q)
  in let rec split_aux (l:string list) : int array list * int array =
    match l with
    | [] -> [], [||]
    | [x] -> [], Array.of_list (split_aux_aux (String.split_on_char ',' (String.sub x 1 (String.length x - 2))))
    | x::q -> let l', v = split_aux q in
              (Array.of_list (split_aux_aux (String.split_on_char ',' (String.sub x 1 (String.length x - 2)))))::l', v
  in let split (s':string) : machine =
    match String.split_on_char ' ' s' with
    | x1::q -> let b, v = split_aux q in
               {panel = Array.init (String.length x1 - 2) (fun t -> x1.[t+1]='#');
                bouton = Array.of_list b;
                volt = v
               }
    | _ -> failwith "format incorrect"
  in Array.map split (Array.of_list (String.split_on_char '\n' s))

let optimise (m:machine) : int =
  let p = Array.make (Array.length m.panel) false in
  let rec optimise_aux (n:int)(i:int) : int =
    if p = m.panel
    then n
    else if i >= Array.length m.bouton
    then max_int
    else begin
      let l1 = optimise_aux n (i+1) in
      Array.iter (fun x -> p.(x) <- not p.(x)) m.bouton.(i);
      let l2 = optimise_aux (n+1) (i+1) in
      Array.iter (fun x -> p.(x) <- not p.(x)) m.bouton.(i);
      min l1 l2
    end
  in optimise_aux 0 0

let optimise_tot (a:machine array) : int =
  Array.fold_left (+) 0 (Array.map optimise a)


let matrix_of_machine (m:machine) : int array array * int array =
  let a = Array.make_matrix (Array.length m.volt) (Array.length m.bouton) 0 in
  for j = 0 to Array.length m.bouton - 1 do
    Array.iter (fun i -> a.(i).(j) <- a.(i).(j) + 1) m.bouton.(j)
  done;
  a, m.volt

open Z3
let optimise_matrix_Z3 (a:int array array)(y:int array) : int array =
  let m = Array.length a in
  let n = Array.length a.(0) in
  
  let ctx = mk_context [] in
  let opt = Optimize.mk_opt ctx in

  let x = Array.init n (fun i -> Expr.mk_const ctx (Symbol.mk_string ctx (Printf.sprintf "x_%d" i))
    (Arithmetic.Integer.mk_sort ctx)) in

  for i = 0 to m - 1 do
    let sum = ref (Arithmetic.Integer.mk_numeral_i ctx 0) in
    for j = 0 to n - 1 do
      let coeff = Arithmetic.Integer.mk_numeral_i ctx a.(i).(j) in
      sum := Arithmetic.mk_add ctx [!sum; Arithmetic.mk_mul ctx [coeff; x.(j)]]
    done;
    let rhs = Arithmetic.Integer.mk_numeral_i ctx y.(i) in
    Optimize.add opt [Boolean.mk_eq ctx !sum rhs]
  done;

  for i = 0 to n - 1 do
    Optimize.add opt [Arithmetic.mk_ge ctx x.(i) (Arithmetic.Integer.mk_numeral_i ctx 0)]
  done;

  let obj = Arithmetic.mk_add ctx (Array.to_list x) in
  Optimize.minimize opt obj |> ignore;

  match Optimize.check opt with
  | Solver.SATISFIABLE -> begin
    match Optimize.get_model opt with
    | Some model -> Array.init n (fun i ->
                                  match Model.eval model x.(i) true with
                                  | Some e -> Z3.Expr.to_string e |> int_of_string
                                  | None   -> failwith "variable not evaluated")
    | None -> failwith "no model returned"
    end
  | Solver.UNSATISFIABLE -> failwith "No solution (constraints unsatisfiable)"
  | Solver.UNKNOWN       -> failwith "Solver could not decide"

let optimise_tot2 (e:machine array) : int =
  let optimise_aux (m:machine) : int =
    let a, y = matrix_of_machine m in
    let x = optimise_matrix_Z3 a y in
    Array.fold_left (+) 0 x
  in Array.fold_left (+) 0 (Array.map optimise_aux e)
