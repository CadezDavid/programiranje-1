(* 1. naloga *)

type complex = { re : float ; im : float}

let complex_add a b = { re = (a.re +. b.re) ; im = (a.im +. b.im)}

let complex_conjugate a = { re = a.re ; im = (-. a.im) }

let rec list_apply_either pred f g = function
  | [] -> []
  | x :: xs ->
    if pred x
    then f x :: list_apply_either pred f g xs
    else g x :: list_apply_either pred f g xs

let eval_poly poly x =
  let rec aux acc x = function
    | [] -> acc
    | koef :: koefs -> aux (acc * x + koef) x koefs
  in
  aux 0 x (List.rev poly)

(* 2. naloga *)

type lastnik = string

type vrt = Obdelovan of lastnik
         | Oddan of lastnik * (vrt * vrt list)
         | Prost

let vrt_primer = Oddan ("Kovalevskay", (Obdelovan "Galois", [Obdelovan "Lagrange"]))

let obdelovalec_vrta = function
  | Obdelovan x -> Some x
  | _ -> None

let rec globina_oddajanja vrt =
  let aux curr a = max curr (globina_oddajanja a) in
  match vrt with
  | Obdelovan _ -> 0
  | Oddan (x, (y, ys)) -> 1 + max (globina_oddajanja y) (List.fold_left aux 0 ys)
  | Prost -> 0

let rec v_uporabi vrt =
  let rec aux = function
    | [] -> false
    | x :: xs -> if v_uporabi x then true else aux xs
  in
  match vrt with
  | Obdelovan _ -> true
  | Oddan (x, (y, ys)) -> v_uporabi y || aux ys
  | Prost -> false

let rec vsi_najemniki = function
  | Obdelovan x -> [x]
  | Oddan (x, (y, ys)) -> List.fold_left (@) [x] (List.map vsi_najemniki (y :: ys))
  | Prost -> []

let rec vsi_obdelovalci = function
  | Obdelovan x -> [x]
  | Oddan (x, (y, ys)) -> List.fold_left (@) [] (List.map vsi_najemniki (y :: ys))
  | Prost -> []

(* 3. naloga *)

let zabojniki = [1; 3; 4; 7; 10]

let rec make_list n e = if n = 0 then [] else e :: make_list (n - 1) e
let rec add_all x = function
  | [] -> []
  | y :: ys -> (x :: y):: add_all x ys

let rec naloga nosilnost = function
  | [] -> failwith "Hm, to ni smiselno."
  | x :: [] when nosilnost mod x = 0 -> (make_list (nosilnost / x) x) :: []
  | x :: [] -> []
  | x :: xs when x < nosilnost -> add_all x (naloga (nosilnost - x) xs) @ add_all x (naloga (nosilnost - x) (x :: xs))
  | x :: xs when x = nosilnost -> (x :: []) :: []
  | x :: xs -> []
