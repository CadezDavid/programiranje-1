(* 1. naloga *)

let razlika_kvadratov a b = 2 * a * b

let uporabi_na_paru f (x, y) = (f x, f y)

let rec ponovi_seznam n list =
  if n <= 0 then [] else
  if n mod 2 = 0 then ponovi_seznam (n / 2) (list @ list) else
    list @ ponovi_seznam (n - 1) list

let razdeli list =
  let rec aux neg not_neg = function
    | [] -> neg, not_neg
    | x :: xs when x < 0 -> aux (x :: neg) not_neg xs
    | x :: xs -> aux neg (x :: not_neg) xs
  in
  aux [] [] list

(* 2. naloga *)

type drevo = Prazno | Sestavljeno of drevo * int * drevo

module TSET = Set.Make(Tuple of int)

let rec relacija_of_drevo = function
  | Prazno -> TSET.empty
  | Sestavljeno (Prazno, _, Prazno) -> TSET.empty
  | Sestavljeno ((l, y, d), x, Prazno) ->
    relacija_of_drevo (l, y, d)
    |> List.fold_left TSET.add [(y1, x); (x; y1); (y2, x); (x; y2)]
  | Sestavljeno (Prazno, x, (l, y, d)) ->
    relacija_of_drevo (l, y, d)
    |> List.fold_left TSET.add [(y1, x); (x; y1); (y2, x); (x; y2)]
  | Sestavljeno ((ll, y1, ld), x, (dl, y2, dd)) ->
    TSET.union (relacija_of_drevo (ll, y1, ld)) (relacija_of_drevo (ll, y1, ld))
    |> List.fold_left TSET.add [(y1, x); (x; y1); (y2, x); (x; y2)]

(* 3. naloga *)
