(* 1. naloga *)

let rec izpisi_vsa_stevila = function
  | [] -> ()
  | x :: xs ->
    print_string (string_of_int x);
    izpisi_vsa_stevila xs

let map2_opt f l1 l2 =
  if List.length l1 != List.length l2 then None else
    let rec aux acc f l1 l2 =
      match l1, l2 with
      | [], [] -> Some (List.rev acc)
      | x :: xs, y :: ys -> aux ((f x y) :: acc) f xs ys
      | _ -> failwith "Different lengths in map2_opt."
    in
    aux [] f l1 l2

let rec map2_opt' f l1 l2 =
  match l1, l2 with
  | [], [] -> Some []
  | x :: xs, y :: ys ->
    (match map2_opt f xs ys with
    | Some l3 -> Some ((f x y) :: l3)
    | None -> None)
  | _, [] | [], _ -> None

(* 2. naloga *)

type filter_tree =
    Sestavljeno of filter_tree * int * filter_tree
  | Seznam of int list

let primer = Sestavljeno (
    Sestavljeno (
      Seznam (1 :: []),
      5,
      Seznam []
    ),
    10,
    Sestavljeno (
      Seznam [],
      15,
      Seznam (19 :: 20 :: [])
      )
  )

let rec vstavi n = function
  | Seznam xs -> Seznam (n :: xs)
  | Sestavljeno (l, x, d) when n <= x -> Sestavljeno (vstavi n l, x, d)
  | Sestavljeno (l, x, d) -> Sestavljeno (l, x, vstavi n d)

let rec vstavi_seznam tree = function
  | [] -> tree
  | x :: xs -> vstavi_seznam (vstavi x tree) xs

let preveri tree =
  let rec aux1 lower upper = function
    | [] -> true
    | x :: xs ->
      match lower, upper with
      | None, None -> true
      | Some a, None -> (a < x) && aux1 lower upper xs
      | None, Some b -> (x <= b) && aux1 lower upper xs
      | Some a, Some b -> (a < x) && (x <= b) && aux1 lower upper xs
  in
  let rec aux2 lower upper = function
    | Seznam xs -> aux1 lower upper xs
    | Sestavljeno (l, x, d) ->
      aux2 lower (Some x) l && aux2 (Some x) upper d
  in
  aux2 None None tree

(* 3. naloga *)

type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  (* Tip linearnih preslikav *)
  type t
  (* Identiteta *)
  val id : t
  (* Dano preslikavo uporabi na vektorju *)
  val uporabi : t -> vektor -> vektor
  (* Vrne linearno preslikavo, določeno z matriko *)
  val iz_matrike : matrika -> t
  (* Vrne linearno preslikavo, določeno s funkcijo
     (predpostavite lahko, da je funkcija linearna) *)
  val iz_funkcije : (vektor -> vektor) -> t
  (* Vrne kompozitum danih preslikav. *)
  val kompozitum : t -> t -> t
end

(* 4. naloga *)
