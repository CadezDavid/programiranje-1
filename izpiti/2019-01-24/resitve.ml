(* 1. naloga *)

let podvoji_vsoto a b = 2 * (a + b)

let povsod_vecji (x, y, z) (x', y', z') = x > x' && y > y' && z > z'

let uporabi_ce_lahko f = function
  | None -> None
  | Some x -> Some (f x)

let rec pojavi_dvakrat a list =
  let rec pojavitve a = function
    | [] -> 0
    | x :: xs when x = a -> 1 + pojavitve a xs
    | x :: xs -> pojavitve a xs
  in
  pojavitve a list = 2

let izracunaj_v_tocki a list =
  let rec aux acc a = function
    | [] -> List.rev acc
    | f :: fs -> aux ((f a) :: acc) a fs
  in
  aux [] a list

let rec eksponent a n =
  let rec aux acc a = function
    | 0 -> acc
    | n -> aux (a * acc) a (n - 1)
  in
  aux 1 a n

(* 2. naloga *)

type 'a mm_drevo =
    Prazno
  | Sestavljeno of 'a mm_drevo * ('a * int) * 'a mm_drevo



let rec vstavi a = function
  | Prazno -> Sestavljeno (Prazno, (a, 1), Prazno)
  | Sestavljeno (l, (x, n), d) when x = a -> Sestavljeno (l, (x, n + 1), d)
  | Sestavljeno (l, (x, n), d) when x > a -> Sestavljeno (vstavi a l, (x, n), d)
  | Sestavljeno (l, (x, n), d) -> Sestavljeno (l, (x, n), vstavi a d)

let multimnozica_iz_seznama list =
  let rec aux drevo = function
    | [] -> drevo
    | x :: xs -> aux (vstavi x drevo) xs
  in
  aux Prazno list

let rec velikost_multimnozice = function
  | Prazno -> 0
  | Sestavljeno (l, (x, n), d) -> velikost_multimnozice l + n + velikost_multimnozice d

let rec seznam_iz_multimnozice drevo =
  let rec aux seznam to_do drevo =
    match to_do, drevo with
    | [], Prazno -> seznam
    | x :: xs, Prazno -> aux seznam xs x
    | _, Sestavljeno (Prazno, (x, n), d) -> aux (x :: seznam) to_do d
    | _, Sestavljeno (l, (x, n), d) -> aux seznam (Sestavljeno (Prazno, (x, n), d) :: to_do) l
  in
  aux [] [] drevo

let seznam_nakljucnih d = List.init d (fun _ -> Random.int d)

(* 3. naloga *)

let rec oceni i n curr = function
    | [] -> i
    | x :: xs ->
      if n < x then oceni curr x (curr + 1) xs
      else oceni i (n - 1) (curr + 1) xs

let rec first n = function
  | [] -> []
  | x :: xs -> if 0 < n then x :: first (n - 1) xs else []

let rec without n = function
  | [] -> []
  | x :: xs -> if 0 < n then without (n - 1) xs else x :: xs

let naloga list =
  let rec aux jumps e (x :: xs) =
    let e = x + e in
    if List.length xs < e then jumps + 1 else
      let skok = oceni 1 1 1 (first e xs) in
      aux (jumps + 1) (e - skok) (without skok (x :: xs))
  in
  aux 0 0 list
