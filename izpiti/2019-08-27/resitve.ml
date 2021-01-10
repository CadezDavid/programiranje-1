(* 1. naloga *)

let odstej_trojici (x1, x2, x3) (y1, y2, y3) = (x1 - y1, x2 - y2, x3 - y3)

let rec max_rezultat_do_n f = function
  | 0 -> f 0
  | n -> max (f n) (max_rezultat_do_n f (n - 1))

let pocisti_seznam list =
  let rec aux list' = function
    | [] -> List.rev list'
    | None :: xs -> aux list' xs
    | x :: xs -> aux (x :: list') xs
  in
  aux [] list

let preveri_urejenost list =
  let rec aux s l = function
    | [] -> (s, l)
    | x :: xs ->
      if x mod 2 = 0 then aux (x :: s) l xs
      else aux s (x :: l) xs
  in
  let rec is_sorted bool = function
    | [] | [_] -> true
    | x1 :: x2 :: xs ->
      if ((x1 < x2) || (not bool)) && ((x1 > x2) && bool)
      then is_sorted bool (x2 :: xs) else false
  in
  let (s, l) = aux [] [] list in
  is_sorted true s && is_sorted false l

(* 2. naloga *)

type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list

let gnezdenje_primer =
    [Element 1;
     Element 2;
     Podseznam
       [Element 3;
        Podseznam [
          Element 4
        ];
        Podseznam []
       ];
     Podseznam [
       Element 5
     ]
    ]

let rec najvecja_globina = function
  | [] -> 1
  | Element x :: xs -> max 1 (najvecja_globina xs)
  | Podseznam x :: xs -> max (1 + najvecja_globina x) (najvecja_globina xs)

let rec preslikaj f = function
  | [] -> []
  | Element x :: xs -> Element (f x) :: (preslikaj f xs)
  | Podseznam x :: xs -> Podseznam (preslikaj f x) :: (preslikaj f xs)

let rec splosci = function
  | [] -> []
  | Element x :: xs -> x :: splosci xs
  | Podseznam x :: xs -> splosci x @ splosci xs

let rec alternirajoci_konstruktorji = function
  | [] | [_] -> true
  | Element _ :: Podseznam x :: xs -> alternirajoci_konstruktorji (Podseznam x :: xs)
  | Podseznam _ :: Element x :: xs -> alternirajoci_konstruktorji (Element x :: xs)
  | _ -> false

let rec zlozi_preko_gnezdenja f acc = function
  | [] -> acc
  | Element x :: xs -> zlozi_preko_gnezdenja f (f acc x) xs
  | Podseznam x :: xs -> zlozi_preko_gnezdenja f (zlozi_preko_gnezdenja f acc x) xs

(* 3. naloga *)
