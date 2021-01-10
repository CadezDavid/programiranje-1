(* -------- 1 -------- *)

let rec vsota = function
  | [] -> failwith "Nepricakovam argument v funkciji vsota."
  | x :: [] -> x
  | x1 :: x2 :: xs -> vsota ((x1 + x2) :: xs)

(* -------- 2 -------- *)

let rec je_narascajoc = function
  | [] | _ :: [] -> true
  | x1 :: x2 :: xs -> if x1 <= x2 then je_narascajoc (x2 :: xs) else false

(* -------- 3 -------- *)

let rec vstavi n = function
  | [] -> n :: []
  | x :: xs when n < x -> n :: x :: xs
  | x :: xs -> x :: vstavi n xs

let uredi list =
  let rec aux urejen = function
    | [] -> urejen
    | x :: xs -> aux (vstavi x urejen) xs
  in
  aux [] list

(* -------- 4 -------- *)

let rec vstavi' cmp n = function
  | [] -> n :: []
  | x :: xs when cmp n x -> n :: x :: xs
  | x :: xs -> x :: vstavi' cmp n xs

let uredi' cmp list =
  let rec aux cmp urejen = function
    | [] -> urejen
    | x :: xs -> aux cmp (vstavi' cmp x urejen) xs
  in
  aux cmp [] list

(* -------- 5 -------- *)

type priority = Top | Group of int

type status = Staff | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)

let primerjaj_potnike {status = a; _} {status = b; _} =
  match a, b with
  | _, Staff -> false
  | Staff, _ -> true
  | Passenger _, Passenger Top -> false
  | Passenger Top, Passenger _ -> true
  | Passenger Group x, Passenger Group y -> x > y

let uredi_seznam_potnikov = uredi' primerjaj_potnike

(* -------- 7 -------- *)

let enaka {status = a; name = name_a} {status = b; name = name_b} = (a = b)

let uredi_v_bloke list =
  let rec aux acc curr = function
    | [] -> List.rev (curr :: acc)
    | x :: [] -> (x :: curr) :: acc
    | a :: b :: xs ->
      if enaka a b then aux acc (a :: curr) (b :: xs)
      else aux ((a :: curr) :: acc) [] (b :: xs)
  in
  list |> uredi_seznam_potnikov |> aux [] []
