(* 1. naloga *)

let list_to_tuple = function
  | x1 :: x2 :: x3 :: [] -> Some (x1, x2, x3)
  | _ -> None

type counter = int * int * int

let compare_with list a =
  let rec aux lt eq gt a = function
    | [] -> (lt, eq, gt)
    | x :: xs when x < a -> aux (1 + lt) eq gt a xs
    | x :: xs when a < x -> aux lt (1 + eq) gt a xs
    | x :: xs -> aux lt eq (1 + gt) a xs
  in
  aux 0 0 0 a list

let apply_all f_list a =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x acc) xs
  in
  aux a (List.rev f_list)
