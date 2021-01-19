let rec randlist len max = List.init len (fun _ -> Random.int max)

let swap a i j =
  let x = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- x;
  a

let quicksort list =
  let a = Array.of_list list in

  let rec pivot a p lower curr upper =
    if curr = upper then (swap a p (lower - 1), (lower - 1)) else
    if a.(curr) < a.(p)
    then pivot (swap a lower curr) p (lower + 1) (curr + 1) upper
    else pivot a p lower (curr + 1) upper
  in

  let rec sort a lower upper =
    if lower = upper then a else
      let (a, b) = pivot a lower (lower + 1) (lower + 1) upper in
      let a = sort a lower b in
      let a = sort a (b + 1) upper in
      a
  in

  Array.to_list (sort a 0 (Array.length a - 1))








(* let quicksort list =
  let rec pivot p small big = function
    | [] -> (small, big)
    | x :: xs when p < x -> pivot p small (x :: big) xs
    | x :: xs -> pivot p (x :: small) big xs
  in
  let rec sort = function
    | [] -> []
    | x :: xs ->
      let (small, big) = pivot x [] [] xs in
      (sort small) @ [x] @ (sort big)
  in
  sort list *)




(* Precej slab quicksort *)

(* let rec quicksort list =
  let rec aux1 a acc = function
    | [] ->
      if acc = [] then a :: [] else
        let x :: xs = acc in
        a :: aux1 x [] xs
    | x :: xs when a < x -> aux1 a (x :: acc) xs
    | x :: xs -> x :: aux1 a acc xs
  in
  let rec aux2 a acc = function
    | [] ->
      if acc = [] then a :: [] else
      let x :: xs = acc in
      a :: aux2 x [] xs
    | x :: xs when x < a -> aux2 a (x :: acc) xs
    | x :: xs -> x :: aux2 a acc xs
  in
  let rec sorted = function
    | [] | _ :: [] -> true
    | x1 :: x2 :: xs -> if x1 <= x2 then sorted (x2 :: xs) else false
  in
  let rec aux list =
    if sorted list then lis *)
