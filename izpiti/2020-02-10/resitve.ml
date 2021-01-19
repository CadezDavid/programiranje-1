(* 1. naloga *)

let dot_prod (x1, y1, z1) (x2, y2, z2) = x1 *. x2 +. y1 *. y2 +. z1 *. z2

let fix_second f a b = f b a

let combine_and_filter f xs ys =
  let rec aux acc f xs ys =
  match xs, ys with
  | _, [] | [], _ -> List.rev acc
  | x :: xs, y :: ys ->
    match f x y with
    | None -> aux acc f xs ys
    | Some z -> aux (z :: acc) f xs ys
  in
  aux [] f xs ys

let rec conditional_print p = function
  | [] -> ()
  | x :: xs when p x ->
    print_string x;
    conditional_print p xs
  | x :: xs -> conditional_print p xs

(* 2. naloga *)

type ('a, 'b) tree =
  | Empty
  | ANode of ('a, 'b) tree * 'a * ('a, 'b) tree
  | BNode of ('a, 'b) tree * 'b * ('a, 'b) tree

let test : (int, bool) tree = ANode (
    BNode (Empty, true, Empty),
    12,
    ANode (ANode (Empty, 0, Empty), 5, BNode (Empty, false, Empty))
  )

let rec adepth = function
  | Empty -> None
  | ANode (l, _, r) ->
    (match adepth l, adepth r with
    | None, None -> Some 0
    | None, Some x | Some x, None -> Some (1 + x)
    | Some x, Some y -> Some (1 + max x y))
  | BNode (l, _, r) ->
    (match adepth l, adepth r with
    | None, None -> None
    | None, Some x | Some x, None -> Some (1 + x)
    | Some x, Some y -> Some (1 + max x y))

let rec bdepth = function
  | Empty -> None
  | ANode (l, _, r) ->
    (match adepth l, adepth r with
     | None, None -> None
     | None, Some x | Some x, None -> Some (1 + x)
     | Some x, Some y -> Some (1 + max x y))
  | BNode (l, _, r) ->
    (match bdepth l, bdepth r with
     | None, None -> Some 0
     | None, Some x | Some x, None -> Some (1 + x)
     | Some x, Some y -> Some (1 + max x y))

type result = { aNodes : int ; bNodes : int }

let sestej r1 r2 = { aNodes = (r1.aNodes + r2.aNodes) ; bNodes = (r1.bNodes + r2.bNodes) }

let rec count = function
  | Empty -> { aNodes = 0 ; bNodes = 0 }
  | ANode (l, x, d) -> sestej (sestej (count l) (count d)) { aNodes = 1 ; bNodes = 0 }
  | BNode (l, x, d) -> sestej (sestej (count l) (count d)) { aNodes = 0 ; bNodes = 1 }

let rec is_typemirror tree1 tree2 =
  match tree1, tree2 with
  | Empty, Empty -> true
  | Empty, _ | _, Empty -> false
  | ANode (l1, x1, d1), BNode (l2, x2, d2) ->
    is_typemirror l1 l2 && x1 = x2 && is_typemirror d1 d2
  | BNode (l1, x1, d1), ANode (l2, x2, d2) ->
    is_typemirror l1 l2 && x1 = x2 && is_typemirror d1 d2
  | BNode (l1, x1, d1), BNode (l2, x2, d2) -> false
  | ANode (l1, x1, d1), ANode (l2, x2, d2) -> false

let rec foldmap fa fb acc = function
  | Empty -> (acc, Empty)
  | ANode (l, x, d) ->
    let (acc1, l') = foldmap fa fb acc l in
    let (acc2, d') = foldmap fa fb acc1 d in
    let (acc3, x') = fa acc2 x in
    (acc3, ANode (l', x', d'))
  | BNode (l, x, d) ->
    let (acc1, l') = foldmap fa fb acc l in
    let (acc2, d') = foldmap fa fb acc1 d in
    let (acc3, x') = fb acc2 x in
    (acc3, BNode (l', x', d'))

(* let rec min_nodes tree =
  let rec aux min_float min_int = function
    | Empty -> (None, None, Empty)
    | ANode (l, x, d) ->
      let (l_min_float, l_min_int, l') = aux (min min_float x) min_int l in
      let (d_min_float, d_min_int, d') = aux (min min_float x) min_int d in
      let min_float = min l_min_float d_min_float in
      let min_int = min l_min_int d_min_int in *)


(* 3. naloga *)
