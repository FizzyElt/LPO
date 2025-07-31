type t =
  | Empty
  | Leaf of int
  | Node of int * int * t * t

let zero_bit x b = x land b == 0

let rec mem x = function
  | Empty -> false
  | Leaf j -> x = j
  | Node (_, b, l, r) ->
    mem
      x
      (if zero_bit x b then
         l
       else
         r)
;;

let rightmost_1_bit x = x land -x

let branch p1 t1 p2 t2 =
  let b = rightmost_1_bit (p1 lxor p2) in
  let p = p1 land (b - 1) in
  if zero_bit p1 b then
    Node (p, b, t1, t2)
  else
    Node (p, b, t2, t1)
;;

let matches_prefix x p b = x land (b - 1) == p

let rec add x = function
  | Empty -> Leaf x
  | Leaf j as t ->
    if j == x then
      t
    else
      branch x (Leaf x) j t
  | Node (p, b, l, r) as t ->
    if matches_prefix x p b then
      if zero_bit x b then
        Node (p, b, add x l, r)
      else
        Node (p, b, l, add x r)
    else
      branch x (Leaf x) p t
;;

let node = function
  | _, _, Empty, t | _, _, t, Empty -> t
  | p, b, l, r -> Node (p, b, l, r)
;;

let rec remove x = function
  | Empty -> Empty
  | Leaf j as t ->
    if x == j then
      Empty
    else
      t
  | Node (p, m, t0, t1) as t ->
    if matches_prefix x p m then begin
      if zero_bit x m then
        node (p, m, remove x t0, t1)
      else
        node (p, m, t0, remove x t1)
    end else
      t
;;

let rec union t1 t2 =
  match t1, t2 with
  | Empty, t | t, Empty -> t
  | Leaf x, t | t, Leaf x -> add x t
  | Node (p1, b1, l1, r1), Node (p2, b2, l2, r2) ->
    if b1 == b2 && p1 == p2 then
      Node (p1, b1, union l1 l2, union r1 r2)
    else if b1 < b2 && matches_prefix p2 p1 b1 then begin
      if zero_bit p2 b1 then
        Node (p1, b1, union l1 t2, r1)
      else
        Node (p1, b1, l1, union r1 t2)
    end else if b1 > b2 && matches_prefix p1 p2 b2 then begin
      if zero_bit p1 b2 then
        Node (p2, b2, union t1 l2, r2)
      else
        Node (p2, b2, l2, union t1 r2)
    end else
      branch p1 t1 p2 t2
;;
