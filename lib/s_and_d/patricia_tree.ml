type t =
  | Empty
  | Leaf of int
  | Node of int * int * t * t

let zero_bit x b = x land b == 0

let rec mem x = function
  | Empty -> false
  | Leaf j -> x = j
  | Node (_, b, l, r) -> mem x (if zero_bit x b then l else r)
;;
