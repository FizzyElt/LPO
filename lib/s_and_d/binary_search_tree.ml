module type PersistentSet = sig
  type elt
  type t
  val empty : t
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val min_elt : t -> elt
  val remove : elt -> t -> t
  val cardinal : t -> int
end

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

module Make (X : Ordered) : PersistentSet with type elt = X.t = struct
  type elt = X.t

  type t =
    | Empty
    | Node of t * elt * t

  let empty = Empty

  let rec min_elt = function
    | Empty -> raise Not_found
    | Node (Empty, x, _) -> x
    | Node (l, _, _) -> min_elt l
  ;;

  let rec max_elt = function
    | Empty -> raise Not_found
    | Node (_, x, Empty) -> x
    | Node (_, _, r) -> max_elt r
  ;;

  let rec mem x = function
    | Empty -> false
    | Node (l, v, r) ->
      let c = X.compare x v in
      c = 0 || if c < 0 then mem x l else mem x r
  ;;

  let rec add x t =
    match t with
    | Empty -> Node (Empty, x, Empty)
    | Node (l, v, r) ->
      let c = X.compare x v in
      if c = 0
      then t
      else if c < 0
      then Node (add x l, v, r)
      else Node (l, v, add x r)
  ;;

  let rec remove_min_elt = function
    | Empty -> Empty
    | Node (Empty, _, r) -> r
    | Node (l, v, r) -> Node (remove_min_elt l, v, r)
  ;;

  let merge tl tr =
    match tl, tr with
    | Empty, t | t, Empty -> t
    | _ -> Node (tl, min_elt tr, remove_min_elt tr)
  ;;

  let rec remove x = function
    | Empty -> Empty
    | Node (l, v, r) ->
      let c = X.compare x v in
      if c = 0
      then merge l r
      else if c < 0
      then Node (remove x l, v, r)
      else Node (l, v, remove x r)
  ;;

  let rec cardinal = function
    | Empty -> 0
    | Node (l, _, r) -> cardinal l + cardinal r + 1
  ;;

  let rec height = function
    | Empty -> 0
    | Node (l, _, r) -> 1 + max (height l) (height r)
  ;;

  let rec iter f = function
    | Empty -> ()
    | Node (l, v, r) ->
      iter f l;
      f v;
      iter f r
  ;;

  let rec elements = function
    | Empty -> []
    | Node (l, v, r) ->
      let rl = elements r in
      let ll = elements l in
      ll @ (v :: rl)
  ;;

  let rec floor x = function
    | Empty -> raise Not_found
    | Node (l, v, r) ->
      let c = X.compare x v in
      if c = 0 then v else if c < 0 then floor x l else right_floor r

  and right_floor = function
    | Empty -> raise Not_found
    | Node (_, v, Empty) -> v
    | Node (_, _, r) -> right_floor r
  ;;
end
