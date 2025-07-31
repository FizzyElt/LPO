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
    | Node of t * elt * t * int

  let empty = Empty

  let height = function
    | Empty -> 0
    | Node (_, _, _, h) -> h
  ;;

  let node l v r = Node (l, v, r, 1 + max (height l) (height r))

  let rec min_elt = function
    | Empty -> raise Not_found
    | Node (Empty, x, _, _) -> x
    | Node (l, _, _, _) -> min_elt l
  ;;

  let rec mem x = function
    | Empty -> false
    | Node (l, v, r, _) ->
      let c = X.compare x v in
      c = 0
      ||
      if c < 0 then
        mem x l
      else
        mem x r
  ;;

  let rec max_elt = function
    | Empty -> raise Not_found
    | Node (_, x, Empty, _) -> x
    | Node (_, _, r, _) -> max_elt r
  ;;

  let balance l v r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 1 then begin
      match l with
      | Node (ll, lv, lr, _) when height ll >= height lr ->
        node ll lv (node lr v r)
      | Node (ll, lv, Node (lrl, lrv, lrr, _), _) ->
        node (node ll lv lrl) lrv (node lrr v r)
      | _ -> assert false
    end else if hr > hl + 1 then begin
      match r with
      | Node (rl, rv, rr, _) when height rr >= height rl ->
        node (node l v rl) rv rr
      | Node (Node (rll, rlv, rlr, _), rv, rr, _) ->
        node (node l v rll) rlv (node rlr rv rr)
      | _ -> assert false
    end else
      node l v r
  ;;

  let rec add x t =
    match t with
    | Empty -> Node (Empty, x, Empty, 1)
    | Node (l, v, r, _) ->
      let c = X.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        balance (add x l) v r
      else
        balance l v (add x r)
  ;;

  let rec remove_min_elt = function
    | Empty -> Empty
    | Node (Empty, _, r, _) -> r
    | Node (l, v, r, _) -> balance (remove_min_elt l) v r
  ;;

  let merge tl tr =
    match tl, tr with
    | Empty, t | t, Empty -> t
    | _ -> balance tl (min_elt tr) (remove_min_elt tr)
  ;;

  let rec remove x = function
    | Empty -> Empty
    | Node (l, v, r, _) ->
      let c = X.compare x v in
      if c = 0 then
        merge l r
      else if c < 0 then
        balance (remove x l) v r
      else
        balance l v (remove x r)
  ;;

  let rec cardinal = function
    | Empty -> 0
    | Node (l, _, r, _) -> cardinal l + cardinal r + 1
  ;;
end
