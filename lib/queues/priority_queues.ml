module type OrderedWithDummy = sig
  type t
  val compare : t -> t -> int
  val dummy : t
end

module ImperativePriorityQueue = struct
  open Resizeable_array

  module type ImperativePriorityQueue = sig
    type t
    type elt
    val create : unit -> t
    val is_empty : t -> bool
    val add : elt -> t -> unit
    val get_min : t -> elt
    val remove_min : t -> unit
  end

  module Make (X : OrderedWithDummy) (A : ResizeableArray) :
    ImperativePriorityQueue with type elt = X.t = struct
    type elt = X.t
    type t = elt A.t

    let create () = A.make 0 X.dummy

    let is_empty h = A.length h = 0

    let get_min h =
      begin
        if A.length h = 0 then
          invalid_arg "get_min"
        else
          A.get h 0
      end
    ;;

    let rec move_up h x i =
      if i = 0 then
        A.set h i x
      else begin
        let fi = (i - 1) / 2 in
        let y = A.get h fi in
        if X.compare y x > 0 then begin
          A.set h i y;
          move_up h x fi
        end else
          A.set h i x
      end
    ;;

    let add x h =
      let n = A.length h in
      A.resize h (n + 1);
      move_up h x n
    ;;

    let min h l r =
      if X.compare (A.get h r) (A.get h l) < 0 then
        r
      else
        l
    ;;

    let smallest_node h x i =
      let l = (2 * i) + 1 in
      let n = A.length h in
      if l >= n then
        i
      else begin
        let r = l + 1 in
        let j =
          if r < n then
            min h l r
          else
            l
        in
        if X.compare (A.get h j) x < 0 then
          j
        else
          i
      end
    ;;

    let rec move_down h x i =
      let j = smallest_node h x i in
      if j = i then
        A.set h i x
      else begin
        A.set h i (A.get h j);
        move_down h x j
      end
    ;;

    let remove_min h =
      let n = A.length h - 1 in
      if n < 0 then invalid_arg "remove_min";
      let x = A.get h n in
      A.resize h n;
      if n > 0 then move_down h x 0
    ;;
  end
end

module PersistentPriorityQueue = struct
  module type PersistentPriorityQueue = sig
    type t
    type elt
    val empty : t
    val is_empty : t -> bool
    val add : elt -> t -> t
    val get_min : t -> elt
    val remove_min : t -> t
  end

  module type Ordered = sig
    type t
    val compare : t -> t -> int
  end

  module Make (X : Ordered) : PersistentPriorityQueue with type elt = X.t =
  struct
    type elt = X.t

    type t =
      | Empty
      | Node of t * elt * t

    let empty = Empty

    let is_empty h = h = Empty

    let get_min = function
      | Empty -> invalid_arg "get_min"
      | Node (_, x, _) -> x
    ;;

    let rec merge ha hb =
      match ha, hb with
      | Empty, h | h, Empty -> h
      | Node (la, xa, ra), Node (lb, xb, rb) ->
        if X.compare xa xb <= 0 then
          Node (ra, xa, merge la hb)
        else
          Node (rb, xb, merge lb ha)
    ;;

    let add x h = merge (Node (Empty, x, Empty)) h

    let remove_min = function
      | Empty -> invalid_arg "remove_min"
      | Node (a, _, b) -> merge a b
    ;;
  end
end
