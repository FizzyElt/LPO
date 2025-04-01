module type ImperativeSet = sig
  type elt
  type t

  val create : unit -> t
  val add : elt -> t -> unit
  val mem : elt -> t -> bool
  val remove : elt -> t -> unit
  val cardinal : t -> int
end

module type HashType = sig
  type t

  val hash : t -> int
  val equal : t -> t -> bool
end

module Make (X : HashType) : ImperativeSet with type elt = X.t = struct
  type elt = X.t
  type t =
    { mutable size : int;
      mutable buckets : elt list array
    }

  let array_length = 5003
  let cardinal h = h.size

  let create () = { size = 0; buckets = Array.make array_length [] }

  let bucket_of x h = X.hash x land max_int mod Array.length h.buckets

  let mem_bucket x b = List.exists (X.equal x) b

  let mem x h =
    let i = bucket_of x h in
    mem_bucket x h.buckets.(i)
  ;;

  let resize h =
    let n = Array.length h.buckets in
    let m = n * 2 in
    let a = Array.make m [] in
    let rehash x =
      let i = X.hash x land max_int mod m in
      a.(i) <- x :: a.(i)
    in
    Array.iter (List.iter rehash) h.buckets;
    h.buckets <- a
  ;;

  let add x h =
    let n = Array.length h.buckets in
    let i = bucket_of x h in
    let b = h.buckets.(i) in
    if not (mem_bucket x b)
    then begin
      h.size <- h.size + 1;
      h.buckets.(i) <- x :: b;
      if h.size > n / 2 then resize h
    end
  ;;

  let remove x h =
    let i = bucket_of x h in
    let b = h.buckets.(i) in
    if mem_bucket x b
    then begin
      h.size <- h.size - 1;
      h.buckets.(i) <- List.filter (fun y -> not (X.equal x y)) b
    end
  ;;
end
