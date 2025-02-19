module type Bitv = sig
  type t
  val create : int -> bool -> t
  val length : t -> int
  val get : t -> int -> bool
  val set : t -> int -> bool -> unit
end

let bpi = Sys.word_size - 1

let max_length = Sys.max_array_length * bpi

module BitVectors : Bitv = struct
  type t =
    { length : int
    ; bits : int array
    }

  let length v = v.length

  let create n b =
    let initv = if b then -1 else 0 in
    let q = n / bpi
    and r = n mod bpi in
    if r = 0
    then { length = n; bits = Array.make q initv }
    else begin
      let a = Array.make (q + 1) initv in
      if b then a.(q) <- (1 lsl r) - 1;
      { length = n; bits = a }
    end
  ;;

  let get v n =
    let i = n / bpi
    and j = n mod bpi in
    (v.bits.(i) lsr j) land 1 <> 0
  ;;

  let set v n b =
    let i = n / bpi
    and j = n mod bpi in
    if b
    then v.bits.(i) <- v.bits.(i) lor (1 lsl j)
    else v.bits.(i) <- v.bits.(i) land lnot (1 lsl j)
  ;;

  let inter v1 v2 =
    let l1 = v1.length in
    if l1 <> v2.length then invalid_arg "Bitv.inter";
    let b = Array.mapi (fun i ei -> ei land v2.bits.(i)) v1.bits in
    { length = l1; bits = b }
  ;;

  let normalize v =
    let r = v.length mod bpi in
    if r > 0
    then (
      let s = Array.length v.bits - 1 in
      v.bits.(s) <- v.bits.(s) land ((1 lsl r) - 1))
  ;;

  let compl v =
    let b = Array.map lnot v.bits in
    let r = { length = v.length; bits = b } in
    normalize r;
    r
  ;;

  let ntz x = failwith "not implemented"

  let iter_true f v =
    Array.iteri
      (fun i ei ->
         let index = i * bpi in
         let rec visit x =
           if x <> 0
           then begin
             let b = x land -x in
             f (index + ntz b);
             visit (x - b)
           end
         in
         visit ei)
      v.bits
  ;;
end
