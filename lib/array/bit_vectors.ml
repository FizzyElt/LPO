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

  let ntz x =
    let rec count_zeros n count =
      if n land 1 = 1
      then count
      else begin
        count_zeros (n lsr 1) (count + 1)
      end
    in
    count_zeros x 0
  ;;

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

  let table_bits =
    [| 0
     ; 1
     ; 1
     ; 2
     ; 1
     ; 2
     ; 2
     ; 3
     ; 1
     ; 2
     ; 2
     ; 3
     ; 2
     ; 3
     ; 3
     ; 4
     ; 1
     ; 2
     ; 2
     ; 3
     ; 2
     ; 3
     ; 3
     ; 4
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 1
     ; 2
     ; 2
     ; 3
     ; 2
     ; 3
     ; 3
     ; 4
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 1
     ; 2
     ; 2
     ; 3
     ; 2
     ; 3
     ; 3
     ; 4
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 4
     ; 5
     ; 5
     ; 6
     ; 5
     ; 6
     ; 6
     ; 7
     ; 1
     ; 2
     ; 2
     ; 3
     ; 2
     ; 3
     ; 3
     ; 4
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 4
     ; 5
     ; 5
     ; 6
     ; 5
     ; 6
     ; 6
     ; 7
     ; 2
     ; 3
     ; 3
     ; 4
     ; 3
     ; 4
     ; 4
     ; 5
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 4
     ; 5
     ; 5
     ; 6
     ; 5
     ; 6
     ; 6
     ; 7
     ; 3
     ; 4
     ; 4
     ; 5
     ; 4
     ; 5
     ; 5
     ; 6
     ; 4
     ; 5
     ; 5
     ; 6
     ; 5
     ; 6
     ; 6
     ; 7
     ; 4
     ; 5
     ; 5
     ; 6
     ; 5
     ; 6
     ; 6
     ; 7
     ; 5
     ; 6
     ; 6
     ; 7
     ; 6
     ; 7
     ; 7
     ; 8
    |]
  ;;

  let pop x =
    let rec count n x =
      if x = 0
      then n
      else begin
        let mask = x land (1 lsl 8) in
        count (n + table_bits.(mask)) (x lsr 8)
      end
    in
    count 0 x
  ;;

  let cardinal v = Array.fold_left (fun n x -> n + pop x) 0 v.bits

  let blit_bits x i n v j =
    let mask = ((1 lsl n) - 1) lsl i in
    let copy_mask = x land mask in
    let copy_mask =
      if i < j
      then copy_mask lsl (j - i)
      else if i > j
      then copy_mask lsr (i - j)
      else copy_mask
    in

    let mask = ((1 lsl n) - 1) lsl j in
    let v_mask = v land mask in
    let v_mask = v_mask lxor v in

    v_mask lor copy_mask
  ;;

  let blit v i v2 j n =
    if n < 0 || i < 0 || j < 0 || i + n > v.length || j + n > +v2.length
    then invalid_arg "blit: invalid bit range";

    let word_size = Sys.word_size in

    let rec loop src_idx dst_idx remaining =
      if remaining = 0
      then v2
      else begin
        let src_word_idx = src_idx / word_size in
        let src_bit_idx = src_idx mod word_size in
        let dst_word_idx = dst_idx / word_size in
        let dst_bit_idx = dst_idx mod word_size in

        let bits_to_process =
          min
            remaining
            (min (word_size - src_bit_idx) (word_size - dst_bit_idx))
        in

        let src_word =
          if src_word_idx < Array.length v.bits
          then v.bits.(src_word_idx)
          else 0
        in

        let dst_word =
          if dst_word_idx < Array.length v2.bits
          then v2.bits.(dst_word_idx)
          else 0
        in

        let new_word =
          blit_bits src_word src_bit_idx bits_to_process dst_word dst_bit_idx
        in

        if dst_word_idx >= Array.length v2.bits
        then invalid_arg "blit: destination array too small";

        v2.bits.(dst_word_idx) <- new_word;

        loop
          (src_idx + bits_to_process)
          (dst_idx + bits_to_process)
          (remaining - bits_to_process)
      end
    in
    loop i j n
  ;;

  let sub v i n =
    if n < 0 || i < 0 || i + n > v.length
    then invalid_arg "sub: invalid bit range";

    let v2 = create n false in

    blit v i v2 0 n
  ;;

  let append v1 v2 =
    let len1 = v1.length in
    let len2 = v2.length in
    let v = create (len1 + len2) false in
    let v = blit v1 0 v 0 len1 in
    let v = blit v2 0 v len1 len2 in
    v
  ;;

  let fill v i n b =
    let fill_v = create v.length b in
    blit fill_v i v i n
  ;;
end
