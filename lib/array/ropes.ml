module type STRING = sig
  type t
  type char
  val length : t -> int
  val empty : t
  val make : int -> char -> t
  val get : t -> int -> char
  val append : t -> t -> t
  val sub : t -> int -> int -> t
end

module type ROPE = sig
  module S : STRING
  include STRING with type char = S.char

  val small_length : int

  val of_string : S.t -> t
  val set : t -> int -> char -> t
  val delete_char : t -> int -> t
  val insert_char : t -> int -> char -> t
  val insert : t -> int -> t -> t
end

module type SmallLength = sig
  val small_length : int
end

module Make (X : STRING) (SL : SmallLength) : ROPE with module S = X = struct
  module S = X

  type char = S.char

  type t =
    | Str of S.t * int * int
    | App of t * t * int

  let empty = Str (S.empty, 0, 0)

  let length = function
    | Str (_, _, n) | App (_, _, n) -> n
  ;;

  let of_string s = Str (s, 0, S.length s)

  let make n c = of_string (S.make n c)

  let rec unsafe_get t i =
    match t with
    | Str (s, ofs, _) -> S.get s (ofs + i)
    | App (t1, t2, _) ->
      let n1 = length t1 in
      if i < n1 then
        unsafe_get t1 i
      else
        unsafe_get t2 (i - n1)
  ;;

  let get t i =
    if i < 0 || i >= length t then invalid_arg "get";
    unsafe_get t i
  ;;

  let small_length = SL.small_length

  let append_string s1 ofs1 len1 s2 ofs2 len2 =
    Str (S.append (S.sub s1 ofs1 len1) (S.sub s2 ofs2 len2), 0, len1 + len2)
  ;;

  let append t1 t2 =
    match t1, t2 with
    | Str (_, _, 0), t | t, Str (_, _, 0) -> t
    | Str (s1, ofs1, len1), Str (s2, ofs2, len2)
      when len1 <= small_length && len2 <= small_length ->
      append_string s1 ofs1 len1 s2 ofs2 len2
    | App (t1, Str (s1, ofs1, len1), _), Str (s2, ofs2, len2)
      when len1 <= small_length && len2 <= small_length ->
      App (t1, append_string s1 ofs1 len1 s2 ofs2 len2, length t1 + len1 + len2)
    | Str (s1, ofs1, len1), App (Str (s2, ofs2, len2), t2, _)
      when len1 <= small_length && len2 <= small_length ->
      App (append_string s1 ofs1 len1 s2 ofs2 len2, t2, len1 + len2 + length t2)
    | t1, t2 -> App (t1, t2, length t1 + length t2)
  ;;

  let ( ++ ) = append

  let rec mksub start stop t =
    if start = 0 && stop = length t then
      t
    else (
      match t with
      | Str (s, ofs, _) -> Str (s, ofs + start, stop - start)
      | App (t1, t2, _) ->
        let n1 = length t1 in
        if stop <= n1 then
          mksub start stop t1
        else if start >= n1 then
          mksub (start - n1) (stop - n1) t2
        else
          mksub start n1 t1 ++ mksub 0 (stop - n1) t2
    )
  ;;

  let sub t ofs len =
    let stop = ofs + len in
    if ofs < 0 || len < 0 || stop > length t then invalid_arg "sub";
    if len = 0 then
      empty
    else
      mksub ofs stop t
  ;;

  let set t i c =
    let n = length t in
    if i < 0 || i >= n then invalid_arg "set";
    let ( ++ ) = S.append in
    let rec loop t =
      match t with
      | App (t1, t2, len) -> App (loop t1, loop t2, len)
      | Str (s, ofs, len) ->
        if i < ofs || i >= ofs + len then
          Str (s, ofs, len)
        else begin
          let new_s =
            S.sub s 0 (i - ofs) ++ S.make 1 c ++ S.sub s (i - ofs + 1) len
          in
          Str (new_s, ofs, len)
        end
    in
    loop t
  ;;

  let rec split_at t i =
    match t with
    | Str (s, ofs, len) ->
      let left = S.sub s ofs i in
      let right = S.sub s (ofs + i) (len - i) in
      Str (left, 0, S.length left), Str (right, ofs + i, S.length right)
    | App (t1, t2, len) -> begin
      if i < 0 || i > len then invalid_arg "split_at";
      let left_len = length t1 in
      if i = left_len then
        t1, t2
      else if i < left_len then (
        let left1, left2 = split_at t1 i in
        left1, left2 ++ t2
      ) else begin
        let right1, right2 = split_at t2 (i - left_len) in
        t1 ++ right1, right2
      end
    end
  ;;

  let insert t i r =
    let n = length t in
    if i < 0 || i > n then invalid_arg "insert";
    let left, right = split_at t i in
    left ++ r ++ right
  ;;

  let insert_char t i c = insert t i (make 1 c)
  let delete_char t i =
    let n = length t in
    if i < 0 || i >= n then invalid_arg "delete_char";
    sub t 0 i ++ sub t (i + 1) (n - i - 1)
  ;;

  let delete_char t i =
    let n = length t in
    if i < 0 || i >= n then invalid_arg "delete_char";
    let ( ++ ) = S.append in
    let rec loop t i =
      match t with
      | Str (s, ofs, len) ->
        if i < ofs then
          Str (s, ofs, len)
        else if i >= ofs + len then
          Str (s, ofs - 1, len)
        else
          Str (S.sub s 0 (i - ofs) ++ S.sub s (i - ofs + 1) len, ofs, len - 1)
      | App (t1, t2, _) ->
        let left = loop t1 i in
        let right = loop t2 i in
        (match left, right with
         | Str (_, _, 0), r -> r
         | l, Str (_, _, 0) -> l
         | l, r -> App (l, r, length l + length r))
    in

    loop t i
  ;;

  let rec iter_leaves f t =
    match t with
    | Str (s, ofs, len) -> f s ofs len
    | App (t1, t2, _) ->
      iter_leaves f t1;
      iter_leaves f t2
  ;;
end
