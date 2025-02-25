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

module Make (X : STRING) (C : SmallLength) : ROPE with module S = X = struct
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
      if i < n1 then unsafe_get t1 i else unsafe_get t2 (i - n1)
  ;;

  let get t i =
    if i < 0 || i >= length t then invalid_arg "get";
    unsafe_get t i
  ;;

  let small_length = C.small_length

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
    if start = 0 && stop = length t
    then t
    else (
      match t with
      | Str (s, ofs, _) -> Str (s, ofs + start, stop - start)
      | App (t1, t2, _) ->
        let n1 = length t1 in
        if stop <= n1
        then mksub start stop t1
        else if start >= n1
        then mksub (start - n1) (stop - n1) t2
        else mksub start n1 t1 ++ mksub 0 (stop - n1) t2)
  ;;

  let sub t ofs len =
    let stop = ofs + len in
    if ofs < 0 || len < 0 || stop > length t then invalid_arg "sub";
    if len = 0 then empty else mksub ofs stop t
  ;;

  let set t i c =
    let n = length t in
    if i < 0 || i >= n then invalid_arg "set";
    sub t 0 i ++ make 1 c ++ sub t (i + 1) (n - i - 1)
  ;;

  let insert t i r =
    let n = length t in
    if i < 0 || i > n then invalid_arg "insert";
    sub t 0 i ++ r ++ sub t i (n - i)
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
    sub t 0 i ++ sub t (i + 1) (n - i - 1)
  ;;

  let rec iter_leaves f t =
    match t with
    | Str (s, ofs, len) -> f s ofs len
    | App (t1, t2, _) ->
      iter_leaves f t1;
      iter_leaves f t2
  ;;
end
