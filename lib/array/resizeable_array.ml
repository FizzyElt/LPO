module type ResizeableArray = sig
  type 'a t
  val length : 'a t -> int
  val make : int -> 'a -> 'a t
  val resize : 'a t -> int -> unit
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
end

module ResizeableArray : ResizeableArray = struct
  type 'a t =
    { default : 'a;
      mutable size : int;
      mutable data : 'a array;
      increment : int option
    }
  let length a = a.size

  let make n d =
    { default = d; size = n; data = Array.make n d; increment = None }
  ;;

  let create n d s =
    { default = d; size = n; data = Array.make n d; increment = s }
  ;;

  let get a i =
    if i < 0 || i >= a.size then invalid_arg "get";
    a.data.(i)
  ;;

  let set a i v =
    if i < 0 || i >= a.size then invalid_arg "set";
    a.data.(i) <- v
  ;;

  let expand inc n s =
    match inc with
    | None -> max (2 * n) s
    | Some i ->
      let diff = s - n in
      if diff mod i = 0 then
        diff + n
      else
        (((diff / i) + 1) * i) + n
  ;;

  let resize a s =
    if s <= a.size then
      Array.fill a.data s (a.size - s) a.default
    else begin
      let n = Array.length a.data in
      if s > n then begin
        let n' = expand a.increment n s in
        let a' = Array.make n' a.default in
        Array.blit a.data 0 a' 0 a.size;
        a.data <- a'
      end
    end;
    a.size <- s;
    if a.size < Array.length a.data / 4 then
      a.data <- Array.sub a.data 0 (Array.length a.data / 2)
  ;;
end

module type Stack = sig
  type 'a t
  val make : int -> 'a -> 'a t
  val length : 'a t -> int
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
end

module Stack : Stack = struct
  include ResizeableArray
  let push (t : 'a t) a =
    let len = length t in
    resize t (len + 1);
    set t len a
  ;;

  let pop t =
    let v = get t (length t - 1) in
    resize t (length t - 1);
    v
  ;;
end
