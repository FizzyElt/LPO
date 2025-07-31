let rec gcd x y =
  if y = 0 then
    x
  else
    gcd y (x mod y)
;;

let rec extended_gcd x y =
  if y = 0 then
    1, 0, x
  else begin
    let q = x / y in
    let u, v, g = extended_gcd y (x - (q * y)) in
    v, u - (q * v), g
  end
;;

let rec exp x n =
  if n = 0 then
    1
  else begin
    let r = exp x (n / 2) in
    if n mod 2 = 0 then
      r * r
    else
      r * r * x
  end
;;

let m = 10

let () = assert (0 < m && m <= (max_int / 2) + 1)

let of_int x =
  let r = x mod m in
  if r < 0 then
    r + m
  else
    r
;;

let add x y =
  let r = x + y in
  if r >= m then
    r - m
  else
    r
;;

let sub x y =
  let r = x - y in
  if r < 0 then
    r + m
  else
    r
;;

let mul x y =
  let r = ref 0 in
  for i = Sys.word_size - 4 downto 0 do
    r := add !r !r;
    if x land (1 lsl i) <> 0 then r := add !r y
  done;
  !r
;;

let div x y =
  let u, _, g = extended_gcd y m in
  if g <> 1 then invalid_arg "div";
  mul x (of_int u)
;;

type matrix = int array array

let init_matrix n m f = Array.init n (fun i -> Array.init m (fun j -> f i j))

let id n =
  init_matrix n n (fun i j ->
    if i = j then
      1
    else
      0)
;;

let size a = Array.length a, Array.length a.(0)

let add a b =
  let ((n, m) as s) = size a in
  if size b <> s then invalid_arg "add";
  init_matrix n m (fun i j -> a.(i).(j) + b.(i).(j))
;;

let mul a b =
  let n, p = size a in
  let q, m = size b in
  if p <> q then invalid_arg "mul";
  let product i j =
    let s = ref 0 in
    for k = 0 to p - 1 do
      s := !s + (a.(i).(k) * b.(k).(j))
    done;
    !s
  in
  init_matrix n m product
;;
