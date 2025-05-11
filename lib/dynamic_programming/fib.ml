let memo = Hashtbl.create 17

let rec fib_memo n =
  try Hashtbl.find memo n with
  | Not_found ->
    let fn = if n <= 1 then n else fib_memo (n - 2) + fib_memo (n - 1) in
    Hashtbl.add memo n fn;
    fn
;;

let fib_dp n =
  if n = 0
  then 0
  else (
    let f = Array.make (n + 1) 0 in
    f.(1) <- 1;
    for i = 2 to n do
      f.(i) <- f.(i - 2) + f.(i - 1)
    done;
    f.(n))
;;

let memo_rec ff =
  let h = Hashtbl.create 5003 in
  let rec f x =
    match Hashtbl.find_opt h x with
    | Some v -> v
    | None ->
      let v = ff f x in
      Hashtbl.add h x v;
      v
  in
  f
;;

let comb_smart_dp n k =
  let row = Array.make (n + 1) 0 in
  row.(0) <- 1;
  for i = 1 to n do
    for j = i downto 1 do
      row.(j) <- row.(j) + row.(j - 1)
    done
  done;
  row.(k)
;;

type tree =
  | E
  | N of int * tree * char * tree

let empty = E

let unique = function
  | E -> 0
  | N (u, _, _, _) -> u
;;

module X = struct
  type t = tree
  let hash = function
    | E -> 0
    | N (_, l, c, r) ->
      ((19 * ((19 * unique l) + Char.code c)) + unique r) land max_int
  ;;

  let equal t1 t2 =
    match t1, t2 with
    | E, E -> true
    | N (_, l1, c1, r1), N (_, l2, c2, r2) -> l1 == l2 && c1 == c2 && r1 == r2
    | _ -> false
  ;;
end

module W = Weak.Make (X)

let nodes = W.create 5003

let node =
  let cpt = ref 1 in
  fun l c r ->
    let n0 = N (!cpt, l, c, r) in
    let n = W.merge nodes n0 in
    if n == n0 then incr cpt;
    n
;;
