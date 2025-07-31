let lt x y = compare x y < 0
let le x y = compare x y <= 0

let rec list_partition ((left, right) as acc) p = function
  | [] -> acc
  | x :: s when le x p -> list_partition (x :: left, right) p s
  | x :: s -> list_partition (left, x :: right) p s
;;

let rec list_quicksort = function
  | [] -> []
  | p :: s ->
    let left, right = list_partition ([], []) p s in
    list_quicksort left @ (p :: list_quicksort right)
;;

let swap a i j =
  let t = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- t
;;

let array_partition a l r =
  let p = a.(l) in
  let m = ref l in
  for i = l + 1 to r - 1 do
    if le a.(i) p then begin
      incr m;
      swap a i !m
    end
  done;
  if l <> !m then swap a l !m;
  !m
;;

let rec quick_rec a l r =
  if l < r then begin
    let m = array_partition a l r in
    quick_rec a l m;
    quick_rec a (m + 1) r
  end
;;

let array_quicksort a = quick_rec a 0 (Array.length a)
