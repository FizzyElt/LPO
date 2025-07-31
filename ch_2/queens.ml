module S = Set.Make (struct
    type t = int
    let compare = compare
  end)

let map f s = S.fold (fun x s -> S.add (f x) s) s S.empty

let rec upto n =
  if n < 0 then
    S.empty
  else
    S.add n (upto (n - 1))
;;

let rec count cols d1 d2 =
  if S.is_empty cols then
    1
  else
    S.fold
      (fun c res ->
         let d1 = map succ (S.add c d1) in
         let d2 = map pred (S.add c d2) in
         res + count (S.remove c cols) d1 d2)
      (S.diff (S.diff cols d1) d2)
      0
;;
let () =
  let n = int_of_string Sys.argv.(1) in
  Format.printf "%d@" (count (upto (n - 1)) S.empty S.empty)
;;
