let lt x y = compare x y < 0
let le x y = compare x y <= 0

let rec list_split l1 l2 = function
  | [] -> l1, l2
  | x :: l -> list_split (x :: l2) l1 l
;;

let rec list_merge l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | x1 :: s1, x2 :: s2 ->
    if le x1 x2 then x1 :: list_merge s1 l2 else x2 :: list_merge l1 s2
;;

let rec list_merge_sort l =
  match l with
  | [] | [ _ ] -> l
  | _ ->
    let l1, l2 = list_split [] [] l in
    list_merge (list_merge_sort l1) (list_merge_sort l2)
;;

let array_merge a1 a2 l m r =
  let i = ref l in
  let j = ref m in
  for k = l to r - 1 do
    if !i < m && (!j = r || le a1.(!i) a1.(!j))
    then begin
      a2.(k) <- a1.(!i);
      incr i
    end
    else begin
      a2.(k) <- a1.(!j);
      incr j
    end
  done
;;

let array_merge_sort a =
  let tmp = Array.copy a in
  let rec merge_sort_rec l r =
    if l < r - 1
    then begin
      let m = (l + r) / 2 in
      merge_sort_rec l m;
      merge_sort_rec m r;
      Array.blit a l tmp l (r - 1);
      array_merge tmp a l m r
    end
  in
  merge_sort_rec 0 (Array.length a)
;;
