let lt x y = compare x y < 0
let le x y = compare x y <= 0

let rec insert x = function
  | y :: l when lt y x -> y :: insert x l
  | l -> x :: l
;;

let list_insertion_sort l = List.fold_left (fun acc x -> insert x acc) [] l

let array_insertion_sort a =
  for i = 1 to Array.length a - 1 do
    let v = a.(i) in
    let j = ref i in
    while 0 < !j && lt v a.(!j - 1) do
      a.(!j) <- a.(!j - 1);
      decr j
    done;
    a.(!j) <- v
  done
;;
