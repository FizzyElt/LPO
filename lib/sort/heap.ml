let lt x y = compare x y < 0
let le x y = compare x y <= 0

module PriorityQueue = Lib.Priority_queues.ImperativePriorityQueue

module Heap =
  PriorityQueue.Make
    (struct
      type t = int
      let compare x y = compare y x
      let dummy = 100
    end)
    (Lib.Resizeable_array.ResizeableArray)

let list_heap_sort l =
  let h = Heap.create () in
  List.iter (fun x -> Heap.add x h) l;
  let res = ref [] in
  while not (Heap.is_empty h) do
    let x = Heap.get_min h in
    Heap.remove_min h;
    res := x :: !res
  done;
  !res
;;

let rec move_down a k v n =
  let r = (2 * k) + 1 in
  if r >= n then
    a.(k) <- v
  else (
    let rmax =
      if r + 1 < n then
        if lt a.(r) a.(r + 1) then
          r + 1
        else
          r
      else
        r
    in
    if le a.(rmax) v then
      a.(k) <- v
    else begin
      a.(k) <- a.(rmax);
      move_down a rmax v n
    end
  )
;;

let array_heap_sort a =
  let n = Array.length a in
  for k = (n / 2) - 1 downto 0 do
    move_down a k a.(k) n
  done;
  for k = n - 1 downto 1 do
    let v = a.(k) in
    a.(k) <- a.(0);
    move_down a 0 v k
  done
;;
