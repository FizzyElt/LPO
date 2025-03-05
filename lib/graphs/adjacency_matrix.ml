type vertex = int
type t = bool array array

let create n = Array.make_matrix n n false

let nb_vertex = Array.length

let mem_edge g v1 v2 = g.(v1).(v2)
let add_edge g v1 v2 = g.(v1).(v2) <- true

let remove_edge g v1 v2 = g.(v1).(v2) <- false

let iter_succ f g v = Array.iteri (fun w b -> if b then f w) g.(v)

let iter_edge f g =
  for v = 0 to nb_vertex g - 1 do
    iter_succ f g v
  done
;;
