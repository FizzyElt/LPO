type vertex = int
type t = vertex list array

let create n = Array.make n []

let nb_vertex = Array.length

let mem_edge g v1 v2 = List.mem v2 g.(v1)

let add_edge g v1 v2 = if not (mem_edge g v1 v2) then g.(v1) <- v2 :: g.(v1)

let remove_edge g v1 v2 = g.(v1) <- List.filter (( <> ) v2) g.(v1)

let iter_succ f g v = List.iter f g.(v)

let iter_edge f g =
  for v = 0 to nb_vertex g - 1 do
    iter_succ (f v) g v
  done
;;
