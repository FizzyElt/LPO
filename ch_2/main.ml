let matrix = Array.make 3 (Array.make 2 false)

let () =
  if matrix.(0) == matrix.(2)
  then Printf.printf "equal memory"
  else Printf.printf "different memory"
;;
