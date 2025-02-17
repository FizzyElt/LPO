let lines = ref []

let () =
  try
    while true do
      lines := read_line () :: !lines
    done
  with
  | End_of_file -> ()
;;

let rec print l =
  match l with
  | [] -> ()
  | s :: r ->
    print_endline s;
    print r
;;

let () = print !lines
