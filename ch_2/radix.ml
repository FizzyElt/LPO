let base = int_of_string Sys.argv.(1)

let list_of_string s =
  let digits = ref [] in
  for i = 0 to String.length s - 1 do
    digits := s.[i] :: !digits
  done;
  !digits
;;

let digit_of_char c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'A' .. 'Z' -> Char.code c - Char.code 'A' + 10
  | c ->
    Printf.eprintf "invalid character %c\n" c;
    exit 1
;;

let check_digit d =
  if d < 0 || d >= base
  then (
    Printf.eprintf "invalid digit %d\n" d;
    exit 1)
;;

let () =
  while true do
    let s = read_line () in
    let cl = list_of_string s in
    let dl = List.map digit_of_char cl in
    List.iter check_digit dl;
    let v = List.fold_right (fun d acc -> d + (base * acc)) dl 0 in
    Printf.printf "%d\n" v
  done
;;
