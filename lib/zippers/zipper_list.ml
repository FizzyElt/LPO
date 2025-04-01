type 'a zipper =
  { left : 'a list;
    right : 'a list
  }

let of_list l = { left = []; right = l }

let move_right z =
  match z.right with
  | [] -> invalid_arg "move_right"
  | x :: r -> { left = x :: z.left; right = r }
;;

let to_list z = List.rev_append z.left z.right

let insert z x = { z with left = x :: z.left }

let delete_left z =
  match z.left with
  | [] -> invalid_arg "delete_left"
  | _ :: l -> { z with left = l }
;;

let delete_right z =
  match z.left with
  | [] -> invalid_arg "delete_right"
  | _ :: r -> { z with right = r }
;;
