type quad =
  | White
  | Black
  | Node of quad * quad * quad * quad

let rec checker_board = function
  | 0 -> Black
  | 1 -> Node (White, Black, White, Black)
  | n ->
    let q = checker_board (n - 1) in
    Node (q, q, q, q)
;;

let rec draw x y w = function
  | White -> ()
  | Black -> Graphics.fill_rect x y w w
  | Node (q1, q2, q3, q4) ->
    let w = w / 2 in
    draw x y w q1;
    draw (x + w) y w q2;
    draw (x + w) (y + w) w q3;
    draw x (y + w) w q4
;;

let () = draw 0 0 256 (checker_board 3)
