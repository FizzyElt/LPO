module Draw = struct
  open Graphics

  let left = 0.
  let right = 300.
  let down = 0.
  let up = 200.

  let ball = 5
  let paddle = 50
  let thick = 8

  let gray = rgb 220 220 220

  let init () =
    let s = Printf.sprintf " %dx%d" (truncate right) (truncate up) in
    open_graph s;
    auto_synchronize false
  ;;

  let clear () =
    set_color gray;
    fill_rect 0 0 (truncate right) (truncate up)
  ;;

  let get_paddle_pos () =
    let x = fst (mouse_pos ()) in
    max 0 (min x (truncate right - paddle))
  ;;

  let game x y =
    clear ();
    set_color black;
    fill_circle (truncate x) (truncate y) ball;
    let x = get_paddle_pos () in
    fill_rect x 0 paddle thick;
    synchronize ();
    x
  ;;
end

let bounce (x, y) (vx, vy) xp =
  let vx =
    if x <= Draw.left || x >= Draw.right then
      -.vx
    else
      vx
  in
  let vy =
    if
      (y <= float Draw.thick && x >= xp && x <= xp +. float Draw.paddle)
      || y >= Draw.up
    then
      -.vy
    else
      vy
  in
  vx, vy
;;

let new_position (x, y) (vx, vy) = x +. vx, y +. vy

let rec play (x, y) (vx, vy) =
  if y <= Draw.down then (
    Printf.eprintf "Game over!\n";
    exit 0
  );
  let xp = Draw.game x y in
  let vx, vy = bounce (x, y) (vx, vy) (float xp) in
  let x', y' = new_position (x, y) (vx, vy) in
  play (x', y') (vx, vy)
;;
