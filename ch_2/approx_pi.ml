open Graphics

let n = read_int ()

let () =
  open_graph " 1000x1000";
  draw_circle 0 0 1000;
  let p = ref 0 in
  for _k = 1 to n do
    let x = Random.float 1.0 in
    let y = Random.float 1.0 in
    if (x *. x) +. (y *. y) <= 1.0 then p := !p + 1;
    fill_circle (truncate (x *. 1000.)) (truncate (y *. 1000.)) 4
  done;
  let pi = 4.0 *. float !p /. float n in
  Printf.printf "%f\n" pi;
  ignore (read_key ())
;;
