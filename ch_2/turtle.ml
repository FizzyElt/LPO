module type ANGLE = sig
  type t
  val of_degrees : float -> t
  val add : t -> t -> t
  val cos : t -> float
  val sin : t -> float
end

module Angle : ANGLE = struct
  type t = float
  let add = ( +. )
  let pi_over_180 = atan 1. /. 45.
  let of_degrees d = d *. pi_over_180
  let cos = cos
  let sin = sin
end

module Turtle (A : ANGLE) = struct
  let draw = ref true
  let pen_down () = draw := true
  let pen_up () = draw := false

  let angle = ref (A.of_degrees 0.)
  let rotate_left d = angle := A.add !angle (A.of_degrees d)
  let rotate_right d = rotate_left (-.d)

  open Graphics
  let tx = ref 600.
  let ty = ref 600.
  let () =
    open_graph " 1200x1200";
    set_line_width 2;
    moveto (truncate !tx) (truncate !ty)
  ;;

  let advance d =
    tx := !tx +. (d *. A.cos !angle);
    ty := !ty +. (d *. A.sin !angle);
    if !draw
    then lineto (truncate !tx) (truncate !ty)
    else moveto (truncate !tx) (truncate !ty)
  ;;
end

module T = Turtle (Angle)

let square d =
  for _k = 1 to 4 do
    T.advance d;
    T.rotate_left 90.
  done
;;

let squares d a =
  for _k = 1 to truncate (360. /. a) do
    square d;
    T.rotate_left a
  done
;;

open Graphics

let () =
  squares 150. 20.;
  ignore (read_key ())
;;
