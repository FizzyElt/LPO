type 'a tree =
  | E
  | N of 'a tree * 'a * 'a tree

type 'a path =
  | Top
  | Left of 'a path * 'a * 'a tree
  | Right of 'a tree * 'a * 'a path

type 'a zipper =
  { path : 'a path;
    tree : 'a tree
  }

let of_tree t = { path = Top; tree = t }

let down_left z =
  match z.tree with
  | E -> invalid_arg "down_left"
  | N (l, x, r) -> { path = Left (z.path, x, r); tree = l }
;;

let down_right z =
  match z.tree with
  | E -> invalid_arg "down_left"
  | N (l, x, r) -> { path = Right (l, x, z.path); tree = r }
;;

let up z =
  match z.path with
  | Top -> invalid_arg "up"
  | Left (p, x, r) -> { path = p; tree = N (z.tree, x, r) }
  | Right (l, x, p) -> { path = p; tree = N (l, x, z.tree) }
;;

let rec to_tree z = if z.path = Top then z.tree else to_tree (up z)

let rec leftmost z = function
  | E -> z
  | N (l, x, r) -> leftmost (Left (z, x, r)) l
;;

let rec compare cmp z1 z2 =
  match z1, z2 with
  | Top, Top -> 0
  | Left (z1, x1, r1), Left (z2, x2, r2) ->
    let c = cmp x1 x2 in
    if c <> 0 then c else compare cmp (leftmost z1 r1) (leftmost z2 r2)
  | Top, Left _ -> -1
  | Left _, Top -> 1
  | Right _, _ | _, Right _ -> assert false
;;

let compare_tree cmp t1 t2 = compare cmp (leftmost Top t1) (leftmost Top t2)

(* cursor *)
type 'a enum =
  | Top
  | Left of 'a * 'a tree * 'a enum

let rec leftmost t e =
  match t with
  | E -> e
  | N (l, x, r) -> leftmost l (Left (x, r, e))
;;
let start t = leftmost t Top
let step = function
  | Top -> raise Exit
  | Left (x, r, e) -> x, leftmost r e
;;
