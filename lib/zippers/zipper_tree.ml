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
