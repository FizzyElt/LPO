module ImperativeQueue = struct
  type 'a cell =
    { elt : 'a;
      mutable next : 'a cell
    }

  type 'a t = 'a cell option ref

  let create () = ref None

  let is_empty q = !q = None

  let push x q =
    match !q with
    | None ->
      let rec c = { elt = x; next = c } in
      q := Some c
    | Some last ->
      let c = { elt = x; next = last.next } in
      last.next <- c;
      q := Some c
  ;;

  let pop q =
    match !q with
    | None -> invalid_arg "pop"
    | Some last when last.next == last ->
      q := None;
      last.elt
    | Some last ->
      let first = last.next in
      last.next <- first.next;
      first.elt
  ;;
end

module PersistentQueue = struct
  type 'a t = 'a list * 'a list

  let empty = [], []

  let is_empty = function
    | [], [] -> true
    | _ -> false
  ;;

  let push x (o, i) = o, x :: i

  let pop = function
    | [], [] -> invalid_arg "pop"
    | x :: o, i -> x, (o, i)
    | [], i -> begin
      match List.rev i with
      | x :: o -> x, (o, [])
      | [] -> assert false
    end
  ;;
end
