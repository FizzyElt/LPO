type t =
  { rank : int array;
    link : int array
  }

let create n = { rank = Array.make n 0; link = Array.init n (fun i -> i) }

let rec find t i =
  let p = t.link.(i) in
  if p = i then
    i
  else begin
    let r = find t p in
    t.link.(i) <- r;
    r
  end
;;

let union t i j =
  let ri = find t i
  and rj = find t j in
  if ri <> rj then begin
    if t.rank.(ri) < t.rank.(rj) then
      t.link.(ri) <- rj
    else begin
      t.link.(rj) <- ri;
      if t.rank.(ri) = t.rank.(rj) then t.rank.(ri) <- t.rank.(ri) + 1
    end
  end
;;
