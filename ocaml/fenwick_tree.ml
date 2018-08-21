class fenwick_tree n zero op =
  object (self: 'self)
  val a = Array.make n zero
  method update x (v:int) =
    let rec loop i =
      if i < n then begin
        a.(i) <- op a.(i) v;
        loop (i lor (i + 1))
      end
    in
    loop x
  method reduce x =
    let rec loop r i = if i >= 0 then loop (op r a.(i)) ((i land (i + 1)) - 1) else r in
    loop zero x
end;;
