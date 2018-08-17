let dsu n =
  object (self: 'self)
    val p = Array.init n (fun i -> i)
    val h = Array.make n 0
    val mutable nc = n
    method findSet x =
      if p.(x) = x then x
      else begin
        let v = self#findSet p.(x) in
        p.(x) <- v;
        v
      end
    method merge x y =
      let i = self#findSet x in
      let j = self#findSet y in
      if i != j then begin
        nc <- nc - 1;
        if h.(i) < h.(j) then p.(i) <- j
        else if h.(i) > h.(j) then p.(j) <- i
        else begin
          p.(i) <- j;
          h.(j) <- h.(j) + 1
        end;
      end
    method components = nc
  end
;;
