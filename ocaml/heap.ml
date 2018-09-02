class heap n (default_value:int) =
  object (self: 'self)
  val a = Array.make n default_value
  val h = Array.make (n+1) 0
  val g = Array.make n 0
  val mutable size = 0

  method heapify_front k =
    let he = h.(k) in
    let rec loop i =
      let o = i lsl 1 in
      if o <= size then begin
        let j = if o < size && a.(h.(o + 1)) < a.(h.(o)) then o + 1 else o in
        if a.(h.(j)) >= a.(he) then i
        else begin
          h.(i) <- h.(j);
          g.(h.(i)) <- i;
          loop j
        end
      end else i
    in
    let i = loop k in
    if i <> k then begin
      h.(i) <- he;
      g.(he) <- i;
    end

  method heapify_back k =
    let he = h.(k) in
    let rec loop i =
      if i > 1 then begin
        let j = i lsr 1 in
        if a.(he) >= a.(h.(j)) then i
        else begin
          h.(i) <- h.(j);
          g.(h.(i)) <- i;
          loop j
        end
      end else i
    in
    let i = loop k in
    if i <> k then begin
      h.(i) <- he;
      g.(he) <- i;
    end

  method insert k = begin
    size <- succ size;
    h.(size) <- k;
    g.(k) <- size;
    self#heapify_back size
  end

  method decrease_key k value = begin
    a.(k) <- value;
    let pos = g.(k) in
    if pos = 0 then self#insert k else self#heapify_back pos
  end

  method get k = a.(k)
  method is_empty = size = 0

  method extract_min () = begin
    assert (size > 0);
    let he = h.(1) in
    g.(he) <- 0;
    size <- pred size;
    if size > 0 then begin
      h.(1) <- h.(size + 1);
      g.(h.(1)) <- 1;
      self#heapify_front 1
    end;
    he
  end

end;;
