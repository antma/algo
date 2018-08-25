class bitset n =
  object
  val a = Array.make ((n + 63) lsr 6) Int64.zero
  method test k = (Int64.compare (Int64.logand a.(k lsr 6) (Int64.shift_left Int64.one (k land 63))) Int64.zero) != 0
  method set k = let j = k lsr 6 in a.(j) <- Int64.logor a.(j) (Int64.shift_left Int64.one (k land 63))
  method clear k =  let j = k lsr 6 in a.(j) <- Int64.logand a.(j) (Int64.lognot (Int64.shift_left Int64.one (n land 63)))
end

class prime_table n =
  let m = n lsr 1 in
  object
  val c = new bitset (max 1 (n lsr 1))
  initializer begin
    c#set 0;
    let rec loop i =
      let k = 2 * i + 1 in
      if k * k < n then begin
        if not (c#test i) then begin
          let rec loop2 j =
            if j < m then begin
              c#set j;
              loop2 (j + k)
            end
          in
          loop2 (2 * i * (i + 1))
        end;
        loop (i + 1)
      end
    in
    loop 1
  end
  method primes =
    let rec loop i cur =
      if (i >= 1) then loop (i - 1) (if c#test i then cur else (2 * i + 1) :: cur)
      else cur
    in
    2 :: (loop (m-1) [])
  method is_prime p =
    if p = 2 then true
    else if (p land 1) = 0 then false
    else (not (c#test (p lsr 1)))
end
