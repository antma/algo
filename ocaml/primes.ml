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

let sieve_array n =
  let p = Array.make (n+1) 0 in
  let rec loop k =
    if k * k <= n then begin
      let rec loop2 j =
        if j <= n then begin
          if p.(j) = 0 then p.(j) <- k;
          loop2 (j + k)
        end
      in
      if p.(k) = 0 then loop2 k;
      loop (succ k)
    end else begin
      let rec loop2 j =
        if j <= n then begin
          if p.(j) = 0 then p.(j) <- j;
          loop2 (succ j)
        end
      in
      loop2 k
    end
  in
  loop 2;
  p
;;

let prime_factorization_array n =
  let p = sieve_array n in
  let c = Array.make (n+1) 0 in
  let next = Array.make (n+1) 0 in
  for i = 2 to n do
    let k = p.(i) in let j = i / k in
    if p.(j) = k then begin
      c.(i) <- c.(j) + 1;
      next.(i) <- next.(j);
    end else begin
      c.(i) <- 1;
      next.(i) <- j;
    end
  done;
  (p, c, next)
;;

let totient_array n =
  let (p, c, next) = prime_factorization_array n in
  let phi = Array.make (n+1) 0 in
  phi.(1) <- 1;
  for i = 2 to n do
    let x = p.(i) in
    let j = next.(i) in
    let f = (i / j) / x in
    phi.(i) <- (x - 1) * f *  phi.(j)
  done;
  (phi, p, c, next)
;;
