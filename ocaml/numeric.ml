
let gcd a b =
  let rec loop a b =
    if b = 0 then a
    else loop b (a mod b)
  in
  if a < b then loop b a else loop a b
;;

let gcd64 a b =
  let rec loop a b =
    if (Int64.compare b Int64.zero) = 0 then a
    else loop b (Int64.rem a b)
  in
  if (Int64.compare a b) < 0 then loop b a else loop a b
;;

let pow64 a p =
  let rec loop a b p =
    if p > 0 then loop (if (p land 1) = 0 then a else Int64.mul a b) (Int64.mul b b) (p lsr 1)
    else a
  in
  loop Int64.one a p
;;

let predmod x q = pred (if x > 0 then x else x + q);;
let mulmod x y q = Int64.to_int (Int64.rem (Int64.mul (Int64.of_int x) (Int64.of_int y)) (Int64.of_int q));;
let powmod a p q =
  let rec loop a b p =
    if p > 0 then loop (if (p land 1) = 0 then a else mulmod a b q) (mulmod b b q) (p lsr 1)
    else a
  in
  loop 1 a p
;;
