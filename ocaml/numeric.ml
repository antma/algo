
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
