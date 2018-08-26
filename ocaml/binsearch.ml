let lowerbound a x =
  let rec f l r =
    if r - l > 1 then begin
      let m = (l + r) lsr 1 in
      if a.(m) < x then f m r else f l m
    end else l
  in
  let k = f 0 (Array.length a) in if a.(k) < x then k+1 else k
;;

let _ = begin
  let a = [| 1; 2; 4; 5; 10|] in
  assert ((lowerbound a 0) = 0);
  assert ((lowerbound a 1) = 0);
  assert ((lowerbound a 2) = 1);
  assert ((lowerbound a 3) = 2);
  assert ((lowerbound a 4) = 2);
  assert ((lowerbound a 5) = 3);
  assert ((lowerbound a 6) = 4);
  assert ((lowerbound a 7) = 4);
  assert ((lowerbound a 10) = 4);
  assert ((lowerbound a 12) = 5);
end
