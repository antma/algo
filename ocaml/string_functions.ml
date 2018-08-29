let compute_prefix_function s =
  let n = String.length s in
  let p = Array.make n 0 in
  let rec loop k q =
    if q < n then begin
      let sq = s.[q] in
      let rec loop2 k =
        if k > 0 && s.[k] <> sq then loop2 p.(k-1) else k
      in
      let j = loop2 k in
      let i = if s.[j] == sq then j + 1 else j in
      p.(q) <- i;
      loop i (q + 1)
    end
  in
  loop 0 1;
  p
;;

let _ = begin
  let p = (compute_prefix_function "ababababca") in
  Array.iter (fun i -> Printf.eprintf "%d\n" i) p;
  assert ((compute_prefix_function "ababababca") = [|0; 0; 1; 2; 3; 4; 5; 6; 0; 1|])
end
