let longest_common_subsequence x y =
  let lx = String.length x in let ly = String.length y in
  let d = Array.make_matrix (lx+1) (ly+1) 0 in
  let p = Array.make_matrix (lx+1) (ly+1) 0 in
  for i = 0 to ly - 1 do p.(lx).(i) <- 2; done;
  for i = 0 to lx - 1 do p.(i).(ly) <- 1; done;
  p.(lx).(ly) <- -1;
  for i = lx - 1 downto 0 do
    for j = ly - 1 downto 0 do
      if x.[i] = y.[j] then d.(i).(j) <- 1 + d.(i+1).(j+1)
      else if d.(i+1).(j) > d.(i).(j+1) then begin
        d.(i).(j) <- d.(i+1).(j);
        p.(i).(j) <- 1;
      end else begin
        d.(i).(j) <- d.(i).(j+1);
        p.(i).(j) <- 2;
      end
    done
  done;
  (d, p)
;;
