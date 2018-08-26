class debruijn32 =
  object
  val db = Int32.of_string "0x4653ADF"
  val t = Array.make 32 0
  initializer begin
    for k = 0 to 31 do
      t.(Int32.to_int (Int32.shift_right_logical (Int32.shift_left db k) 27)) <- k
    done
  end
  method lookup x = t.(Int32.to_int (Int32.shift_right_logical (Int32.mul db (Int32.of_int x)) 27))
end

let _ = begin
  let db = new debruijn32 in
  for i = 0 to 30 do
    assert (db#lookup (1 lsl i) = i)
  done
end
