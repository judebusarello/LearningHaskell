type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start temp end 
  hanoi 0 _ _ _ = []
  hanoi 1 start _ end = [(start, end)]
  hanoi n start temp end =
    hanoi (n - 1) start end temp
    hanoi n start temp end
    hanoi (n - 1) temp start end


