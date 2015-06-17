type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n start temp end =
     hanoi (n - 1) start end temp
     ++ [(start,end)]
     ++ hanoi (n - 1) temp start end
