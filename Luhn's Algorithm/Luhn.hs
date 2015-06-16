cardNumber :: Integer
cardNumber = 5594589764218858


isValidLuhn :: Integer -> Bool
isValidLuhn n
  | (luhnSum
    (luhnExpand
    (luhnDouble
    (rawToRevList cardNumber)))) `mod` 10 == 0  = True
  | otherwise                                   = False


luhnSum :: [Integer] -> Integer
luhnSum n = sum n

luhnExpand :: [Integer] -> [Integer]
luhnExpand [] = []
luhnExpand n
  | head n >= 10            =  (div (head n) 10) 
                              :(mod (head n) 10)
                              :luhnExpand (tail n)
  | otherwise               = head n: luhnExpand (tail n)

luhnDouble :: [Integer] -> [Integer]
luhnDouble [] = []
luhnDouble n 
  | mod (length n) 2 == 1   = (head n) * 2 : luhnDouble (tail n)
  | otherwise               = head n : luhnDouble (tail n)
  

rawToRevList :: Integer -> [Integer]
rawToRevList 0 = []
rawToRevList n = lastDigit n : rawToRevList (dropLastDigit n)

lastDigit :: Integer -> Integer
lastDigit n = mod n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10
