module WeekFirst
      (
      validateCard
      )
where
  
import Data.Char

validateCard :: Int -> Bool
validateCard = validate . sumDigits . doubleEveryOther . toDigitsRev

toDigits :: Int -> [Int]
toDigits x 
      | x < 1 = []
      | otherwise = map digitToInt $ show x

toDigitsRev :: Int -> [Int]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther x = reverse $ (\x -> if (mod (fst x) 2) == 0 then (snd x) * 2 else snd x) <$> zip [1..] x

sumDigits :: [Int] -> Int
sumDigits = foldr (+) 0 . concat . map toDigits

validate :: Int -> Bool
validate  = \x -> mod x 10 == 0