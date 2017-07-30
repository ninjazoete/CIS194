module WeekFirst
      (
      validateCard
      )
where
  
import Data.Char

-- Card Validation

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
validate  = (== 0) . (`mod` 10)

-- Hanoi Tower

type Peg = String
type Move = (Peg, Peg)
hanoiThree :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiThree disks source target temp
      | disks <= 1 = [(source, target)]
      | otherwise = hanoiThree (disks-1) source temp target
                 ++ hanoiThree 1 source target temp
                 ++ hanoiThree (disks -1) temp target source
