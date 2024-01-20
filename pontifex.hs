import Data.Char (chr, ord)
import Data.List (elemIndex, splitAt)
import Data.Maybe (fromJust)

data Card = Card Int | JokerA | JokerB deriving (Show, Eq)

type Deck = [Card]

-- should shuffle this at some point
initial = map Card [1 .. 52] ++ [JokerA] ++ [JokerB]

-- preserve the invariant that jokers don't end up first
rotate n xs = take (length xs) (drop n (cycle xs))

swapAt n xs = let (ys, a : b : zs) = splitAt n xs in ys ++ b : a : zs

wrapSwap :: Card -> Deck -> Deck
wrapSwap card deck
  | index == length deck - 1 = swapAt 1 . rotate 1 $ deck
  | otherwise = swapAt index deck
  where
    index = fromJust $ elemIndex card deck

tripleCut :: Deck -> Deck
tripleCut deck =
  let ind1 = fromJust $ elemIndex JokerA deck
      ind2 = fromJust $ elemIndex JokerB deck
      (topCut, rest) = splitAt (min ind1 ind2) deck
      (midCut, btmCut) = splitAt (max ind1 ind2 + 1) rest
   in btmCut ++ midCut ++ topCut

toInt (Card x) = x
toInt _ = 53

countCut :: Deck -> Deck
countCut deck =
  let (topCut, restCut) = splitAt (toInt . last $ deck) (init deck)
   in restCut ++ topCut ++ [last deck]

-- <3
keystreamDeck :: Deck -> Deck
keystreamDeck = countCut . tripleCut . wrapSwap JokerB . wrapSwap JokerB . wrapSwap JokerA

-- gotta skip them jokesters
key :: Deck -> Int
key deck
  | k `elem` [JokerA, JokerB] = key . keystreamDeck $ deck
  | otherwise = toInt k
  where
    k = deck !! toInt (head deck)

keyStream :: String -> [Int]
keyStream = map key . scanl (const . keystreamDeck) initial . tail

-- CHARACTER TO NUMBER CONVERSIONS

-- going from A=1, B=2, etc. to ASCII uppercase
shamt = ord 'A' - 1

add = (+) -- hehe

toNum = subtract shamt . ord

shiftMod x n = (n - 1) `mod` x + 1 -- handling negatives

toChar = chr . add shamt . shiftMod 26

-- ENCRYPTION (p straightforward)
encrypt :: String -> String
encrypt input = zipWith (\k i -> toChar (toNum i + k)) (keyStream input) input

decrypt :: [Int] -> String -> String
decrypt = zipWith (\k i -> toChar (toNum i - k))

main = do
  putStrLn $ encrypt "SHRUTIISCOOL"
  print $ keyStream "SHRUTIISCOOL"
  print $ decrypt [2, 49, 51, 14, 50, 24, 9, 15, 34, 14, 50, 14] "UEQIRGRHKCMZ"