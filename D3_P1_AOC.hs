import Data.List
input = 347991




data Snail a b c = EmptySnail | Snail a b (Snail a b c) deriving (Show)

initial = Snail 1 (0,0) EmptySnail


findX :: Int -> [Int] -> [Int] -> Int
findX 1 _ _ = 0
findX x direction@(y:ys) acc@(z:zs)
  | y == 0  = if z == 0 then (findX (x-1) ys zs) + 1 else (findX (x-1) direction zs) + 1
  | y == 2  = if z == 0 then (findX (x-1) ys zs) - 1 else (findX (x-1) direction zs) - 1
  | otherwise = if z == 0 then findX (x-1) ys zs else findX (x-1) direction zs


findY :: Int -> [Int] -> [Int] -> Int
findY 1 _ _ = 0
findY x direction@(y:ys) acc@(z:zs)
  | y == 1  = if z == 0 then (findY (x-1) ys zs) + 1 else (findY (x-1) direction zs) + 1
  | y == 3  = if z == 0 then (findY (x-1) ys zs) - 1 else (findY (x-1) direction zs) - 1
  | otherwise = if z == 0 then findY (x-1) ys zs else findY (x-1) direction zs

-- (0 + head . scanl (+) 1 (map (`mod` 2) [0,1..]),0)

-- scanl (+) 1 (map (`mod` 2) [0,1..])
-- (map (`mod` 4) [0,1..])

-- intercalate [0] $ concat $ map (replicate 2) $ take 4 $ interate (1:) [1]


makeSnail :: Int -> Snail Int (Int,Int) c
makeSnail 1 = initial
makeSnail x =
  let direction = take (x-1) $ (map (`mod` 4) [0,1..])
      -- acc = reverse . take (x-1) $ (scanl (+) 1 (map (`mod` 2) [0,1..]))
      stepper = take (x-1) $ 0:0:(intercalate [0] $ concat $ map (replicate 2) $ take x $ iterate (1:) [1])
  in  Snail x (findX x direction stepper, findY x direction stepper) (makeSnail (x-1))

readSnail :: Snail a (Int, Int) c -> Int
readSnail (Snail a (x, y) c) = abs x + abs y
