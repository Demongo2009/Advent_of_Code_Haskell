import Data.List
input = 347991




data Snail a b c d = EmptySnail | Snail a b (Snail a b c d) d deriving (Show)

initial = Snail 1 (0,0) EmptySnail 1


findX :: Int -> [Int] -> [Int] -> Int
findX 1 _ _ = 0
findX x direction@(current_dir:direction_rest) acc@(change_dir:next_change)
  | current_dir == 0  = if change_dir == 0 then (findX (x-1) direction_rest next_change) + 1 else (findX (x-1) direction next_change) + 1
  | current_dir == 2  = if change_dir == 0 then (findX (x-1) direction_rest next_change) - 1 else (findX (x-1) direction next_change) - 1
  | otherwise = if change_dir == 0 then findX (x-1) direction_rest next_change else findX (x-1) direction next_change


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


makeSnail :: Int -> Snail Int (Int,Int) c Int
makeSnail 1 = initial
makeSnail x =
  let direction = take (x-1) $ (map (`mod` 4) [0,1..])
      -- acc = reverse . take (x-1) $ (scanl (+) 1 (map (`mod` 2) [0,1..]))
      stepper = take (x-1) $ 0:0:(intercalate [0] $ concat $ map (replicate 2) $ take x $ iterate (1:) [1])
  in  Snail x (findX x direction stepper, findY x direction stepper) (makeSnail (x-1)) 0

readSnailXY :: Snail a (Int, Int) c d -> (Int, Int)
readSnailXY (Snail _ (x, y) _ _) = (x, y)

readSnailValue :: Snail a b c Int -> Int
readSnailValue (Snail _ _ _ value) = value

readSnailScale :: Snail Int b c d -> Int
readSnailScale (Snail scale _ _ _) = scale

readSnailSnail :: Snail a b c d -> Snail a b c d
readSnailSnail EmptySnail = EmptySnail
readSnailSnail (Snail _ _ snail_tail d) = snail_tail

searchForSnailXY :: Int -> (Int, Int)
searchForSnailXY which_scale = readSnailXY $ makeSnail which_scale

searchForSnailScale :: (Int, Int) -> Int -> Snail Int (Int, Int) c Int
searchForSnailScale (0, 0) _ = initial
searchForSnailScale (x, y) acc
  | readSnailXY (makeSnail acc) == (x, y) = makeSnail acc
  | otherwise = searchForSnailScale (x, y) (acc+1)

searchForSnail :: (Int, Int) -> Int -> Snail Int (Int, Int) c Int -> Snail Int (Int, Int) c Int
searchForSnail (0, 0) _ snail = initial
searchForSnail (x, y) acc snail
  | readSnailXY (makeSnail acc) == (x, y) = dropSnail ((lengthSnail snail) - acc) snail
  | otherwise = searchForSnail (x, y) (acc+1) snail

lengthSnail :: Snail a b c d -> Int
lengthSnail EmptySnail = 0
lengthSnail (Snail _ _ snail_tail _) = (lengthSnail snail_tail)+ 1


-- computeSnail ::
-- computeSnail new_value snail
--   | value /= new_value


-- computeSnail ::
-- computeSnail 1 _ = initial
-- computeSnail which_scale snail=
--   let current_XY = searchForSnailXY which_scale
--       current_X = fst current_XY
--       current_Y = snd current_XY
--   in  Snail which_scale (computeSnail (searchForSnail (current_X-1, current_Y)) + computeSnail (searchForSnail (current_X-1, current_Y-1))
--   + computeSnail (searchForSnail (current_X, current_Y-1)) + computeSnail (searchForSnail (current_X+1, current_Y-1)) + computeSnail (searchForSnail (current_X+1, current_Y))
--   + computeSnail (searchForSnail (current_X+1, current_Y+1)) + computeSnail (searchForSnail (current_X, current_Y+1)) + computeSnail (searchForSnail (current_X-1, current_Y+1)))


dropSnail :: Int -> Snail Int (Int,Int) c Int  -> Snail Int (Int, Int) c Int
dropSnail 1 snail = readSnailSnail snail
dropSnail amount snail = dropSnail (amount - 1) (readSnailSnail snail)
-- takeSnail amount snail = [  | x <- (take ((lengthSnail snail) - amount ) [1,1..]) ]

takeSnail :: Int -> Snail Int (Int,Int) c Int  -> Snail Int (Int, Int) c Int
takeSnail 0 snail = EmptySnail
takeSnail amount snail = Snail (readSnailScale snail) (readSnailXY snail) (takeSnail (amount-1) (readSnailSnail snail)) (readSnailValue snail)

takeSnailWith :: Int -> Snail Int (Int,Int) c Int -> Snail Int (Int, Int) c Int -> Snail Int (Int, Int) c Int
takeSnailWith 0 snail end_snail = end_snail
takeSnailWith amount snail end_snail = Snail (readSnailScale snail) (readSnailXY snail) (takeSnailWith (amount-1) (readSnailSnail snail) end_snail) (readSnailValue snail)



upgradeSnail :: Snail Int (Int, Int) c Int -> Int -> Int -> Snail Int (Int, Int) c Int
upgradeSnail snail which_scale value =
  let length = ((lengthSnail snail) - which_scale)
  in takeSnailWith (length) snail (Snail which_scale (searchForSnailXY which_scale) (dropSnail (length+1) snail) value)

--
--
computeSnail :: Int -> Snail Int (Int, Int) c Int ->  Int -> Int
computeSnail 1 _ _ = 1
computeSnail wanted_value snail acc =
  let current_X = fst $ readSnailXY (makeSnail acc)
      current_Y = snd $ readSnailXY (makeSnail acc)
      value = (readSnailValue (searchForSnail (current_X-1, current_Y) 1 snail) + readSnailValue (searchForSnail (current_X-1, current_Y-1) 1 snail)
         + readSnailValue (searchForSnail (current_X, current_Y-1) 1 snail) + readSnailValue (searchForSnail (current_X+1, current_Y-1) 1 snail) + readSnailValue (searchForSnail (current_X+1, current_Y) 1 snail)
         + readSnailValue (searchForSnail (current_X+1, current_Y+1) 1 snail) + readSnailValue (searchForSnail (current_X, current_Y+1) 1 snail) + readSnailValue (searchForSnail (current_X-1, current_Y+1) 1 snail))

      new_snail = upgradeSnail snail acc value
  in if value > wanted_value then
    -- let current_X = fst $ readSnailXY (makeSnail (acc+1))
    --     current_Y = snd $ readSnailXY (makeSnail (acc+1))
    --     value = (readSnailValue (searchForSnail (current_X-1, current_Y) 1 new_snail) + readSnailValue (searchForSnail (current_X-1, current_Y-1) 1 new_snail)
    --        + readSnailValue (searchForSnail (current_X, current_Y-1) 1 new_snail) + readSnailValue (searchForSnail (current_X+1, current_Y-1) 1 new_snail) + readSnailValue (searchForSnail (current_X+1, current_Y) 1 new_snail)
    --        + readSnailValue (searchForSnail (current_X+1, current_Y+1) 1 new_snail) + readSnailValue (searchForSnail (current_X, current_Y+1) 1 new_snail) + readSnailValue (searchForSnail ((current_X-1), (current_Y+1)) 1 new_snail))
    -- in
     value

    else computeSnail wanted_value new_snail (acc+1)
