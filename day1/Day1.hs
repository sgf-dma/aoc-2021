
module Day1
    ( runF1
    , runF2
    )
  where

f1 :: [Int] -> Int
f1 [] = 0
-- y x(cur)
f1 (k : ks) = foldr (\x g y -> let n = g x in if y < x then n + 1 else n) (const 0) ks k

f2 :: [Int] -> Int
f2 [] = 0
f2 (k1 : k2 : ks) = fst $ foldr go (\_ _ -> (0, 0)) ks k1 k2
 where
  go ::
    Int ->
    (Int -> Int -> (Int, Int)) ->
    (Int -> Int -> (Int, Int))
  -- w y x(cur)
  go x g w y =
    let (n, next) = g y x
        cur = w + y + x
     in if next > cur then (n + 1, cur) else (n, cur)

readInput :: FilePath -> IO [Int]
readInput = fmap (map read . lines) . readFile

runF1 :: FilePath -> IO ()
runF1 fp = do
    xs <- readInput fp
    print $ "Answer1: " ++ show (f1 xs)

runF2 :: FilePath -> IO ()
runF2 fp = do
    xs <- readInput fp
    print $ "Answer2: " ++ show (f2 xs)

