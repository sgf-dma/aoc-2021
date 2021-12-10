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

main :: IO ()
main = do
  c <- readFile "day1/input.txt"
  let xs = map read (lines c)
  print $ "Answer1: " ++ show (f1 xs)
  print $ "Answer2: " ++ show (f2 xs)
