
f1 :: [Int] -> Int
f1 [] = 0
f1 (k : ks) = foldr (\x g y -> let n = g x in if y < x then n + 1 else n) (const 0) ks k

main :: IO ()
main = do
    c <- readFile "day1/input.txt"
    let xs = map read (lines c)
    print (f1 xs)
