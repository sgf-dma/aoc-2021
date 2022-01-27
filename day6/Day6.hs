
module Day6
    ( runF1
    )
  where

import Control.Comonad
import Control.Comonad.Traced
import Data.Function
import Data.List.Split

newtype Traced2 m a = Traced2 (m -> a)

instance Functor (Traced2 m) where
    fmap f (Traced2 g) = Traced2 (f <$> g)

instance Monoid m => Comonad (Traced2 m) where
    extract (Traced2 f) = f mempty
    extend q (Traced2 g) = Traced2 (\m -> q (Traced2 (\m' -> g (m <> m'))))
    duplicate (Traced2 g) = Traced2 (\m -> Traced2 (\m' -> g (m <> m')))

trace2 :: m -> Traced2 m a -> a
trace2 x (Traced2 f) = f x

traces2 :: Monoid m => (a -> m) -> Traced2 m a -> a
traces2 f w = trace2 (f (extract w)) w

oneDay :: [Int] -> [Int]
oneDay xs =
    let (ys, k) = foldr go ([], 0) xs
    in  ys ++ take k (repeat 8)
  where
    go :: Int -> ([Int], Int) -> ([Int], Int)
    go x (zs, n)
      | x == 0      = (6 : zs, n + 1)
      | otherwise   = (x - 1 : zs, n)

-- Primitive version not requiring comonad-s at all.
fish :: Traced [Int] [Int]
fish = traced oneDay

fishes :: [Int] -> Int -> [Int]
fishes xs 0 = xs
fishes xs n = fishes (trace xs fish) (n - 1)

-- First, note, that
--
--      extract $ w =>> trace x
--
-- assigns second monoid m' to x and first monoid m to [] (see 'extend'
-- definition above). But
--
--      trace x w
--
-- assigns /first/ monoid m to x. Understanding this distinction is the key
-- for following explanations.

-- Version with 'First' monoid. Here
--
--      trace x $ fishF =>>.....
--
-- is the same as just
--
--      trace x fishF
--
-- because 'First' Monoid will always choose leftmost monoid value, i.e. the
-- one supplied by 'trace'. Thus, it does not matter what follows 'fishF' in
-- the 'extend' chain, only first 'fishF' will be actually executed, when i
-- apply 'trace'. But 'traces' does exactly this: it extracts current result
-- and 'trace'-s it through the same comonad. This is effectively the same as
-- just 'trace'-ing current result through just 'fishF' ignoring the rest of
-- the comonad chain. But tracing current result through 'fishF' is just one
-- step. Then, next 'traces' 'extract'-s this result (which is now one step
-- further) and all repeats.

oneDayF :: First [Int] -> [Int]
oneDayF (First Nothing) = oneDay []
oneDayF (First (Just xs)) = oneDay xs

fishF :: Traced (First [Int]) [Int]
fishF = traced oneDayF

fishesF :: [Int] -> Int -> [Int]
fishesF ages n
  | n > 0       = extract (fix go n)
  | otherwise   = ages
  where
    go :: (Int -> Traced (First [Int]) [Int]) -> Int -> Traced (First [Int]) [Int]
    go rec x
      | x > 1     = rec (x - 1) =>> traces (First . Just)
      | otherwise = fishF =>> trace (First (Just ages))

-- Version with 'Last' monoid. Here
--
--      fishL =>> trace x =>> .....
--
-- is the same as
--
--      fishL =>> trace x
--
-- because extending 'trace' applies last argument and no matter how many
-- times...
--
-- Here
--
--      trace (Last . Just $ [3,4,3,1,2]) $ fishL =>> traces (Last . Just)
--
-- makes two steps, First step is made by 'fishL', when it is 'extract'-ed
-- inside 'traces' (remember, 'extract' applies mempty, so 'Last' does not
-- matter here). Second step is made, when 'traces' applies 'trace' to the
-- same comonad: 'trace' applied by 'traces' effectively provides /last/
-- argument to monoid sum (see 'extend' definition for 'Traced'), and, thus,
-- 'fishL' makes step from previous result.
--
-- So, (fishL =>> traces (Last . Just)) makes /two steps/ further.
--
-- When i add one more 'traces' to the end
--
--      trace (Last . Just $ [3,4,3,1,2]) $ fishL =>> traces (Last . Just) =>> traces (Last . Just)
--
-- the last 'traces' applies 'extract' to (fishL =>> traces (Last . Just)),
-- but i know from before, that this comonad does /two steps/. So,
-- 'extract'-ing from this comonad makes two steps further. Then 'traces'
-- applies 'trace' to the same comonad, which also makes /two steps/ furhter,
-- as i know from before. Thus, this chain actually will make /4 steps further/.
--
-- So, each 'traces' runs previous comonad twice. And, thus, applying one more
-- 'traces' (to the total of 3 'traces' in chain) multiplies /4 steps/ by 2
-- and i'll move 8 steps further. Etc.
--
-- So, version with 'Last' calculates fishes at the power of 2 day. The exact
-- power of 2 depends from condition (x > 1) in go function in 'fishesL'
-- below.

oneDayL :: Last [Int] -> [Int]
oneDayL (Last Nothing) = oneDay []
oneDayL (Last (Just xs)) = oneDay xs

fishL :: Traced (Last [Int]) [Int]
fishL = traced oneDayL

-- Calculates fishes at 2^(n - 1) day.
fishesL :: [Int] -> Int -> [Int]
fishesL ages n
  | n > 0       = trace (Last (Just ages)) (fix go n)
  | otherwise   = ages
  where
    go :: (Int -> Traced (Last [Int]) [Int]) -> Int -> Traced (Last [Int]) [Int]
    go rec x
      | x > 1     = rec (x - 1) =>> traces (Last . Just)
      | otherwise = fishL

readInput :: FilePath -> IO [Int]
readInput = fmap (map read . wordsBy (== ',')) . readFile

runF1 :: FilePath -> IO ()
runF1 fp = do
    ages <- readInput fp
    let ys = fishesF ages 80
    print $ "Answer1: " ++ show (length ys)

