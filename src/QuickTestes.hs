module QuickTestes where

import Data.List (sort, (\\))
import Test.QuickCheck


qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:ys) = x <= y && ordered (y:ys)

prop_permutation xs = permutation xs (qsort xs)
    where permutation as bs = null (as \\ bs) && null (bs \\ as)

prop_maximum xs =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

prop_sort_model xs = sort xs == qsort xs
