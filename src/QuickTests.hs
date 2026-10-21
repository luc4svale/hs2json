module QuickTests where

import Data.List (sort, (\\))
import Data.Maybe (listToMaybe)
import Test.QuickCheck
  ( Arbitrary(..)
  , Gen
  , Property
  , choose
  , (==>)
  )

data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq, Show)

instance Arbitrary Ternary where
    arbitrary = do
        n <- choose (0, 2) :: Gen Int
        return $ case n of
                      0 -> Yes
                      1 -> No
                      _ -> Unknown

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum' :: Ord a => [a] -> Property
prop_minimum' xs =
    not (null xs) ==> listToMaybe (qsort xs) == Just (minimum xs)

prop_ordered :: Ord a => [a] -> Bool
prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [_x]     = True
          ordered (x:y:ys) = x <= y && ordered (y:ys)

prop_permutation :: Ord a => [a] -> Bool
prop_permutation xs = permutation xs (qsort xs)
    where permutation as bs = null (as \\ bs) && null (bs \\ as)

prop_maximum :: Ord a => [a] -> Property
prop_maximum xs =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append :: Ord a => [a] -> [a] -> Property
prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        listToMaybe (qsort (xs ++ ys)) == Just (min (minimum xs) (minimum ys))

prop_sort_model :: Ord a => [a] -> Bool
prop_sort_model xs = sort xs == qsort xs