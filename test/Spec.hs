{-# LANGUAGE TemplateHaskell #-}
import Prelude hiding ((<>))

import Test.QuickCheck
import Data.List (intersperse)
import Control.Monad(liftM, liftM2)
import System.Exit(exitFailure)

import Prettify

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]

prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

prop_char c   = char c   == Char c
prop_text s   = text s   == if null s then Empty else Text s
prop_line     = line     == Line
prop_double d = double d == text (show d)

prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]
        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys


return []
runTests = $quickCheckAll

main :: IO ()
main = do
    passed <- runTests
    if passed
        then putStrLn "Passou em todos os testes."
        else do putStrLn "Alguns testes falharam."
                exitFailure