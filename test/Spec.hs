{-# LANGUAGE TemplateHaskell #-}
import Prelude hiding ((<>))

import Test.QuickCheck
import Data.List (intersperse)
import Control.Monad(liftM, liftM2)
import System.Exit(exitFailure)
import Data.Char (isSpace)
import Prettify


instance Arbitrary Doc where
    arbitrary = sized genDoc
      where
        genDoc 0 =
            oneof
                [ return Empty
                , liftM Char arbitrary
                , liftM Text arbitrary
                , return Line
                ]

        genDoc n =
            frequency
                [ (4, genDoc 0)
                , (3, liftM2 Concat subDoc subDoc)
                , (2, liftM group subDoc)
                ]
          where
            subDoc = genDoc (n `div` 2)

    shrink doc =
        [Empty | doc /= Empty] ++ genericShrink doc


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

prop_compact_text s = compact (text s) == s

prop_pretty_text :: Int -> String -> Bool
prop_pretty_text w s =
    pretty w (text s) == s

prop_pretty_lines_fit w d =
    w >= 0 ==>
    all lineFits (lines (pretty w d))
  where
    lineFits line
      | length line <= w = True
      | otherwise =
          let indent = length (takeWhile (== ' ') line)
              piece  = takeWhile (/= ' ') (drop indent line)
          in length piece > w - indent

equivalent :: String -> String -> Bool
equivalent s1 s2 =
    filter (not . isSpace) s1 == filter (not . isSpace) s2

prop_compact_pretty :: Positive Int -> Doc -> Property
prop_compact_pretty (Positive w) doc =
    let c = compact doc
        p = pretty w doc
    in counterexample
         (unlines
            [ "Largura: " ++ show w
            , "Documento: " ++ show doc
            , "compact: " ++ show c
            , "pretty:  " ++ show p
            ])
         (equivalent c p)

return []
runTests = $quickCheckAll

main :: IO ()
main = do
    passed <- runTests
    if passed
        then putStrLn "Passou em todos os testes."
        else do putStrLn "Alguns testes falharam."
                exitFailure