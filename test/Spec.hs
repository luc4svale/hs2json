{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Prelude hiding ((<>))

import Test.QuickCheck
import Data.List (intersperse)
import Control.Monad(liftM, liftM2)
import System.Exit(exitFailure)
import Data.Char (isSpace)
import SimpleJSON
import Prettify


instance Arbitrary Doc where
    arbitrary = sized genDoc
      where
        genDoc 0 =
            oneof
                [ return Empty
                , liftM Char (arbitrary `suchThat` (/= '\n'))
                , liftM Text (arbitrary `suchThat` notElem '\n')
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
        filter wellFormed ([Empty | doc /= Empty] ++ genericShrink doc)

wellFormed :: Doc -> Bool
wellFormed (Char c)       = c /= '\n'
wellFormed (Text s)       = '\n' `notElem` s
wellFormed (Concat a b)   = wellFormed a && wellFormed b
wellFormed u@(Union _ r)  = group r == u && wellFormed r
wellFormed (Nest _ x)     = wellFormed x
wellFormed _              = True

instance Arbitrary JValue where
    arbitrary = sized genJValue

genJValue :: Int -> Gen JValue
genJValue 0 = scalar
genJValue n = oneof [scalar, recursive]
  where
    smaller   = genJValue (n `div` 2)
    field     = (,) <$> arbitrary <*> smaller
    recursive = oneof
      [ JObject <$> resize (n `div` 2) (listOf field)
      , JArray  <$> resize (n `div` 2) (listOf smaller)
      ]

scalar :: Gen JValue
scalar = oneof
  [ JString <$> arbitrary
  , JNumber <$> arbitrary
  , JBool   <$> arbitrary
  , return JNull
  ]


prop_empty_id :: Doc -> Bool
prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

prop_char :: Char -> Bool
prop_char c   = char c   == Char c

prop_text :: String -> Bool
prop_text s   = text s   == if null s then Empty else Text s

prop_line :: Bool
prop_line     = line     == Line

prop_double :: Double -> Bool
prop_double d = double d == text (show d)

prop_hcat :: [Doc] -> Bool
prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

prop_punctuate' :: Doc -> [Doc] -> Bool
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]
        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys

prop_compact_text :: String -> Bool
prop_compact_text s = compact (text s) == s

prop_pretty_text :: Int -> String -> Bool
prop_pretty_text w s =
    pretty w (text s) == s

maxLine :: String -> Int
maxLine = maximum . (0 :) . map length . lines

prop_pretty_lines_fit :: Int -> Doc -> Bool

prop_pretty_lines_fit w d =
    maxLine (pretty w d) <= max w (maxLine (pretty (-1) d))

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

prop_getString :: String -> Bool
prop_getString s = getString (JString s) == Just s

prop_getInt :: Double -> Bool
prop_getInt d = getInt (JNumber d) == Just (truncate d)

prop_getDouble :: Double -> Bool
prop_getDouble d = getDouble (JNumber d) == Just d

prop_getBool :: Bool -> Bool
prop_getBool b = getBool (JBool b) == Just b

prop_getObject :: [(String, JValue)] -> Bool
prop_getObject o = getObject (JObject o) == Just o

prop_getArray :: [JValue] -> Bool
prop_getArray a = getArray (JArray a) == Just a

prop_getString_other :: JValue -> Bool
prop_getString_other v = case v of
    JString _ -> True
    _         -> getString v == Nothing

prop_getNumber_other :: JValue -> Bool
prop_getNumber_other v = case v of
    JNumber _ -> True
    _         -> getInt v == Nothing && getDouble v == Nothing

prop_getBool_other :: JValue -> Bool
prop_getBool_other v = case v of
    JBool _ -> True
    _       -> getBool v == Nothing

prop_getObject_other :: JValue -> Bool
prop_getObject_other v = case v of
    JObject _ -> True
    _         -> getObject v == Nothing

prop_getArray_other :: JValue -> Bool
prop_getArray_other v = case v of
    JArray _ -> True
    _        -> getArray v == Nothing

prop_isNull :: JValue -> Bool
prop_isNull v = isNull v == case v of
    JNull -> True
    _     -> False

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    passed <- runTests
    if passed
        then putStrLn "Passou em todos os testes."
        else do putStrLn "Alguns testes falharam."
                exitFailure