module Prettify 
    (
      Doc(..)
    , empty
    , char
    , text
    , double
    , line
    , (<>)
    , hcat
    , fsep
    , (</>)
    , group
    , softline
    , punctuate
    , compact
    , pretty
    , fill
    , nest
    ) where

import Prelude hiding ((<>))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         | Nest Int Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

nest :: Int -> Doc -> Doc
nest i x = Nest i x

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y     = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten (Nest i x)     = Nest i (flatten x)
flatten other          = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _p []     = []
punctuate _p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String 
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)
                Nest _ y     -> transform (y:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [(0,x)]
    where best col ((i,d):ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c : best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds      
                Line         -> '\n' : replicate i ' ' ++ best i ds
                Nest j y     -> best col ((i + j, y):ds)
                a `Concat` b -> best col ((i,a):(i,b):ds)
                a `Union` b  -> nicest col (best col ((i,a):ds))
                                           (best col ((i,b):ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
_w `fits` ""        = True
_w `fits` ('\n':_)  = True
w `fits` (_c:cs)    = (w - 1) `fits` cs

fill :: Int -> Doc -> Doc
fill width x = x <> text (replicate (width - columns) ' ')
    where columns = length (compact (flatten x))
