
import Test.QuickCheck
import Prettify
import Control.Applicative (liftA2)
import Control.Monad (liftM)
import Prelude hiding ((<>))

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftA2 Concat arbitrary arbitrary
              , liftA2 Union  arbitrary arbitrary ]

prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

main :: IO ()
main = do
    docs <- generate arbitrary :: IO [Doc]
    print docs