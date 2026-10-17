module Main (main) where

import SimpleJSON (JValue(..))
import PrettyJSON (renderJValue)
import Prettify (compact, pretty)

example :: JValue
example = JObject
    [ ("projeto", JString "hs2json")
    , ("dupla",   JArray [JString "Lucas", JString "Izadora"])
    , ("escape",  JString "acentuação, \"aspas\" e\ttab")
    , ("versao",  JNumber 0.1)
    , ("testado", JBool True)
    , ("pendente", JNull)
    ]

main :: IO ()
main = do 
    putStrLn "== compact =="
    putStrLn (compact doc)
    putStrLn ""
    putStrLn "== pretty 40 =="
    putStrLn (pretty 40 doc)
  where 
    doc = renderJValue example
