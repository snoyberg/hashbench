{-# LANGUAGE OverloadedStrings #-}
import Gauge
import Lib
import qualified Data.Text as T

texts :: [T.Text]
texts =
  [ ""
  , "hello world"
  , T.pack ['A'..'Z']
  , T.pack $ replicate 1000 '\0'
  , T.pack $ replicate 10000 'א'
  ]

main :: IO ()
main =
    defaultMain $ map go texts
  where
    go text = bgroup (show $ T.length text)
      [ bench "C" $ whnf (call hash_via_c) text
      , bench "Haskell" $ whnf (call hash_via_haskell) text
      ]
