module Main where

import           Network.Wai.Handler.Warp
import           SampleServer

main :: IO ()
main = run 8081 app
