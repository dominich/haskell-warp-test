{-# LANGUAGE OverloadedStrings #-}

-- Simple fibonacci server.

module Main where

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

fib 1 = 1
fib 0 = 0
fib x = fib (x - 1) + fib (x - 2)

application req respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] $ case pathInfo req of
    [x] ->
      case (reads $ T.unpack x :: [(Int, String)]) of
        [] ->
          "invalid number"
        [(y, x)] ->
          BL.pack . show $ fib y
    _  ->
      "no number specified"

main = run 3000 application
