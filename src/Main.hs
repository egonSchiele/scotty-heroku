{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import System.IO.Unsafe
import qualified Data.Text.Lazy as T
import System.Environment

contents = T.pack . unsafePerformIO $ readFile "blogpost.html"

main = do
  port <- getEnv "PORT"
  scotty (fromIntegral $ read port) $ do
    get "/" $ do
      html contents

