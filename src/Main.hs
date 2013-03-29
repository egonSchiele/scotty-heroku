{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import System.IO.Unsafe
import qualified Data.Text.Lazy as T

contents = T.pack . unsafePerformIO $ readFile "blogpost.html"

main = scotty 8234 $ do
  get "/" $ do
    html contents

