{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import System.IO.Unsafe
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as BS
import System.Environment

contents = (T.decodeUtf8With lenientDecode) . unsafePerformIO $ BS.readFile "src/blogpost.html"

main = do
  port <- getEnv "PORT"
  scotty (fromIntegral $ read port) $ do
    get "/" $ do
      html contents

