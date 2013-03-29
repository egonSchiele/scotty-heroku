{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import System.IO.Unsafe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import System.Environment

contents = (E.decodeUtf8With E.lenientDecode) . unsafePerformIO $ BS.readFile "src/blogpost.html"

main = do
  port <- getEnv "PORT"
  scotty (fromIntegral $ read port) $ do
    get "/" $ do
      html contents

