module Main where

import Lib
import qualified Data.ByteString


main :: IO ()
main = do
    print genesisBlock
    print $ sha256Block genesisBlock
