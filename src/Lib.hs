module Lib where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString
import qualified Data.Word
import           Unsafe.Coerce      (unsafeCoerce)

data Transaction = Transaction { from   :: String
                               , to     :: String
                               , amount :: Float
                               , pow    :: String -- Proof of work
                               } deriving (Show)

data Block = Block { index :: Int  -- Index of the block
                   , txs   :: [Transaction] -- List of transactions in the block
                   , hash  :: Data.ByteString.ByteString -- Hash of the block
                   } deriving (Show)

-- Str8 to word8
strToWord8 :: String -> [Data.Word.Word8]
strToWord8 = map unsafeCoerce

-- SHA256 our Block
-- Blockhash is dependent on current block
sha256Block :: Block -> Data.ByteString.ByteString
sha256Block (Block blockIndex blockTxs blockHash) = digest
    where blockTxStr = foldr ((++) . show) "" blockTxs
          blockString = show blockIndex ++ blockTxStr ++ show blockHash
          blockByteStr = (Data.ByteString.pack . strToWord8) blockString
          ctx0 = SHA256.init
          ctx = foldl SHA256.update ctx0 [blockByteStr]
          digest = SHA256.finalize ctx


-- Our genesis block
genesisBlock :: Block
genesisBlock = Block blockIndex blockTxs blockHash
    where blockIndex = 0
          blockTxs = [Transaction "heaven" "kendrick" 15 "000000000000000000000000000000000"]
          blockHash = (Data.ByteString.pack . strToWord8) "000000000000000000000000000000001"
