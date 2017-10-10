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

data Block = Block { index    :: Int  -- Index of the block
                   , txs      :: [Transaction] -- List of transactions in the block
                   , hash     :: Data.ByteString.ByteString -- Hash of the block
                   , prevHash :: Data.ByteString.ByteString -- Prev Hash of the block
                   } deriving (Show)

-- Str8 to word8
strToWord8 :: String -> [Data.Word.Word8]
strToWord8 = map unsafeCoerce

strToByteString :: String -> Data.ByteString.ByteString
strToByteString = Data.ByteString.pack . strToWord8

-- SHA256 our Block
-- Blockhash is dependent on current block and prev block hash
hashBlock :: Block -> Data.ByteString.ByteString
hashBlock (Block blockIndex blockTxs blockHash prevHash) = digest
    where blockTxStr = foldr ((++) . show) "" blockTxs
          blockString = show blockIndex ++ blockTxStr ++ show blockHash
          blockByteStr = strToByteString blockString
          ctx0 = SHA256.init
          ctx = foldl SHA256.update ctx0 [Data.ByteString.append blockByteStr prevHash]
          digest = SHA256.finalize ctx


-- Our genesis block
genesisBlock :: Block
genesisBlock = Block blockIndex blockTxs blockHash prevHash
    where blockIndex = 0
          blockTxs = [Transaction "heaven" "kendrick" 15 "000000000000000000000000000000000"]
          blockHash = strToByteString "000000000000000000000000000000001"
          prevHash = strToByteString "000000000000000000000000000000000"
