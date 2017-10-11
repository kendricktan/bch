module Lib where

import qualified Crypto.Hash.SHA256     as SHA256
import           Data.ByteString        (ByteString, append, pack)
import           Data.ByteString.Base16 (decode, encode)
import           Data.ByteString.UTF8   (fromString)
import qualified Data.Word
import           Unsafe.Coerce          (unsafeCoerce)

data Transaction = Transaction { from   :: String
                               , to     :: String
                               , amount :: Float
                               } deriving (Show)

data Block = Block { index    :: Int  -- Index of the block
                   , txs      :: [Transaction] -- List of transactions in the block
                   , hash     :: Data.ByteString.ByteString -- Hash of the block
                   , prevHash :: Data.ByteString.ByteString -- Prev Hash of the block
                   , nonce    :: Maybe Int -- Nonce of the block (proof of work)
                   } deriving (Show)

-- SHA256 our Block
-- Blockhash is dependent on current block and prev block hash
hashBlock :: Block -> ByteString
hashBlock (Block blockIndex blockTxs blockHash prevHash _) = encode digest
    where blockTxStr = foldr ((++) . show) "" blockTxs
          blockString = blockTxStr ++ show blockHash
          blockByteStr = fromString blockString
          ctx = SHA256.updates SHA256.init [blockByteStr, prevHash]
          digest = SHA256.finalize ctx

-- Mine the block
-- sha256 of the hashed block starts with a 0
mineBlock :: Block -> Int -> Block
mineBlock b@(Block i t h p _) n = case head pow of
                                    '0' -> Block i t h p (Just n)
                                    _   -> mineBlock b (i + 1)
    where blockHash = hashBlock b
          nonce = fromString $ show n
          ctx = SHA256.updates SHA256.init [blockHash, nonce]
          pow = show . encode $ SHA256.finalize ctx


-- Our genesis block
--
genesisBlock :: Block
genesisBlock = Block blockIndex blockTxs blockHash prevHash Nothing
    where blockIndex = 0
          blockTxs = [Transaction "heaven" "kendrick" 15]
          blockHash = encode $ fromString "000000000000000000000000000000001"
          prevHash = encode $ fromString "000000000000000000000000000000000"
