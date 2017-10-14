{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Main where

import qualified Control.Monad.State as S
import           Data.Aeson          hiding (json)
import qualified Data.ByteString
import           Data.IORef
import           Data.Monoid         ((<>))
import           Data.String         (fromString)
import           Data.Text           (Text, pack)
import           GHC.Generics
import           Lib
import           Web.Spock.Config
import           Web.Spock.Core

newtype RunStateT s m = RunStateT { runStateT :: forall a. S.StateT s m a -> m a }

type Blockchain = [Block]


-- Adds a transaction
--
addTx :: (Monad m) => Transaction -> S.StateT Blockchain m Blockchain
addTx tx = do
    bc <- S.get
    let bh = Prelude.head bc -- Block head
    let bt = Prelude.tail bc -- Block tail
    let bh' = blockAddTx bh tx -- Add tx to the head of block
    let bc' = ([bh'] ++ bt) -- New blockchain
    S.put bc'
    return [bh']


-- Mines the block head
--
mine :: (Monad m) => S.StateT Blockchain m Blockchain
mine = do
    bc <- S.get -- Blockchain
    let bh = Prelude.head bc -- Block head
    let bt = Prelude.tail bc -- Block tail
    let mb = mineBlock bh 0 -- Mined block
    let bh' = Block (index mb) [] "" (hash mb) Nothing -- New block head
    let bc' = ([bh', mb] ++ bt)
    S.put bc'
    return [mb]


-- Returns current block head
--
current :: (Monad m) => S.StateT Blockchain m Blockchain
current = do
    bc <- S.get
    return $ [Prelude.head bc]


-- Gets current blockhash
--
getBlockhash :: (Monad m) => String -> S.StateT Blockchain m Blockchain
getBlockhash h = do
    bc <- S.get
    return $ filter (\b -> (hash b) == h) bc


-- Gets entire blockchasin
--
getBlockchain :: (Monad m) => S.StateT Blockchain m Blockchain
getBlockchain = S.get >>= return


-- StateT Monad to wrap the blockchainaround
-- a state (making it 'mutable')
-- https://stackoverflow.com/questions/31952792/how-do-i-use-a-persistent-state-monad-with-spock
restartableStateT :: s -> IO (RunStateT s IO)
restartableStateT s0 = do
    r <- newIORef s0
    return $ RunStateT $ \act -> do
        s <- readIORef r
        (x, s') <- S.runStateT act s
        atomicModifyIORef' r $ const (s', x)


main :: IO ()
main = do
    runner <- restartableStateT [genesisBlock]
    runSpock 8080 $ spockT (runStateT runner) $ do
        get "/" $ (S.lift getBlockchain) >>= json
        get "current" $ (S.lift current) >>= json        
        get "block" $ do
            blockid <- param "blockid"
            case blockid of
              (Just h) -> (S.lift $ getBlockhash h) >>= json
              _        -> text "please supply blockid"
        post "mine" $ (S.lift mine) >>= json
        post "addtx" $ do
            from <- param "from"
            to <- param "to"
            amount <- param "amount"
            case (from, to, amount) of
              (Just f, Just t, Just a) -> (S.lift $ addTx (Transaction f t a)) >>= json
              _                        -> text "missing/wrong params fam"
