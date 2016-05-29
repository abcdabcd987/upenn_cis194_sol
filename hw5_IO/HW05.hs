{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Bits (xor)
import Data.List
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret original modified = do
  o <- BS.unpack <$> BS.readFile original
  m <- BS.unpack <$> BS.readFile modified
  return $ BS.pack $ filter (/= 0) $ zipWith xor o m

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  ciphertext <- BS.readFile (path ++ ".enc")
  let plaintext = zipWith xor (BS.unpack ciphertext) $ cycle (BS.unpack key)
  BS.writeFile path $ BS.pack plaintext

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = decode <$> BS.readFile path

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs pathVictimList pathTransaction = do
  v <- parseFile pathVictimList  :: IO (Maybe [String])
  t <- parseFile pathTransaction
  return $ case (v, t) of
    (Just vs, Just ts) -> Just $ filter (\Transaction{tid=tid} -> tid `elem` vs) ts
    (_, _)             -> Nothing

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow (t:ts) = let m1 = getFlow ts
                     totFrom = Map.findWithDefault 0 (from t) m1
                     m2 = Map.insert (from t) (totFrom - (amount t)) m1
                     totTo = Map.findWithDefault 0 (to t) m2
                 in Map.insert (to t) (totTo + (amount t)) m2

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ maximumBy (comparing snd) $ Map.assocs m

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ids =
  let payee = sortBy (compare `on` snd) [x | x@(_, amount) <- Map.assocs m, amount < 0]
      payer = sortBy (compare `on` snd) [x | x@(_, amount) <- Map.assocs m, amount > 0]
      pay (i:is) ((fn, fa):fs) ((tn, ta):ts) =
        Transaction { from = fn, to = tn, amount = tot, tid = i } : (pay is fs' ts')
          where tot = min fa ta
                fs' = if fa > ta then (fn, fa-ta):fs else fs
                ts' = if ta > fa then (tn, ta-fa):ts else ts
      pay _ _ _ = []
  in pay ids payer payee

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path xs = BS.writeFile path $ encode $ toJSON xs

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "clues/dog-original.jpg"
                        "clues/dog.jpg"
                        "clues/transactions.json"
                        "clues/victims.json"
                        "clues/new-ids.json"
                        "clues/new-transactions.json"
  putStrLn crim

