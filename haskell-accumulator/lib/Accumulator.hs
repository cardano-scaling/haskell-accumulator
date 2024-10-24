{-# LANGUAGE TypeApplications #-}

module Accumulator where

import Cardano.Crypto.Hash.Blake2b (Blake2b_224)
import Cardano.Crypto.Hash.Class (digest)
import qualified Data.ByteString as BS
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))

type Element = BS.ByteString
type Count = Int
type Blake2bHash = BS.ByteString

-- | Use a Map for fast lookups and precompute the hash of each element
type Accumulator = Map.Map Element (Blake2bHash, Count)

emptyAccumulator :: Accumulator
emptyAccumulator = Map.empty

-- | Function to add an element with its Blake2b hash and count
addElement :: Accumulator -> Element -> Accumulator
addElement acc element =
    let hashValue = digest (Proxy @Blake2b_224) element
     in Map.insertWith
            (\(_, oldCount) (_, newCount) -> (hashValue, oldCount + newCount))
            element
            (hashValue, 1)
            acc

-- | Function to remove an element from the accumulator
removeElement :: Element -> Accumulator -> Accumulator
removeElement = Map.update adjustCount
  where
    adjustCount (h, count)
        | count > 1 = Just (h, count - 1)
        | otherwise = Nothing

-- | Function to check if an element is in the accumulator
elementExists :: Element -> Accumulator -> Bool
elementExists = Map.member

buildAccumulator :: [Element] -> Accumulator
buildAccumulator = foldl' addElement emptyAccumulator
