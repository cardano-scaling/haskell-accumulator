module Bindings (
    getPolyCommitOverG1,
    getPolyCommitOverG2,
) where

import Accumulator (Accumulator, Element, elementExists, removeElement)
import Bindings.Internal (getPolyCommitmentG1, getPolyCommitmentG2)
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Fr, Point1, Point2, Scalar, blsGenerator, blsMult, frFromScalar, scalarFromInteger)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import PlutusCore.Bitwise (byteStringToInteger)
import System.IO.Unsafe (unsafePerformIO)

{- | A general function to process elements from an accumulator
| It checks if each element in the list exists in the accumulator and removes it if it does
-}
processElements :: [Element] -> Accumulator -> Either String Accumulator
processElements [] acc = Right acc -- If all elements are processed, return the modified accumulator
processElements (x : xs) acc
    | elementExists x acc =
        -- Remove element from the accumulator and continue processing
        let updatedAcc = removeElement x acc
         in processElements xs updatedAcc
    | otherwise = Left $ "Element not found in accumulator: " ++ show x -- Error if element not found

{- | Convert a hash (ByteString) into a Fr (field element)
| note: this function is not safe and does not check for errors
| e.g. if the hash is not a valid Fr element (blake2b-224 ensures this)
-}
hashToFr :: B.ByteString -> Fr
hashToFr hash = unsafePerformIO $ do
    scalar <- scalarFromInteger $ byteStringToInteger True hash
    frFromScalar scalar

-- | A generalized function to collect Fr elements from an accumulator using foldrWithKey
collectFrs :: Map.Map Element (B.ByteString, Int) -> [Fr]
collectFrs = Map.foldrWithKey collectFr []
  where
    -- `collectFr` processes each element, hashes it to Fr, replicates it by the count, and accumulates lazily
    collectFr :: Element -> (B.ByteString, Int) -> [Fr] -> [Fr]
    collectFr _ (hashValue, count) rest =
        let elementFr = hashToFr hashValue
         in replicate count elementFr ++ rest

-- | Generalized function to get a proof (works for both G1 and G2)
getCommit ::
    ([Fr] -> [crs] -> Either String crs) -> -- The function to get the commitment
    [Element] -> -- List of elements
    Accumulator -> -- Accumulator
    [crs] -> -- Common reference string (CRS)
    Either String crs
getCommit getCommitment memb acc crs =
    -- Process all elements in `memb` through the accumulator
    case processElements memb acc of
        Left err -> Left err
        Right remainingAcc ->
            -- Build a list of Fr elements from the remaining accumulator
            let frList = collectFrs remainingAcc
             in -- Get the polynomial commitment using the list of Fr elements and the CRS
                getCommitment frList crs

-- | Given a list of elements and an accumulator + CRS, calculate the polynomial commitment of the proof of membership function for G1 proof
getPolyCommitOverG1 :: [Element] -> Accumulator -> [Point1] -> Either String Point1
getPolyCommitOverG1 = getCommit getPolyCommitmentG1

-- | Specialized function for G2 proof
getPolyCommitOverG2 :: [Element] -> Accumulator -> [Point2] -> Either String Point2
getPolyCommitOverG2 = getCommit getPolyCommitmentG2
