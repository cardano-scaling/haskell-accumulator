module Bindings (
    getProofOverG1,
    getProofOverG2,
) where

import Accumulator (Accumulator, Element, elementExists, removeElement)
import Bindings.Internal (getPolyCommitmentG1, getPolyCommitmentG2)
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Fr, Point1, Point2, Scalar, blsGenerator, blsMult, frFromScalar, scalarFromInteger)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import PlutusCore.Bitwise (byteStringToInteger)

{- | A general function to process elements from an accumulator
| It checks if each element in the list exists in the accumulator and removes it if it does
-}
processElements :: [Element] -> Accumulator -> IO (Either String Accumulator)
processElements [] acc = return $ Right acc -- If all elements are processed, return the modified accumulator
processElements (x : xs) acc
    | elementExists x acc =
        -- Remove element from the accumulator and continue processing
        let updatedAcc = removeElement x acc
         in processElements xs updatedAcc
    | otherwise = return $ Left $ "Element not found in accumulator: " ++ show x -- Error if element not found

{- | Convert a hash (ByteString) into a Fr (field element)
| note: this function is not safe and does not check for errors
| e.g. if the hash is not a valid Fr element (blake2b-224 ensures this)
-}
hashToFr :: B.ByteString -> IO Fr
hashToFr hash = do
    scalar <- scalarFromInteger $ byteStringToInteger True hash
    frFromScalar scalar

-- | A generalized function to collect Fr elements from an accumulator using foldrWithKey
collectFrs :: Map.Map Element (B.ByteString, Int) -> IO [Fr]
collectFrs = Map.foldrWithKey collectFr (return [])
  where
    -- `collectFr` processes each element, hashes it to Fr, replicates it by the count, and accumulates lazily
    collectFr :: Element -> (B.ByteString, Int) -> IO [Fr] -> IO [Fr]
    collectFr _ (hashValue, count) accIO = do
        elementFr <- hashToFr hashValue
        rest <- accIO
        return $ replicate count elementFr ++ rest

-- | Generalized function to get a proof (works for both G1 and G2)
getProof ::
    ([Fr] -> [crs] -> IO (Either String crs)) -> -- The function to get the commitment
    [Element] -> -- List of elements
    Accumulator -> -- Accumulator
    [crs] -> -- Common reference string (CRS)
    IO (Either String crs)
getProof getCommitment memb acc crs = do
    -- Process all elements in `memb` through the accumulator
    remainingAccumulator <- processElements memb acc
    case remainingAccumulator of
        Left err -> return $ Left err
        Right remainingAcc -> do
            -- Build a list of Fr elements from the remaining accumulator
            frList <- collectFrs remainingAcc
            -- Get the polynomial commitment using the list of Fr elements and the CRS
            getCommitment frList crs

-- | Given a list of elements and an accumulator + CRS, calculate the polynomial commitment of the proof of membership function for G1 proof
getProofOverG1 :: [Element] -> Accumulator -> [Point1] -> IO (Either String Point1)
getProofOverG1 = getProof getPolyCommitmentG1

-- | Specialized function for G2 proof
getProofOverG2 :: [Element] -> Accumulator -> [Point2] -> IO (Either String Point2)
getProofOverG2 = getProof getPolyCommitmentG2
