{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Accumulator (Accumulator, Element (..), addElement, emptyAccumulator)
import Bindings (getProofOverG1, getProofOverG2)
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (
    Point1 (..),
    Point2 (..),
    Scalar (..),
    blsCompress,
    blsGenerator,
    blsMult,
    frFromScalar,
    millerLoop,
    ptFinalVerify,
    scalarFromFr,
    scalarFromInteger,
    scalarToInteger,
 )
import Criterion.Main
import qualified Data.ByteString as B
import qualified Field as F
import GHC.IO (unsafePerformIO)
import PlutusTx.Numeric (
    AdditiveGroup (..),
    AdditiveMonoid (..),
    AdditiveSemigroup (..),
    Module (..),
    MultiplicativeMonoid (..),
    MultiplicativeSemigroup (..),
 )
import System.Random (randomRIO)

-- Helper function to generate a random ByteString of a given length
generateRandomByteString :: Int -> IO B.ByteString
generateRandomByteString n = B.pack <$> mapM (\_ -> randomRIO (0, 255)) [1 .. n]

-- Generate a random set of N ByteStrings
generateRandomSet :: Int -> IO [Element]
generateRandomSet n = mapM (\_ -> generateRandomByteString 32) [1 .. n]

-- Create test setup with a set of elements and a subset of that set
mkTestSetup :: Int -> Int -> IO (Accumulator, [Element], [Point1], [Point2])
mkTestSetup setSize subSetSize = do
    -- Generate a random set of ByteStrings
    set <- generateRandomSet setSize
    let setMap = foldl addElement emptyAccumulator set :: Accumulator

    -- Define a tau
    let tau = F.Scalar 10

    -- Define powers of tau
    let powerOfTauField = map (F.powModScalar tau) [0 .. (fromIntegral setSize)]

    -- Convert the powers of tau to integers back
    let powerOfTauInt = map F.unScalar powerOfTauField

    -- Convert the integers to Scalars of powerOfTauInt
    powerOfTauScalar <- mapM scalarFromInteger powerOfTauInt

    -- Define the generators of G1 and G2
    let g1 = blsGenerator :: Point1
    let g2 = blsGenerator :: Point2

    -- Map the power of tau over both G1 and G2
    let crsG1 = map (blsMult g1) powerOfTauInt :: [Point1]
    let crsG2 = map (blsMult g2) powerOfTauInt :: [Point2]

    -- Define a subset we are proving
    let subset = take subSetSize set :: [Element]

    return (setMap, subset, crsG1, crsG2)

-- Helper to extract the result or force an error
benchmarkProofG1 :: [Element] -> Accumulator -> [Point1] -> IO ()
benchmarkProofG1 subSet setMap crsG1 = do
    result <- getProofOverG1 subSet setMap crsG1
    case result of
        Left err -> error err -- Force an error to trigger computation
        Right _ -> return ()

benchmarkProofG2 :: [Element] -> Accumulator -> [Point2] -> IO ()
benchmarkProofG2 subSet setMap crsG2 = do
    result <- getProofOverG2 subSet setMap crsG2
    case result of
        Left err -> error err -- Force an error to trigger computation
        Right _ -> return ()

-- Main function with benchmarking
main :: IO ()
main = do
    -- Create a test setup with 1_000 elements and a subset of 1 (the proof is over the set minus the subset)
    (setMap, subSet, crsG1, crsG2) <- mkTestSetup 1_000 1

    -- Benchmark the two calculations
    defaultMain
        [ bgroup
            "proof calculations"
            [ bench "getProofOverG1" $ nfIO (benchmarkProofG1 subSet setMap crsG1)
            , bench "getProofOverG2" $ nfIO (benchmarkProofG2 subSet setMap crsG2)
            ]
        ]

-- -- Get proof over G1 and G2
-- comm1 <- getProofOverG1 subSet setMap crsG1
-- comm2 <- getProofOverG2 subSet setMap crsG2

-- let g1 = blsGenerator :: Point1
-- let g2 = blsGenerator :: Point2

-- -- Verify the pairing
-- case comm1 of
--     Left err -> print err
--     Right g1Commitment -> case comm2 of
--         Left err -> print err
--         Right g2Commitment -> do
--             let pt1 = millerLoop g1Commitment g2
--                 pt2 = millerLoop g1 g2Commitment
--                 pairingCheck = ptFinalVerify pt1 pt2
--             print pairingCheck

-- print "Run complete"
