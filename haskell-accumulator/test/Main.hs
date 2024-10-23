{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Accumulator (Accumulator, Element (..), addElement, emptyAccumulator, removeElement)
import Bindings (getPolyCommitOverG1, getPolyCommitOverG2)
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
import Data.List (nub)
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

-- Helper function to pick n random elements from a list
pickRandomElements :: Int -> [a] -> IO [a]
pickRandomElements n xs = mapM (\_ -> (xs !!) <$> randomRIO (0, length xs Prelude.- 1)) [1 .. n]

-- Create test setup with a set of elements and a subset of that set
mkTestSetup :: Int -> Int -> IO (Accumulator, [Element], [Point1], [Point2])
mkTestSetup setSize subSetSize = do
    -- Generate a random set of ByteStrings
    set <- generateRandomSet setSize
    let setMap = foldl addElement emptyAccumulator set :: Accumulator

    -- Define a tau
    let tau = F.Scalar 22_435_875_175_126_190_499_447_740_508_185_965_837_690_552_500_527_637_822_603_658_699_938_581_184_511

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

    -- Randomly pick subSetSize elements from the set
    subsetNonUnique <- pickRandomElements subSetSize set
    let subset = nub subsetNonUnique

    return (setMap, subset, crsG1, crsG2)

-- Helper to extract the result or force an error
benchmarkProofG1 :: [Element] -> Accumulator -> [Point1] -> IO ()
benchmarkProofG1 subSet setMap crsG1 = do
    result <- getPolyCommitOverG1 subSet setMap crsG1
    case result of
        Left err -> error err -- Force an error to trigger computation
        Right _ -> return ()

benchmarkProofG2 :: [Element] -> Accumulator -> [Point2] -> IO ()
benchmarkProofG2 subSet setMap crsG2 = do
    result <- getPolyCommitOverG2 subSet setMap crsG2
    case result of
        Left err -> error err -- Force an error to trigger computation
        Right _ -> return ()

-- Main function with benchmarking
main :: IO ()
main = do
    -- Create a test setup with 1_000 elements and a subset of 1 (the proof is over the set minus the subset)
    (setMap, subSet, crsG1, crsG2) <- mkTestSetup 10 2

    -- Benchmark the two calculations
    defaultMain
        [ bgroup
            "proof calculations"
            [ bench "getProofOverG1" $ nfIO (benchmarkProofG1 subSet setMap crsG1)
            , bench "getProofOverG2" $ nfIO (benchmarkProofG2 subSet setMap crsG2)
            ]
        ]

    -- An basic E2E example.

    -- Say we have this set of elements (note that the first argument of the mkTestSetup function call above
    -- should be bigger than the length of this set)
    let mySet = ["element1", "element2", "element3", "element4"] :: [Element]
        -- We can get the offchain explicit accumulator via
        myAcc = foldl addElement emptyAccumulator mySet :: Accumulator

    --  Then we can calculate the onchain representation of our accumulator via
    accCommit <- getPolyCommitOverG2 [] myAcc crsG2

    -- Say we want to proof the subset of the set
    let mySubset = ["element1", "element4"] :: [Element]

    -- then the proof can be calculated via
    proof <- getPolyCommitOverG2 mySubset myAcc crsG2

    -- Verify the proof onchain (but doing it with offchain code)
    case accCommit of
        Left err -> print $ err ++ " for accumulator"
        Right g2AccCommit -> case proof of
            Left err -> print $ err ++ " for proof"
            Right g2ProofCommit -> do
                let subsetAcc = foldl addElement emptyAccumulator mySubset :: Accumulator
                    g1 = blsGenerator :: Point1
                subsetCommit <- getPolyCommitOverG1 [] subsetAcc crsG1
                case subsetCommit of
                    Left err -> print $ err ++ " for subset accumulator"
                    Right g1SubsetCommit -> do
                        let pt1 = millerLoop g1 g2AccCommit
                            pt2 = millerLoop g1SubsetCommit g2ProofCommit
                            pairingCheck = ptFinalVerify pt1 pt2
                        print pairingCheck

    print "Run complete"
