{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Accumulator (Accumulator, Element (..), addElement, buildAccumulator, emptyAccumulator, removeElement)
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
import qualified Data.Set as Set
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
import Text.Printf (printf)

-- | Helper function to convert a ByteString to a hex string
byteStringAsHex :: B.ByteString -> String
byteStringAsHex bs = "0x" ++ concat (B.foldr' (\w s -> printf "%02x" w : s) [] bs)

-- | Helper function to generate a random ByteString of a given length
generateRandomByteString :: Int -> IO B.ByteString
generateRandomByteString n = B.pack <$> mapM (\_ -> randomRIO (0, 255)) [1 .. n]

-- | Generate a random set of N ByteStrings
generateRandomSet :: Int -> IO [Element]
generateRandomSet n = mapM (\_ -> generateRandomByteString 32) [1 .. n]

-- | Helper function to pick n random elements from a list
pickRandomElements :: Int -> [a] -> IO [a]
pickRandomElements n xs = mapM (\_ -> (xs !!) <$> randomRIO (0, length xs Prelude.- 1)) [1 .. n]

-- | Create test setup with a set of elements and a subset of that set
generateTestSetup :: Int -> Int -> IO (Accumulator, [Element], [Point1], [Point2])
generateTestSetup setSize subSetSize = do
    -- Generate a random set of ByteStrings
    set <- generateRandomSet setSize
    let accumulator = buildAccumulator set

    -- Define a tau (a large secret value that no one knows)
    let tau = F.Scalar 22_435_875_175_126_190_499_447_740_508_185_965_837_690_552_500_527_637_822_603_658_699_938_581_184_511

    -- Define powers of tau (tau^0, tau^1, ..., tau^setSize+1 over the field)
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
    -- let subset = nub subsetNonUnique
    let subset = Set.toList $ Set.fromList subsetNonUnique

    return (accumulator, subset, crsG1, crsG2)

-- | Helper to extract the result or force an error
benchmarkProofG1 :: [Element] -> Accumulator -> [Point1] -> ()
benchmarkProofG1 subSet setMap crsG1 =
    case getPolyCommitOverG1 subSet setMap crsG1 of
        Left err -> error err -- Force an error to trigger computation
        Right _ -> ()

-- | Helper to extract the result or force an error
benchmarkProofG2 :: [Element] -> Accumulator -> [Point2] -> ()
benchmarkProofG2 subSet setMap crsG2 =
    case getPolyCommitOverG2 subSet setMap crsG2 of
        Left err -> error err -- Force an error to trigger computation
        Right _ -> ()

-- | Run benchmarks for proof calculations
runBenchmarks :: IO ()
runBenchmarks = do
    (accumulator, subSet, crsG1, crsG2) <- generateTestSetup 1_000 0
    defaultMain
        [ bgroup
            "proof calculations"
            [ bench "getProofOverG1" $ whnf (\() -> benchmarkProofG1 subSet accumulator crsG1) ()
            , bench "getProofOverG2" $ whnf (\() -> benchmarkProofG2 subSet accumulator crsG2) ()
            ]
        ]

-- | Run an end-to-end example
runE2EExample :: IO ()
runE2EExample = do
    -- Create a test setup with 1_000 elements and a subset of size 0 (the proof is over the set minus the subset, so this is a good test case)
    (accumulator, subSet, crsG1, crsG2) <- generateTestSetup 1_000 0

    -- Say we have this set of elements (note that the first argument of the generateTestSetup function call above
    -- should be bigger than the length of this set, as we use the CRS from there)
    let mySet = ["element1", "element2", "element3", "element4", "element5", "element6", "element7", "element8", "element9", "element10"] :: [Element]
        -- We can get the offchain explicit accumulator via
        myAcc = buildAccumulator mySet

    --  Then we can calculate the onchain representation of our accumulator via
    let accCommit = getPolyCommitOverG2 [] myAcc crsG2

        -- Say we want to proof the subset of the set
        mySubset = mySet -- ["element3", "element9"] :: [Element]

        -- then the proof can be calculated via
        proof = getPolyCommitOverG2 mySubset myAcc crsG2

    -- Verify the proof onchain (but doing it with offchain code)
    case (accCommit, proof) of
        (Left err, _) -> print $ err ++ " for accumulator"
        (_, Left err) -> print $ err ++ " for proof"
        (Right g2AccCommit, Right g2ProofCommit) -> do
            let subsetAcc = buildAccumulator mySubset
                subsetCommit = getPolyCommitOverG1 [] subsetAcc crsG1
            case subsetCommit of
                Left err -> print $ err ++ " for subset accumulator"
                Right g1SubsetCommit -> do
                    let g1 = blsGenerator :: Point1
                        pt1 = millerLoop g1 g2AccCommit
                        pt2 = millerLoop g1SubsetCommit g2ProofCommit
                        pairingCheck = ptFinalVerify pt1 pt2
                        accBS = blsCompress g2AccCommit
                        proofBS = blsCompress g2ProofCommit
                    print "Proving that the subset is in the set for:"
                    print $ "Subset: " ++ show mySubset
                    print $ "With proof: " ++ show (byteStringAsHex proofBS)
                    print $ "Set: " ++ show mySet
                    print $ "With accumulator commitment: " ++ show (byteStringAsHex accBS)
                    print $ "The proof is evaluated as: " ++ show pairingCheck
                    print "E2E run complete"

-- Main function with benchmarking and an E2E example
main :: IO ()
main = do
    runBenchmarks
    runE2EExample
