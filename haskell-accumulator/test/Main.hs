module Main where

import Bindings.Internal (getPolyCommitmentG1, getPolyCommitmentG2)
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
import GHC.IO (unsafePerformIO)

main :: IO ()
main = do
    -- This represents the roots of the polynomial (x + 1)^5 = x^5 + 5x^4 + 10x^3 + 10x^2 + 5x + 1
    -- which is a polynomial of degree 5 (so it has 6 coefficients)
    let roots = [1, 1, 1, 1, 1]
    -- Convert the integers to Scalars
    scalars <- mapM scalarFromInteger roots
    -- Convert the Scalars to Fr (the underlying field element type of blst we use (blst_fr))
    scalarsFr <- mapM frFromScalar scalars
    -- Define tau
    let tauInt = 10
    -- define powers of tau
    let powerOfTauInt = [1, 10, 100, 1000, 10000, 100000]
    -- Convert the integers to Scalars of powerOfTauInt
    powerOfTau <- mapM scalarFromInteger powerOfTauInt
    -- Define the generators of G1 and G2
    let g1 = blsGenerator :: Point1
    let g2 = blsGenerator :: Point2
    -- map the power of tau over both G1 and G2
    let crsG1 = map (blsMult g1) powerOfTauInt :: [Point1]
    let crsG2 = map (blsMult g2) powerOfTauInt :: [Point2]
    -- call the FFI functions to get the poly commitment
    comm1 <- getPolyCommitmentG1 scalarsFr crsG1
    comm2 <- getPolyCommitmentG2 scalarsFr crsG2
    -- verify the pairing, similar to the test in the rust-accumulator library
    case comm1 of
        Left err -> print err
        Right g1Commitment -> case comm2 of
            Left err -> print err
            Right g2Commitment -> do
                let pt1 = millerLoop g1Commitment g2
                let pt2 = millerLoop g1 g2Commitment
                let pairingCheck = ptFinalVerify pt1 pt2
                print pairingCheck
    print "Run complete"
