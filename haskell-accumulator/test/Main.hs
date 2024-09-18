module Main where

import Bindings (getPolyCommitmentG1, getPolyCommitmentG2)
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Scalar (..), frFromScalar, scalarFromFr, scalarFromInteger, scalarToInteger)
import GHC.IO (unsafePerformIO)

main :: IO ()
main = do
    let roots = [1, 1, 1, 1, 1]
    -- Convert the integers to Scalars
    scalars <- mapM scalarFromInteger roots
    x <- scalarToInteger (head scalars)
    -- Convert the Scalars to Fr values
    frs <- mapM frFromScalar scalars
    print x

-- TODO:
-- get affine G1/G2
-- define tau
-- define powers of tau
-- crs over G1
-- crs over G2
-- call the functions getPolyCommitmentG1 and getPolyCommitmentG2
