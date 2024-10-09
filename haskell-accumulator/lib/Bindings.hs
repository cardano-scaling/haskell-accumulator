{-# LANGUAGE TypeApplications #-}

module Bindings where

import Bindings.Internal (getPolyCommitmentG1, getPolyCommitmentG2)
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Fr, Point1, Point2, Scalar, frFromScalar, scalarFromInteger)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224)
import Cardano.Crypto.Hash.Class (digest)
import qualified Data.ByteString as B
import Data.Proxy
import PlutusCore.Bitwise (byteStringToInteger)

unsafeGetProofOverG1 :: [B.ByteString] -> [B.ByteString] -> [Point1] -> IO (Either String Point1)
unsafeGetProofOverG1 memb set crs = do
    -- First we convert the ByteStrings to Fr elements
    -- By hashing them with Blake2b-224 to map them in the field
    let membHash = map (digest (Proxy @Blake2b_224)) memb
        setHash = map (digest (Proxy @Blake2b_224)) set
    -- Then we convert those byte strings to Fr elements
    membScalar <- mapM (scalarFromInteger . byteStringToInteger True) membHash
    setScalar <- mapM (scalarFromInteger . byteStringToInteger True) setHash
    -- Then we convert the scalars to fr
    membFr <- mapM frFromScalar membScalar
    setFr <- mapM frFromScalar setScalar
    -- Then we get the poly commitment
    polyCommitment <- getPolyCommitmentG1 membFr crs
    case polyCommitment of
        Left err -> pure $ Left err
        Right polyCommitment -> pure $ Right polyCommitment
