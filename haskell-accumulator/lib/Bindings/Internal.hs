{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bindings.Internal (
    getPolyCommitmentG1,
    getPolyCommitmentG2,
) where

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (
    Curve1,
    Curve2,
    Fr (..),
    Point1,
    Point2,
    PointPtr (..),
    sizeFr,
    sizePoint,
    withNewPoint',
    withPoint,
 )
import Cardano.Crypto.PinnedSizedBytes (psbUseAsCPtr)
import Control.Monad (forM_)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import Foreign (copyBytes)
import Foreign.C.Types (CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import System.IO.Unsafe (unsafePerformIO)

-- [General notes on this file]
-- This file contains the FFI bindings to the Rust library 'rust-accumulator' with
-- source location https://github.com/cardano-scaling/rust-accumulator
-- This rust lib is compiled to C and then linked using haskell.nix to Haskell.
-- This FFI below uses two functions from the Rust library:
--
-- 1. `getPolyCommitmentG1` - This function takes in a pointer to a projective g1 point and two lists of Fr elements
--    and g1 power of tau elements. It mutates the projective g1 point in the first argument to be the poly commitment.
--
-- 2. `getPolyCommitmentG2` - This function takes in a pointer to a projective g2 point and two lists of Fr elements
--    and g2 power of tau elements. It mutates the projective g2 point in the first argument to be the poly commitment.
--
-- Note that both functions are unsafe and do not perform any bounds checking. It is the responsibility of the caller to
-- ensure that the input lists are of the correct length.

-- | Unsafe version of getPolyCommitmentG1 that does not perform any bounds checking.
getPolyCommitmentG1_ :: [Fr] -> [Point1] -> IO Point1
getPolyCommitmentG1_ frs pts =
    withFrContiguous frs $ \fr ->
        withPoint1Contiguous pts $ \pt ->
            withNewPoint' $ \(PointPtr out) ->
                get_poly_commitment_g1 (castPtr out) fr (fromIntegral $ length frs) pt (fromIntegral $ length pts)

-- | Unsafe version of getPolyCommitmentG2 that does not perform any bounds checking.
getPolyCommitmentG2_ :: [Fr] -> [Point2] -> IO Point2
getPolyCommitmentG2_ frs pts =
    withFrContiguous frs $ \fr ->
        withPoint2Contiguous pts $ \pt ->
            withNewPoint' $ \(PointPtr out) ->
                get_poly_commitment_g2 (castPtr out) fr (fromIntegral $ length frs) pt (fromIntegral $ length pts)

-- | Safe version of getPolyCommitmentG1_ that performs bounds checking.
getPolyCommitmentG1 :: [Fr] -> [Point1] -> Either String Point1
getPolyCommitmentG1 frs pts = unsafePerformIO $ do
    let ptsExpectedSize = length frs + 1
    if length pts < ptsExpectedSize
        then return $ Left "The G1 points list must be at least one element larger than the scalar list."
        else Right <$> getPolyCommitmentG1_ frs (take ptsExpectedSize pts)

-- | Safe version of getPolyCommitmentG2_ that performs bounds checking.
getPolyCommitmentG2 :: [Fr] -> [Point2] -> Either String Point2
getPolyCommitmentG2 frs pts = unsafePerformIO $ do
    -- Check if the list of Point2 is at least one element larger than Fr
    let ptsExpectedSize = length frs + 1
    if length pts < ptsExpectedSize
        then return $ Left "The G2 points list must be at least one element larger than the scalar list."
        else Right <$> getPolyCommitmentG2_ frs (take ptsExpectedSize pts)

-- | Allocate a contiguous buffer of Fr elements and pass a Ptr to the callback.
withFrContiguous :: [Fr] -> (Ptr Void -> IO a) -> IO a
withFrContiguous frList k =
    allocaBytes (sizeFr * length frList) $ \(destPtr :: Ptr Void) -> do
        forM_ (zip [0, sizeFr ..] frList) $ \(offset, Fr psb) ->
            psbUseAsCPtr psb $ \src ->
                copyBytes (destPtr `plusPtr` offset) src sizeFr
        k destPtr

-- | Allocate a contiguous buffer of Point1 elements and pass a Ptr to the callback.
withPoint1Contiguous :: [Point1] -> (Ptr Void -> IO a) -> IO a
withPoint1Contiguous points k =
    let sz = sizePoint (Proxy :: Proxy Curve1)
    in allocaBytes (sz * length points) $ \(destPtr :: Ptr Void) -> do
        forM_ (zip [0, sz ..] points) $ \(offset, pt) ->
            withPoint pt $ \(PointPtr pPtr) ->
                copyBytes (destPtr `plusPtr` offset) pPtr sz
        k destPtr

-- | Allocate a contiguous buffer of Point2 elements and pass a Ptr to the callback.
withPoint2Contiguous :: [Point2] -> (Ptr Void -> IO a) -> IO a
withPoint2Contiguous points k =
    let sz = sizePoint (Proxy :: Proxy Curve2)
    in allocaBytes (sz * length points) $ \(destPtr :: Ptr Void) -> do
        forM_ (zip [0, sz ..] points) $ \(offset, pt) ->
            withPoint pt $ \(PointPtr pPtr) ->
                copyBytes (destPtr `plusPtr` offset) pPtr sz
        k destPtr

foreign import ccall "get_poly_commitment_g1" get_poly_commitment_g1 :: Ptr Void -> Ptr Void -> CSize -> Ptr Void -> CSize -> IO ()
foreign import ccall "get_poly_commitment_g2" get_poly_commitment_g2 :: Ptr Void -> Ptr Void -> CSize -> Ptr Void -> CSize -> IO ()
