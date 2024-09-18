{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

module Bindings where

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Curve1, Curve2, Fr (..), Point (..), Point1, Point2, PointPtr (..), sizeFr, sizePoint, withNewPoint')
import Control.Monad (forM, forM_, when)
import Data.Data (Proxy (..))
import Data.Void (Void)
import Foreign (ForeignPtr, copyBytes, mallocForeignPtrBytes, withForeignPtr)
import Foreign.C.Types (CSize (..))
import Foreign.Ptr (Ptr, plusPtr)

-- [General notes on this file]
-- This file contains the FFI bindings to the Rust library 'rust-accumulator' with
-- source location https://github.com/input-output-hk/rust-accumulator
-- This rust lib is compiled to C and then linked using haskell.nix to the Haskell.
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
getPolyCommitmentG1_ frs pts = do
    -- Concatenate the Fr elements into a single ForeignPtr
    frPtr <- concatFr frs
    -- Concatenate the Point1 elements into a single ForeignPtr
    ptPtr <- concatPoint1 pts
    -- Call the FFI function with a newly allocated Point1 using withNewPoint'
    withNewPoint' $ \(PointPtr out) ->
        -- Unwrap the PointPtr to get the raw Ptr Void
        withForeignPtr frPtr $ \fr ->
            withForeignPtr ptPtr $ \pt ->
                get_poly_commitment_g1 out fr (fromIntegral $ length frs) pt (fromIntegral $ length pts)

-- | Unsafe version of getPolyCommitmentG2 that does not perform any bounds checking.
getPolyCommitmentG2_ :: [Fr] -> [Point2] -> IO Point2
getPolyCommitmentG2_ frs pts = do
    -- Concatenate the Fr elements into a single ForeignPtr
    frPtr <- concatFr frs
    -- Concatenate the Point2 elements into a single ForeignPtr
    ptPtr <- concatPoint2 pts
    -- Call the FFI function with a newly allocated Point2 using withNewPoint'
    withNewPoint' $ \(PointPtr out) ->
        withForeignPtr frPtr $ \fr ->
            withForeignPtr ptPtr $ \pt ->
                get_poly_commitment_g2 out fr (fromIntegral $ length frs) pt (fromIntegral $ length pts)

-- | Safe version of getPolyCommitmentG1 that performs bounds checking.
getPolyCommitmentG1 :: [Fr] -> [Point1] -> IO (Either String Point1)
getPolyCommitmentG1 frs pts = do
    if null frs
        then return $ Left "The scalar list cannot be empty."
        else do
            let ptsExpectedSize = length frs + 1
            if length pts < ptsExpectedSize
                then return $ Left "The G1 points list must be at least one element larger than the scalar list."
                else do
                    result <- getPolyCommitmentG1_ frs (take ptsExpectedSize pts)
                    return $ Right result

-- | Safe version of getPolyCommitmentG2 that performs bounds checking.
getPolyCommitmentG2 :: [Fr] -> [Point2] -> IO (Either String Point2)
getPolyCommitmentG2 frs pts = do
    -- Check if the list of Fr elements is empty
    if null frs
        then return $ Left "The scalar list cannot be empty."
        else do
            -- Check if the list of Point2 is at least one element larger than Fr
            let ptsExpectedSize = length frs + 1
            if length pts < ptsExpectedSize
                then return $ Left "The G2 points list must be at least one element larger than the scalar list."
                else do
                    result <- getPolyCommitmentG2_ frs (take ptsExpectedSize pts)
                    return $ Right result

-- [Helper functions]

-- | given a list of Fr elements (under the hood blst_f objects), concatenate them into a single ForeignPtr
concatFr :: [Fr] -> IO (ForeignPtr Void)
concatFr frList = do
    -- Calculate total size needed
    let totalSize = sizeFr * length frList

    -- Allocate the final large ForeignPtr
    finalPtr <- mallocForeignPtrBytes totalSize

    -- Copy each Fr element into the finalPtr
    withForeignPtr finalPtr $ \destPtr -> do
        let copyElement offset (Fr srcPtr) = withForeignPtr srcPtr $ \src ->
                copyBytes (destPtr `plusPtr` offset) src sizeFr
        -- Use a forM_ loop to copy each element
        forM_ (zip [0, sizeFr ..] frList) $ uncurry copyElement

    -- Return the final ForeignPtr
    return finalPtr

-- | Function to split a ForeignPtr containing multiple Fr elements into a list of individual Fr elements
splitFr :: ForeignPtr Void -> Int -> IO [Fr]
splitFr srcPtr n = do
    -- Iterate over the number of elements and extract each `Fr`
    withForeignPtr srcPtr $ \src ->
        forM [0 .. n - 1] $ \i -> do
            -- Allocate memory for each individual `Fr`
            frPtr <- mallocForeignPtrBytes sizeFr
            -- Copy the corresponding bytes from the original buffer
            withForeignPtr frPtr $ \dst ->
                copyBytes dst (src `plusPtr` (i * sizeFr)) sizeFr
            -- Return the new `Fr`
            return $ Fr frPtr

-- | given a list of Point1 elements, concatenate them into a single ForeignPtr
concatPoint1 :: [Point1] -> IO (ForeignPtr Void)
concatPoint1 pointList = do
    -- Calculate total size needed (sizePoint for each Point1)
    let totalSize = sizePoint (Proxy @Curve1) * length pointList

    -- Allocate the final large ForeignPtr
    finalPtr <- mallocForeignPtrBytes totalSize

    -- Copy each Point1 element into the finalPtr
    withForeignPtr finalPtr $ \destPtr -> do
        let copyElement offset (Point srcPtr) = withForeignPtr srcPtr $ \src ->
                copyBytes (destPtr `plusPtr` offset) src (sizePoint (Proxy @Curve1))
        -- Use a forM_ loop to copy each element
        forM_ (zip [0, sizePoint (Proxy @Curve1) ..] pointList) $ uncurry copyElement

    -- Return the final ForeignPtr
    return finalPtr

-- | Function to split a ForeignPtr containing multiple Point1 elements into a list of individual Point1 elements
splitPoint1 :: ForeignPtr Void -> Int -> IO [Point1]
splitPoint1 srcPtr n = do
    -- Iterate over the number of elements and extract each `Point1`
    withForeignPtr srcPtr $ \src ->
        forM [0 .. n - 1] $ \i -> do
            -- Allocate memory for each individual `Point1`
            pointPtr <- mallocForeignPtrBytes (sizePoint (Proxy @Curve1))
            -- Copy the corresponding bytes from the original buffer
            withForeignPtr pointPtr $ \dst ->
                copyBytes dst (src `plusPtr` (i * sizePoint (Proxy @Curve1))) (sizePoint (Proxy @Curve1))
            -- Return the new `Point1`
            return $ Point pointPtr

-- | given a list of Point2 elements, concatenate them into a single ForeignPtr
concatPoint2 :: [Point2] -> IO (ForeignPtr Void)
concatPoint2 pointList = do
    -- Calculate total size needed (sizePoint for each Point2)
    let totalSize = sizePoint (Proxy @Curve2) * length pointList

    -- Allocate the final large ForeignPtr
    finalPtr <- mallocForeignPtrBytes totalSize

    -- Copy each Point2 element into the finalPtr
    withForeignPtr finalPtr $ \destPtr -> do
        let copyElement offset (Point srcPtr) = withForeignPtr srcPtr $ \src ->
                copyBytes (destPtr `plusPtr` offset) src (sizePoint (Proxy @Curve2))
        -- Use a forM_ loop to copy each element
        forM_ (zip [0, sizePoint (Proxy @Curve2) ..] pointList) $ uncurry copyElement

    -- Return the final ForeignPtr
    return finalPtr

-- | Function to split a ForeignPtr containing multiple Point2 elements into a list of individual Point2 elements
splitPoint2 :: ForeignPtr Void -> Int -> IO [Point2]
splitPoint2 srcPtr n = do
    -- Iterate over the number of elements and extract each `Point2`
    withForeignPtr srcPtr $ \src ->
        forM [0 .. n - 1] $ \i -> do
            -- Allocate memory for each individual `Point2`
            pointPtr <- mallocForeignPtrBytes (sizePoint (Proxy @Curve2))
            -- Copy the corresponding bytes from the original buffer
            withForeignPtr pointPtr $ \dst ->
                copyBytes dst (src `plusPtr` (i * sizePoint (Proxy @Curve2))) (sizePoint (Proxy @Curve2))
            -- Return the new `Point2`
            return $ Point pointPtr

---- [FFI bindings]

foreign import ccall "get_poly_commitment_g1" get_poly_commitment_g1 :: Ptr Void -> Ptr Void -> CSize -> Ptr Void -> CSize -> IO ()
foreign import ccall "get_poly_commitment_g2" get_poly_commitment_g2 :: Ptr Void -> Ptr Void -> CSize -> Ptr Void -> CSize -> IO ()
