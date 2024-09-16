{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings where

foreign import ccall "get_poly_commitment_g1" get_poly_commitment_g1 :: Ptr Void -> Ptr Void -> CSize -> Ptr Void -> CSize -> IO ()

foreign import ccall "get_poly_commitment_g2" get_poly_commitment_g2 :: Ptr Void -> Ptr Void -> CSize -> Ptr Void -> CSize -> IO ()
