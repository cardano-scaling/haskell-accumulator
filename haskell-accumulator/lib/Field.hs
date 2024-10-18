{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Field where

import Data.String (String)
import PlutusTx.Numeric (
    AdditiveGroup (..),
    AdditiveMonoid (..),
    AdditiveSemigroup (..),
    Module (..),
    MultiplicativeMonoid (..),
    MultiplicativeSemigroup (..),
    (*),
    (+),
    (-),
 )
import PlutusTx.Prelude (
    AdditiveGroup (..),
    AdditiveMonoid (..),
    AdditiveSemigroup (..),
    Either (..),
    Eq ((==)),
    Integer,
    Module (..),
    MultiplicativeMonoid (..),
    MultiplicativeSemigroup (..),
    Ord ((<), (<=)),
    divide,
    error,
    even,
    modulo,
    otherwise,
    ($),
    (&&),
 )

bls12_381_scalar_prime :: Integer
bls12_381_scalar_prime = 52435875175126190479447740508185965837690552500527637822603658699938581184513

newtype Scalar = Scalar {unScalar :: Integer}

mkScalar :: Integer -> Either String Scalar
mkScalar n = if 0 <= n && n < bls12_381_scalar_prime then Right (Scalar n) else Left "Scalar not in field"

instance AdditiveSemigroup Scalar where
    {-# INLINEABLE (+) #-}
    (+) (Scalar a) (Scalar b) = Scalar $ (a + b) `modulo` bls12_381_scalar_prime

instance AdditiveMonoid Scalar where
    zero = Scalar 0

instance AdditiveGroup Scalar where
    (-) (Scalar a) (Scalar b) = Scalar $ (a - b) `modulo` bls12_381_scalar_prime

negateScalar :: Scalar -> Scalar
negateScalar (Scalar x) = if x == 0 then Scalar 0 else Scalar $ bls12_381_scalar_prime - x

instance MultiplicativeSemigroup Scalar where
    (*) (Scalar a) (Scalar b) = Scalar $ (a * b) `modulo` bls12_381_scalar_prime

instance MultiplicativeMonoid Scalar where
    one = Scalar 1

powModScalar :: Scalar -> Integer -> Scalar
powModScalar b e
    | e < 0 = zero
    | e == 0 = one
    | even e = powModScalar (b * b) (e `divide` 2)
    | otherwise = b * powModScalar (b * b) ((e - 1) `divide` 2)

extendedEuclidean :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclidean a b =
    if a == 0
        then (b, 0, 1)
        else
            let (gcd, x1, y1) = extendedEuclidean (b `modulo` a) a
                x = y1 - b `divide` a * x1
             in (gcd, x, x1)

multiplicativeInverse :: Integer -> Integer -> Integer
multiplicativeInverse m a =
    let (gcd, x, _) = extendedEuclidean a m
     in if gcd == 1 then x `modulo` m else error ()

instance Module Integer Scalar where
    scale :: Integer -> Scalar -> Scalar
    scale e b = powModScalar b e

class (MultiplicativeMonoid a) => MultiplicativeGroup a where
    div :: a -> a -> a
    recip :: a -> a

instance MultiplicativeGroup Scalar where
    recip (Scalar a) = Scalar $ multiplicativeInverse bls12_381_scalar_prime a
    div a b = a * recip b
