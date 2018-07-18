module Norm where

import Prelude

import Data.Int (pow)
import Data.Ord (abs)
import Data.Rational (Rational, fromInt, (%))

data Norm = Inf | Padic Int

takeNorm :: Norm -> Int -> Rational
takeNorm _ 0 = fromInt 0
takeNorm Inf n = fromInt $ abs n
takeNorm (Padic p) n = 1 % (p `pow` takeNormPower n)
  where takeNormPower m
          | n `mod` p == 0 = 1 + takeNormPower (m / p)
          | otherwise = 0
