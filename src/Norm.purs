module Norm where

import Prelude

import Data.Int (pow)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Rational (Rational, fromInt, (%))

data Norm = Inf | Padic Int

getPrime :: Norm -> Maybe Int
getPrime Inf = Nothing
getPrime (Padic p) = Just p

isPadic :: Norm -> Boolean
isPadic Inf = false
isPadic (Padic _) = true

takeNorm :: Norm -> Int -> Rational
takeNorm _ 0 = fromInt 0
takeNorm Inf n = fromInt $ abs n
takeNorm (Padic p) n = 1 % (p `pow` takeNormPower n)
  where takeNormPower m
          | m `mod` p == 0 = 1 + takeNormPower (m / p)
          | otherwise = 0
