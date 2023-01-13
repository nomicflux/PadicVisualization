module Component.Bubble where

import Prelude

import Data.Int (round, toNumber)
import Data.List as L
import Data.List (List)
import Data.Number as Number
import Norm (Norm(..))
import Roots as Roots

cubeBy :: Int -> Int -> Int -> Int -> Int -> Int
cubeBy cube sqr by plus v = v*v*v*cube + v*v*sqr + v*by + plus

normSqrt :: Norm -> Int -> (Int -> List Int)
normSqrt Inf _ = L.singleton <<< round <<< Number.sqrt <<< toNumber
normSqrt (Padic p) steps = Roots.pSqrt p steps

normCbrt :: Norm -> Int -> (Int -> List Int)
normCbrt Inf _ = L.singleton <<< round <<< flip Number.pow (1.0 / 3.0) <<< toNumber
normCbrt (Padic p) steps = Roots.pCbrt p steps
