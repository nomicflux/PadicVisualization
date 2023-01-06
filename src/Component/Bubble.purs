module Component.Bubble where

import Prelude

import Data.Int (round, toNumber)
import Data.List as L
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Rational (Rational)
import Math as Math
import Norm (Norm(..), takeNorm)
import Roots as Roots

cubeBy :: Int -> Int -> Int -> Int -> Int -> Int
cubeBy cube sqr by plus v = v*v*v*cube + v*v*sqr + v*by + plus

normSqrt :: Norm -> Int -> (Int -> List Int)
normSqrt Inf _ = L.singleton <<< round <<< Math.sqrt <<< toNumber
normSqrt (Padic p) steps = Roots.pSqrt p steps

sqrtBubble :: Norm -> Int -> Int -> List Int
sqrtBubble norm steps = normSqrt norm steps
