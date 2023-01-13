module PolarCoordinates where

import Data.Number as Number
import HalogenHelpers.Coordinates (Coordinates)
import Prelude ((*))

type PolarCoordinates = { r :: Number
                        , theta :: Number
                        }

mkPolar :: Number -> Number -> PolarCoordinates
mkPolar r theta = { r, theta }

polarToCartesian :: PolarCoordinates -> Coordinates
polarToCartesian coords =
  { x: coords.r * Number.cos coords.theta
  , y: coords.r * Number.sin coords.theta
  }

scale :: Number -> PolarCoordinates -> PolarCoordinates
scale n coords = { r: coords.r * n
                 , theta: coords.theta
                 }
