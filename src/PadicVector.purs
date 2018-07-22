module PadicVector where

import Data.Eq ((==))
import Data.Int (pow, toNumber)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>), (>=))
import Data.Tuple (Tuple(..))
import HalogenHelpers.Coordinates (Coordinates)
import Math as Math
import PolarCoordinates (PolarCoordinates, mkPolar, polarToCartesian)
import Prelude (mod, otherwise, ($), (*), (+), (-), (/), (<=))

padicRep :: Int -> Int -> List Int
padicRep p x = go x
 where
   go y
     | y == 0 = L.Nil
     | otherwise =
       let m = y `mod` p
       in L.Cons m (go ((y - m) / p))

addCoords :: Coordinates -> Coordinates -> Coordinates
addCoords a b = { x: a.x + b.x
                , y: a.y + b.y
                }

calcStep :: Int -> Int -> Int -> PolarCoordinates
calcStep p n d =
  let r = 1.0 / toNumber (p `pow` n)
      theta = Math.tau / toNumber p * toNumber d
  in mkPolar r theta

toCoordList :: Int -> Int -> List PolarCoordinates
toCoordList p x =
  let padic = padicRep p x
  in L.mapWithIndex (calcStep p) padic

baseCoordinates :: Coordinates
baseCoordinates = {x: 0.0, y: 0.0}

toVector :: Int -> Int -> Coordinates
toVector p x =
  let cl = toCoordList p x
  in L.foldl (\acc next -> addCoords acc (polarToCartesian next)) baseCoordinates cl
