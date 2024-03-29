module PadicVector where

import Data.Eq ((==))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (pow, toNumber)
import Data.List (List, (:))
import Data.List as L
import Data.List.Lazy as LL
import Data.Monoid ((<>))
import Data.Number as Number
import HalogenHelpers.Coordinates (Coordinates)
import PolarCoordinates (PolarCoordinates, mkPolar, polarToCartesian)
import Prelude (mod, otherwise, (*), (+), (-), (/))

padicRep :: Int -> Int -> List Int
padicRep p x = L.reverse (go L.Nil x)
 where
   go acc y
     | y == 0 = 0 : acc
     | otherwise =
       let m = y `mod` p
       in go (m : acc) ((y - m) / p)

addCoords :: Coordinates -> Coordinates -> Coordinates
addCoords a b = { x: a.x + b.x
                , y: a.y + b.y
                }

calcStep :: Int -> Int -> Int -> PolarCoordinates
calcStep p n d =
  let r = 1.0 / toNumber (p `pow` n)
      theta = Number.tau / toNumber p * toNumber d
  in mkPolar r theta

toCoordList :: Int -> Int -> Int -> List PolarCoordinates
toCoordList maxLength p x =
  let padic = padicRep p x
      extra = L.fromFoldable (LL.replicate (maxLength - (L.length padic)) 0)
  in mapWithIndex (calcStep p) (padic <> extra)

baseCoordinates :: Coordinates
baseCoordinates = {x: 0.0, y: 0.0}

toVector :: Int -> Int -> Int -> Coordinates
toVector max p x =
  let maxP = padicRep p max
      cl = toCoordList (L.length maxP + 1) p x
  in L.foldl (\acc next -> addCoords acc (polarToCartesian next)) baseCoordinates cl
