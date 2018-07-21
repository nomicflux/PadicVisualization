module PadicVector where

import Data.Eq ((==))
import Data.Int (pow, toNumber)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Ord ((>))
import Data.Tuple (Tuple(..))
import HalogenHelpers.Coordinates (Coordinates)
import Math as Math
import PolarCoordinates (PolarCoordinates, mkPolar, polarToCartesian)
import Prelude (mod, otherwise, ($), (*), (+), (-), (/))

addCoords :: Coordinates -> Coordinates -> Coordinates
addCoords a b = { x: a.x + b.x
                , y: a.y + b.y
                }

calcStep :: Int -> Int -> Int -> Tuple PolarCoordinates (Maybe Int)
calcStep p n x =
  let power = p `pow` n
      pow_1 = toNumber $ p `pow` (n - 1)
      m = x `mod` power
      r = 1.0 / pow_1
      theta = Math.pi * 2.0 / toNumber power * toNumber m
      pc = mkPolar r theta
  in if power > x
     then Tuple pc Nothing
     else Tuple pc (Just $ x - m)

toCoordList :: Int -> Int -> Int -> List PolarCoordinates
toCoordList p n x =
  let (Tuple pc next) = calcStep p n x
  in case next of
    Nothing -> L.Cons pc L.Nil
    Just y -> L.Cons pc (toCoordList p (n + 1) y)

baseCoordinates :: Coordinates
baseCoordinates = {x: 0.0, y: 0.0}

toVector :: Int -> Int -> Coordinates
toVector p x
  | x == 0 = baseCoordinates
  | otherwise =
    let cl = toCoordList p 1 x
    in L.foldl (\acc next -> addCoords acc (polarToCartesian next)) baseCoordinates cl
