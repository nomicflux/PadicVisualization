module Component.Animation where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Math as Math

newtype Tick = Tick Int

incTick :: Tick -> Maybe Tick
incTick t@(Tick n)
  | atMax t = Just (Tick (n + 1))
  | otherwise = Nothing

maxTick :: Tick
maxTick = Tick 100

atMax :: Tick -> Boolean
atMax (Tick n) =
  let (Tick x) = maxTick
  in n >= x

proportionalTick :: Maybe Tick -> Number
proportionalTick Nothing = 1.0
proportionalTick (Just (Tick n)) =
  let (Tick x) = maxTick
  in (toNumber n) / (toNumber x)

interpolate :: (Number -> Number) ->
               Maybe Tick ->
               Number -> Number -> Number
interpolate f t to from =
  let prop = f (proportionalTick t)
  in (to - from) * prop

cosInterpolate :: Maybe Tick -> Number -> Number -> Number
cosInterpolate = interpolate (Math.cos <<< (\x -> Math.pi / 4.0 * x))

sinInterpolate :: Maybe Tick -> Number -> Number -> Number
sinInterpolate = interpolate (Math.sin <<< (1.0 - _) <<< (\x -> Math.pi / 4.0 * x))

linInterpolate :: Maybe Tick -> Number -> Number -> Number
linInterpolate = interpolate identity
