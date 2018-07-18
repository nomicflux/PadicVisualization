module Component.Bubble where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Rational (Rational)
import Halogen as H
import HalogenHelpers.Coordinates (Coordinates)
import Math as Math
import Norm (Norm, takeNorm)

newtype Tick = Tick Int

maxTick :: Tick
maxTick = Tick 100

type Model = { oldLocation :: Maybe Coordinates
             , location :: Coordinates
             , value :: Int
             , time :: Maybe Tick
             , norm :: Norm
             }

initialModel :: Norm -> Int -> Coordinates -> Model
initialModel norm value location = { oldLocation: Nothing
                                   , location
                                   , value
                                   , time: Nothing
                                   , norm
                                   }

tick :: Model -> Model
tick model = model { time = model.time >>= incTick }
  where incTick (Tick n) =
          let (Tick x) = maxTick
          in if n == x then Nothing else Just (Tick $ n + 1)

proportionalTick :: Maybe Tick -> Number
proportionalTick Nothing = 1.0
proportionalTick (Just (Tick n)) =
  let (Tick x) = maxTick
  in (toNumber n) / (toNumber x)

cosInterpolate :: Maybe Tick -> Coordinates -> Coordinates -> Coordinates
cosInterpolate t to from =
  let prop = Math.cos $ Math.pi / 4.0 * proportionalTick t
  in { x: from.x + (to.x - from.x) * prop
     , y: from.y + (to.y - from.y) * prop
     }

getCoordinates :: Model -> Coordinates
getCoordinates model = maybe model.location (cosInterpolate model.time model.location) model.oldLocation

getValue :: Model -> Rational
getValue model = takeNorm model.norm model.value

data Query a = SendTick (Unit -> a)
             | IncValue (Rational -> a)
             | DecValue (Rational -> a)
             | ChangeNorm Norm (Unit -> a)

incValue :: Model -> Model
incValue model = model { value = model.value + 1 }

decValue :: Model -> Model
decValue model = model { value = model.value - 1 }

changeNorm :: Norm -> Model -> Model
changeNorm norm = _ { norm = norm }

data Message = Noop

eval :: forall m. Query ~> H.ComponentDSL Model Query Message m
eval (SendTick reply) = H.modify_ tick >>= \_ -> pure (reply unit)
eval (IncValue reply) = H.modify incValue >>= (pure <<< reply <<< getValue)
eval (DecValue reply) = H.modify decValue >>= (pure <<< reply <<< getValue)
eval (ChangeNorm norm reply) = H.modify_ (changeNorm norm) >>= \_ -> pure (reply unit)
