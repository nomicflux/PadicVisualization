module Component.Canvas where

import Prelude

import Component.Animation (Tick, incTick)
import Component.Animation as An
import Component.Bubble (Bubble)
import Component.Bubble as B
import Data.Int (toNumber)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Rational (Rational, fromInt)
import Data.Rational as R
import Halogen as H
import Halogen.HTML as HH
import HalogenHelpers.Coordinates (Coordinates)
import Math as Math
import Norm (Norm(..), isPadic)

type Model = { maxInt :: Int
             , norm :: Norm
             , bubbles :: List Bubble
             , time :: Maybe Tick
             , size :: Int
             , windingNumber :: Int
             }

type Input = { size :: Int
             , maxInt :: Int
             , windingNumber :: Int
             }

initialModel :: Input -> Model
initialModel input = { maxInt: input.maxInt
                     , norm: Inf
                     , bubbles: B.mkBubble <$> (L.range 0 input.maxInt)
                     , time: Nothing
                     , size: input.size
                     , windingNumber: input.windingNumber
                     }

getR :: Model -> Rational -> Rational
getR model value =
  if isPadic model.norm then value else value / fromInt model.size

getTheta :: Model -> Int -> Number
getTheta model value = Math.pi * 2.0 * (toNumber value) / (toNumber model.windingNumber)

polarToCartesian :: Rational -> Number -> Coordinates
polarToCartesian r theta =
  let r' = R.toNumber r
  in { x: r' * Math.cos theta
     , y: r' * Math.sin theta
     }

getNewCoordinates :: Model -> Bubble -> Coordinates
getNewCoordinates model bubble =
  let r = getR model (B.getNormedValue model.norm bubble)
      theta = getTheta model (B.getValue bubble)
  in polarToCartesian r theta

getOldCoordinates :: Model -> Bubble -> Maybe Coordinates
getOldCoordinates model bubble =
  let mr = getR model <$> (B.getNormedOldValue model.norm bubble)
      mtheta = getTheta model <$> (B.getOldValue bubble)
  in polarToCartesian <$> mr <*> mtheta

getCoordinates :: Model -> Bubble -> Coordinates
getCoordinates model bubble =
  let mold = getOldCoordinates model bubble
      new = getCoordinates model bubble
  in case mold of
    Nothing -> new
    Just old ->
      let interpolater = An.cosInterpolate model.time
      in { x: interpolater new.x old.x
         , y: interpolater new.y old.y
         }

component :: forall m. H.Component HH.HTML Query Input Message m
component = H.component { initialState: initialModel
                        , render
                        , eval
                        , receiver: const Nothing
                        }

render :: Model -> H.ComponentHTML Query
render model = HH.div_ []

data Query a = ChangeMax Int (Unit -> a)
             | ChangeWinding Int (Unit -> a)
             | ChangeNorm Norm (Unit -> a)
             | IncValues a
             | DecValues a
             | MoveTick a

data Message = Noop

filterBubbles :: Model -> Model
filterBubbles model =
  let newBubbles = L.filter (\b -> let v = B.getValue b in v <= model.maxInt && v >= 0) model.bubbles
  in model { bubbles = newBubbles }

addBubble :: Bubble -> Model -> Model
addBubble bubble model =
  model { bubbles = L.Cons bubble model.bubbles }

incBubbles :: Model -> Model
incBubbles model =
  model { bubbles = B.incValue <$> model.bubbles }

decBubbles :: Model -> Model
decBubbles model =
  model { bubbles = B.decValue <$> model.bubbles }

eval :: forall m. Query ~> H.ComponentDSL Model Query Message m
eval (ChangeMax max reply) = H.modify_ (_ { maxInt = max }) *> pure (reply unit)
eval (ChangeWinding winding reply) = H.modify_ (_ { windingNumber = winding }) *> pure (reply unit)
eval (ChangeNorm norm reply) = H.modify_ (_ { norm = norm }) *> pure (reply unit)
eval (IncValues next) = do
  model <- H.get
  let newBubble = B.mkBubble 0
  H.modify_ incBubbles
  H.modify_ $ addBubble newBubble
  H.modify_ filterBubbles
  pure next
eval (DecValues next) = do
  model <- H.get
  let newBubble = B.mkBubble model.maxInt
  H.modify_ decBubbles
  H.modify_ $ addBubble newBubble
  H.modify_ filterBubbles
  pure next
eval (MoveTick next) = do
  tick <- H.gets (_.time)
  H.modify_ (_ { time = tick >>= incTick })
  pure next
