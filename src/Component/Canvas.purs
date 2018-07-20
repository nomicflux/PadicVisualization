module Component.Canvas where

import Prelude

import Color as C
import Component.Animation (Tick, incTick, startTick)
import Component.Animation as An
import Component.Bubble (Bubble)
import Component.Bubble as B
import Data.Array as A
import Data.Int (round, toNumber)
import Data.List.Lazy (List)
import Data.List.Lazy as L
import Data.Maybe (Maybe(..))
import Data.Rational (Rational, fromInt)
import Data.Rational as R
import Debug.Trace as D
import Halogen as H
import Halogen.HTML as HH
import HalogenHelpers.Coordinates (Coordinates, addOffset)
import HalogenHelpers.SVG as SVG
import Math as Math
import Norm (Norm, isPadic)

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type Model = { maxInt :: Int
             , norm :: Norm
             , bubbles :: List Bubble
             , time :: Maybe Tick
             , size :: Int
             , windingNumber :: Int
             , numIncs :: Int
             }

type Input = { size :: Int
             , maxInt :: Int
             , windingNumber :: Int
             , norm :: Norm
             }

initialModel :: Input -> Model
initialModel input = { maxInt: input.maxInt
                     , norm: input.norm
                     , bubbles: B.mkBubble <$> (L.range 0 input.maxInt)
                     , time: Nothing
                     , size: input.size
                     , windingNumber: input.windingNumber
                     , numIncs: 0
                     }

scale :: Int
scale = 100

maxValue :: Model -> Int
maxValue model =
  if isPadic model.norm then 1 else model.maxInt

getR :: Model -> Rational -> Rational
getR model value = value / fromInt (maxValue model)

getTheta :: Model -> Int -> Number
getTheta model value = Math.pi * 2.0 * (toNumber value) / (toNumber model.windingNumber)

polarToCartesian :: Rational -> Number -> Coordinates
polarToCartesian r theta =
  let r' = R.toNumber r
  in { x: toNumber scale * r' * Math.cos theta
     , y: toNumber scale * r' * Math.sin theta
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
  let offset = { top: toNumber (2 * scale)
               , left: toNumber (2 * scale)
               }
      mold = (flip addOffset offset) <$> getOldCoordinates model bubble
      new = addOffset (getNewCoordinates model bubble) offset
  in case mold of
    Nothing -> new
    Just old ->
      let interpolater = An.linInterpolate model.time
      in { x: interpolater new.x old.x
         , y: interpolater new.y old.y
         }

component :: forall m. H.Component HH.HTML Query Input Message m
component = H.component { initialState: initialModel
                        , render
                        , eval
                        , receiver: const Nothing
                        }

numInPlace :: Model -> Int -> Int
numInPlace model x = (x - model.numIncs) `mod` (model.maxInt + 1)

renderBubble :: Model -> Bubble -> H.ComponentHTML Query
renderBubble model bubble =
  let coords = getCoordinates model bubble
      hue = 360.0 * toNumber (numInPlace model $ B.getValue bubble) / (toNumber model.maxInt)
      alpha = 0.5 * An.proportionalTick model.time
      color = C.toHexString (C.hsva hue 1.0 (alpha + 0.5) alpha)
  in
   SVG.circle [ SVG.cx $ round coords.x
              , SVG.cy $ round coords.y
              , SVG.r 1
              , SVG.stroke color
              , SVG.fill color
              ]

render :: Model -> H.ComponentHTML Query
render model =
  let dblSizeStr = show $ 4 * scale
  in
   HH.div_ [ SVG.svg [ SVG.width model.size
                     , SVG.height model.size
                     , SVG.viewBox $ A.intercalate " " [ "0"
                                                       , "0"
                                                       , dblSizeStr
                                                       , dblSizeStr
                                                       ]
                     ]
             (renderBubble model <$> A.fromFoldable model.bubbles) ]

data Query a = ChangeMax Int (Unit -> a)
             | ChangeWinding Int (Unit -> a)
             | ChangeNorm Norm (Unit -> a)
             | IncValues a
             | DecValues a
             | MoveTick (Unit -> a)

data Message = Noop

filterBubbles :: Model -> Model
filterBubbles model =
  let newBubbles = L.filter (\b -> let v = B.getValue b in v <= model.maxInt && v >= 0) model.bubbles
  in model { bubbles = newBubbles }

addBubble :: Bubble -> Model -> Model
addBubble bubble model =
  model { bubbles = L.cons bubble model.bubbles }

incBubbles :: Model -> Model
incBubbles model =
  model { bubbles = B.incValue <$> model.bubbles
        , numIncs = model.numIncs + 1
        }

decBubbles :: Model -> Model
decBubbles model =
  model { bubbles = B.decValue <$> model.bubbles
        , numIncs = model.numIncs - 1
        }

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
eval (MoveTick reply) = do
  tick <- H.gets (_.time)
  let newTick = tick >>= incTick
  case newTick of
    Just _ -> do
      H.modify_ (_ { time = newTick })
      pure (reply unit)
    Nothing -> do
      H.modify_ (_ { time = Just startTick })
      eval (IncValues (reply unit))
