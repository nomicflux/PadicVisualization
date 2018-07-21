module Component.Canvas where

import Prelude

import Color as Co
import Component.Animation (Tick, incTick, startTick)
import Component.Animation as An
import Component.Bubble (Bubble)
import Component.Bubble as B
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as A
import Data.Array.ST (STArray)
import Data.Array.ST as AST
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, fromInt)
import Data.Rational as R
import Halogen as H
import Halogen.HTML as HH
import HalogenHelpers.Coordinates (Coordinates)
import HalogenHelpers.Coordinates as C
import HalogenHelpers.SVG as SVG
import Math as Math
import Norm (Norm, getPrime, isPadic)
import PadicVector as PV
import PolarCoordinates (PolarCoordinates, mkPolar)
import PolarCoordinates as PC

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data CoordType = Circular | PadicVector

type Model = { maxInt :: Int
             , norm :: Norm
             , bubbles :: Array Bubble
             , time :: Maybe Tick
             , size :: Int
             , windingNumber :: Int
             , numIncs :: Int
             , coordType :: CoordType
             }

type Input = { size :: Int
             , maxInt :: Int
             , windingNumber :: Int
             , norm :: Norm
             , coordType :: CoordType
             }

initialModel :: Input -> Model
initialModel input = { maxInt: input.maxInt
                     , norm: input.norm
                     , bubbles: B.mkBubble <$> (A.range 0 input.maxInt)
                     , time: Nothing
                     , size: input.size
                     , windingNumber: input.windingNumber
                     , numIncs: 0
                     , coordType: input.coordType
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

getNewCoordinates :: Model -> Bubble -> PolarCoordinates
getNewCoordinates model bubble =
  let r = getR model (B.getNormedValue model.norm bubble)
      theta = getTheta model (B.getValue bubble)
  in {r: R.toNumber r, theta}

getOldCoordinates :: Model -> Bubble -> Maybe PolarCoordinates
getOldCoordinates model bubble =
  let mr = (R.toNumber <<< getR model) <$> (B.getNormedOldValue model.norm bubble)
      mtheta = getTheta model <$> (B.getOldValue bubble)
  in mkPolar <$> mr <*> mtheta

normalizeCoords :: Coordinates -> Coordinates
normalizeCoords coords =
  let offset = { top: toNumber (2 * scale)
               , left: toNumber (2 * scale)
               }
  in C.addOffset (C.scale (toNumber scale) coords) offset

getCircleCoordinates :: Model -> Bubble -> Coordinates
getCircleCoordinates model bubble =
  let mold = (normalizeCoords <<< PC.polarToCartesian) <$> getOldCoordinates model bubble
      new = normalizeCoords $ PC.polarToCartesian $ (getNewCoordinates model bubble)
  in case mold of
    Nothing -> new
    Just old ->
      let interpolater = An.linInterpolate model.time
      in { x: interpolater new.x old.x
         , y: interpolater new.y old.y
         }

getPadicVectorCoordinates :: Model -> Bubble -> Coordinates
getPadicVectorCoordinates model bubble =
  let p = fromMaybe 0 (getPrime model.norm)
      mold = (normalizeCoords <<< PV.toVector p) <$> B.getOldValue bubble
      new = normalizeCoords $ PV.toVector p $ B.getValue bubble
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

square :: Number -> Number
square n = n * n

renderBubble :: Number -> Model -> Bubble -> H.ComponentHTML Query
renderBubble alpha model bubble =
  let coords = case model.coordType of
        Circular -> getCircleCoordinates model bubble
        PadicVector -> getPadicVectorCoordinates model bubble
      hue = case model.coordType of
        Circular -> (360.0 * 45.0 * toNumber (numInPlace model $ B.getValue bubble) / (toNumber model.maxInt))
        PadicVector -> (360.0 * 45.0 * toNumber (B.getValue bubble) / (toNumber model.maxInt))
      color = Co.toHexString (Co.hsv hue 1.0 1.0)
  in
   SVG.circle [ SVG.cx coords.x
              , SVG.cy coords.y
              , SVG.r 1
              , SVG.stroke color
              , SVG.fill color
              , SVG.opacity alpha
              ]

render :: Model -> H.ComponentHTML Query
render model =
  let dblSizeStr = show $ 4 * scale
      alpha = 0.2 + 0.8 * (square $ Math.cos $ Math.pi * An.proportionalTick model.time)
  in
   HH.div_ [ SVG.svg [ SVG.width model.size
                     , SVG.height model.size
                     , SVG.viewBox $ A.intercalate " " [ "0"
                                                       , "0"
                                                       , dblSizeStr
                                                       , dblSizeStr
                                                       ]
                     ]
             (renderBubble alpha model <$> model.bubbles) ]

data Query a = ChangeMax Int (Unit -> a)
             | ChangeWinding Int (Unit -> a)
             | ChangeNorm Norm (Unit -> a)
             | IncValues a
             | DecValues a
             | MoveTick (Unit -> a)

data Message = Noop

replaceBubble :: Int -> Bubble -> Bubble -> Bubble
replaceBubble maxInt def bubble =
  let v = B.getValue bubble
  in if v <= maxInt && v >= 0
     then bubble
     else def

incBubble :: forall h. Int -> Bubble -> STArray h Bubble -> Int -> ST h Unit
incBubble maxInt def bubbles idx = do
  _ <- AST.modify idx (replaceBubble maxInt def <<< B.incValue) bubbles
  pure unit

incAll :: Model -> Model
incAll model =
  let
    def = B.mkBubble 0

    newBubblesST :: forall h. Array Bubble -> ST h (Array Bubble)
    newBubblesST bubbles = do
      bubblesST <- AST.thaw bubbles
      _ <- ST.for 0 (A.length bubbles) (incBubble model.maxInt def bubblesST)
      AST.freeze bubblesST
    newBubbles = ST.run (newBubblesST model.bubbles)
  in
   model { bubbles = newBubbles
         , numIncs = model.numIncs + 1
         }

decBubble :: forall h. Int -> Bubble -> STArray h Bubble -> Int -> ST h Unit
decBubble maxInt def bubbles idx = do
  _ <- AST.modify idx B.decValue bubbles
  _ <- AST.modify idx (replaceBubble maxInt def) bubbles
  pure unit

decAll :: Model -> Model
decAll model =
  let
    def = B.mkBubble model.maxInt

    newBubblesST :: forall h. Array Bubble -> ST h (Array Bubble)
    newBubblesST bubbles = do
      bubblesST <- AST.thaw bubbles
      _ <- ST.for 0 (A.length bubbles) (decBubble model.maxInt def bubblesST)
      AST.freeze bubblesST
    newBubbles = ST.run (newBubblesST model.bubbles)
  in
   model { bubbles = newBubbles
         , numIncs = model.numIncs - 1
         }

eval :: forall m. Query ~> H.ComponentDSL Model Query Message m
eval (ChangeMax max reply) = H.modify_ (_ { maxInt = max }) *> pure (reply unit)
eval (ChangeWinding winding reply) = H.modify_ (_ { windingNumber = winding }) *> pure (reply unit)
eval (ChangeNorm norm reply) = H.modify_ (_ { norm = norm }) *> pure (reply unit)
eval (IncValues next) = do
  model <- H.get
  H.modify_ incAll
  pure next
eval (DecValues next) = do
  model <- H.get
  H.modify_ decAll
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
