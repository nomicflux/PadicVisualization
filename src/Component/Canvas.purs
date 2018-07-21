module Component.Canvas where

import Prelude

import Color as Co
import Component.Animation (Tick, incTick, startTick)
import Component.Animation as An
import Component.Bubble (Bubble)
import Component.Bubble as B
import Control.Monad.Reader as Reader
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as A
import Data.Array.ST (STArray)
import Data.Array.ST as AST
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Rational (Rational, fromInt)
import Data.Rational as R
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import HalogenHelpers.Coordinates (Coordinates)
import HalogenHelpers.Coordinates as C
import HalogenHelpers.SVG as SVG
import Math as Math
import Norm (Norm, getPrime, isPadic)
import PadicVector (baseCoordinates)
import PadicVector as PV
import PolarCoordinates (PolarCoordinates)
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
             , circleCache :: Map Int Coordinates
             , padicCache :: Map Int Coordinates
             , maxTick :: Int
             }

type Input = { size :: Int
             , maxInt :: Int
             , windingNumber :: Int
             , norm :: Norm
             , coordType :: CoordType
             , maxTick :: Int
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
                     , circleCache: M.empty
                     , padicCache: M.empty
                     , maxTick: input.maxTick
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

toPolar :: Model -> Bubble -> PolarCoordinates
toPolar model bubble =
  let r = getR model (B.getNormedValue model.norm bubble)
      theta = getTheta model (B.getValue bubble)
  in {r: R.toNumber r, theta}

normalizeCoords :: Coordinates -> Coordinates
normalizeCoords coords =
  let offset = { top: toNumber (2 * scale)
               , left: toNumber (2 * scale)
               }
  in C.addOffset (C.scale (toNumber scale) coords) offset

getCircleCoord :: Model -> Int -> Coordinates
getCircleCoord model n =
  let b = B.mkBubble n
  in normalizeCoords $ PC.polarToCartesian $ (toPolar model b)

getPadicCoord :: Model -> Int -> Coordinates
getPadicCoord model n =
  let b = B.mkBubble n
      p = fromMaybe 0 (getPrime model.norm)
  in normalizeCoords $ PV.toVector p $ B.getValue b

getCoordinates :: Map Int Coordinates -> Int -> Maybe Tick -> Bubble -> Coordinates
getCoordinates cache maxTick time bubble =
  let mold = B.getOldValue bubble >>= \x -> M.lookup x cache
      new = fromMaybe baseCoordinates $ M.lookup (B.getValue bubble) cache
  in case mold of
    Nothing -> new
    Just old ->
      let interpolater n o = Reader.runReader (An.linInterpolate time n o) maxTick
      in { x: interpolater new.x old.x
         , y: interpolater new.y old.y
         }

component :: forall m. H.Component HH.HTML Query Input Message m
component = H.lifecycleComponent { initialState: initialModel
                                 , render
                                 , eval
                                 , receiver: const Nothing
                                 , initializer: Just $ H.action InitCaches
                                 , finalizer: Nothing
                                 }

numInPlace :: Model -> Int -> Int
numInPlace model x = (x - model.numIncs) `mod` (model.maxInt + 1)

square :: Number -> Number
square n = n * n

renderBubble :: Number -> Model -> Bubble -> Tuple String (H.ComponentHTML Query)
renderBubble alpha model bubble =
  let cache = case model.coordType of
        Circular -> model.circleCache
        PadicVector -> model.padicCache
      coords = getCoordinates cache model.maxTick model.time bubble
      hue = case model.coordType of
        Circular -> (360.0 * 45.0 * toNumber (numInPlace model $ B.getValue bubble) / (toNumber model.maxInt))
        PadicVector -> (360.0 * 45.0 * toNumber (B.getValue bubble) / (toNumber model.maxInt))
      color = Co.toHexString (Co.hsv hue 1.0 1.0)
      key = A.intercalate "-" [ show $ B.getValue bubble ]
  in
   Tuple key $ SVG.circle [ SVG.cx coords.x
                          , SVG.cy coords.y
                          , SVG.r 1
                          , SVG.stroke color
                          , SVG.fill color
                          , SVG.opacity alpha
                          ]

render :: Model -> H.ComponentHTML Query
render model =
  let dblSizeStr = show $ 4 * scale
      propTick = Reader.runReader (An.proportionalTick model.time) model.maxTick
      alpha = 0.2 + 0.8 * (square $ Math.cos $ Math.pi * propTick)
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
             | InitCaches a
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
  maxTick <- H.gets (_.maxTick)
  tick <- H.gets (_.time)
  let newTick = tick >>= \t -> Reader.runReader (incTick t) maxTick
  case newTick of
    Just _ -> do
      H.modify_ (_ { time = newTick })
      pure (reply unit)
    Nothing -> do
      H.modify_ (_ { time = Just startTick })
      eval (IncValues (reply unit))
eval (InitCaches next) = do
  model <- H.get
  let ints = A.range 0 model.maxInt
      emptyCache :: Map Int Coordinates
      emptyCache = M.empty

      circles :: Map Int Coordinates
      circles = A.foldl (\acc x -> M.insert x (getCircleCoord model x) acc) emptyCache ints
      padics :: Map Int Coordinates
      padics = A.foldl (\acc x -> M.insert x (getPadicCoord model x) acc) emptyCache ints
  H.modify_ (_ { circleCache = circles
               , padicCache = padics
               })
  pure next
