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
import Data.Int (round, toNumber)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, fromInt)
import Data.Rational as R
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas as Ca
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenHelpers.Coordinates (Coordinates)
import HalogenHelpers.Coordinates as C
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
             , numIncs :: Int
             , coordType :: CoordType
             , cache :: Map Int Coordinates
             , colorCache :: Map Int String
             , maxTick :: Int
             , animate :: Boolean
             , scale :: Int
             , radius :: Int
             , addTo :: Int
             , multBy :: Int
             }

type Input = { size :: Int
             , maxInt :: Int
             , norm :: Norm
             , coordType :: CoordType
             , maxTick :: Int
             , scale :: Int
             , radius :: Int
             , addTo :: Int
             , multBy :: Int
             }

mkBubbles :: Int -> Array Bubble
mkBubbles maxInt = B.mkBubble <$> (A.range 0 maxInt)

initialModel :: Input -> Model
initialModel input = { maxInt: input.maxInt
                     , norm: input.norm
                     , bubbles: mkBubbles input.maxInt
                     , time: Nothing
                     , size: input.size
                     , numIncs: 0
                     , coordType: input.coordType
                     , cache: M.empty
                     , colorCache: M.empty
                     , maxTick: input.maxTick
                     , animate: true
                     , scale: input.scale
                     , radius: input.radius
                     , addTo: input.addTo
                     , multBy: input.multBy
                     }

maxValue :: Model -> Int
maxValue model =
  if isPadic model.norm then 1 else model.maxInt

getR :: Model -> Rational -> Rational
getR model value = value / fromInt (maxValue model)

getTheta :: Model -> Int -> Number
getTheta model value = Math.pi * 2.0 * (toNumber value) / (toNumber model.maxInt)

toPolar :: Model -> Bubble -> PolarCoordinates
toPolar model bubble =
  let r = getR model (B.getNormedValue model.norm bubble)
      theta = getTheta model (B.getValue bubble)
  in {r: R.toNumber r, theta}

normalizeCoords :: Model -> Coordinates -> Coordinates
normalizeCoords model coords =
  let offset = { top: 1.5 * toNumber model.scale
               , left: 1.5 * toNumber model.scale
               }
  in C.addOffset (C.scale (toNumber model.scale) coords) offset

getCircleCoord :: Model -> Int -> Coordinates
getCircleCoord model n =
  let b = B.mkBubble n
  in normalizeCoords model $ PC.polarToCartesian (toPolar model b)

getPadicCoord :: Model -> Int -> Coordinates
getPadicCoord model n =
  let b = B.mkBubble n
      p = fromMaybe 0 (getPrime model.norm)
  in normalizeCoords model $ PV.toVector model.maxInt p (B.getValue b)

getCoordinates :: (Number -> Number -> Number) ->
                  Map Int Coordinates -> Bubble -> Coordinates
getCoordinates interpolater cache bubble =
  let mold = B.getOldValue bubble >>= flip M.lookup cache
      new = fromMaybe baseCoordinates $ M.lookup (B.getValue bubble) cache
  in case mold of
    Nothing -> new
    Just old ->
      { x: interpolater new.x old.x
      , y: interpolater new.y old.y
      }

component :: H.Component HH.HTML Query Input Message Aff
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

mkColor :: Model -> Int -> String
mkColor model value =
  let p = fromMaybe 1 $ getPrime model.norm
      divisor = case model.coordType of
        Circular -> p
        PadicVector -> 1
      step = 360.0 / toNumber ((model.maxInt + 1) / divisor)
      hue = step * toNumber value
  in Co.toHexString (Co.hsv hue 1.0 1.0)

drawCircle :: Ca.Context2D -> Coordinates -> Number -> String -> Effect Unit
drawCircle ctx coords r color = do
  Ca.beginPath ctx
  Ca.arc ctx { x: coords.x, y: coords.y, radius: r, start: 0.0, end: 2.0 * Math.pi }
  Ca.setFillStyle ctx color
  Ca.fill ctx
  Ca.setStrokeStyle ctx color
  Ca.stroke ctx
  Ca.closePath ctx

drawBubble :: Ca.Context2D ->
              (Bubble -> Coordinates) ->
              (Int -> String) ->
              Int ->
              Bubble ->
              Effect Unit
drawBubble ctx coordGetter colorGetter radius bubble =
  let coords = coordGetter bubble
      b = B.getValue bubble
      color = colorGetter b
  in drawCircle ctx coords (toNumber radius) color

canvasId :: String
canvasId = "bubble-canvas"

render :: Model -> H.ComponentHTML Query
render model =
  let size = round $ 4.0 * toNumber model.scale
  in
   HH.canvas [ HP.width size
             , HP.height size
             , HP.id_ canvasId
             ]

data Query a = ChangeMax Int (Unit -> a)
             | ChangeTick Int (Unit -> a)
             | ChangeNorm Norm (Unit -> a)
             | ChangeScale Int (Unit -> a)
             | ChangeRadius Int (Unit -> a)
             | ChangeAddTo Int (Unit -> a)
             | ChangeMultBy Int (Unit -> a)
             | ToggleRepr (Unit -> a)
             | ToggleAnimation (Unit -> a)
             | IncValues a
             | InitCaches a
             | MoveTick (Unit -> a)

data Message = Noop

replaceBubble :: Int -> Bubble -> Bubble
replaceBubble maxInt bubble =
  let v = B.getValue bubble
  in if v <= maxInt && v >= 0
     then bubble
     else B.mkBubble (v `mod` (maxInt + 1))

type Inc = { addTo :: Int
           , multBy :: Int
           }

incBubble :: forall h. Int -> Inc -> STArray h Bubble -> Int -> ST h Unit
incBubble maxInt inc bubbles idx = do
  _ <- AST.modify idx (replaceBubble maxInt <<< B.incValueBy inc.addTo <<< B.multValueBy inc.multBy) bubbles
  pure unit

incAll :: Model -> Model
incAll model =
  let
    changer :: forall h. STArray h Bubble -> Int -> ST h Unit
    changer = incBubble model.maxInt {addTo: model.addTo, multBy: model.multBy}

    newBubblesST :: forall h. Array Bubble -> ST h (Array Bubble)
    newBubblesST bubbles = do
      bubblesST <- AST.thaw bubbles
      _ <- ST.for 0 (A.length bubbles) (changer bubblesST)
      AST.freeze bubblesST
    newBubbles = ST.run (newBubblesST model.bubbles)
  in
   model { bubbles = newBubbles
         , numIncs = model.numIncs + 1
         }

reinitCache :: forall a. a -> H.ComponentDSL Model Query Message Aff a
reinitCache next =  do
  model <- H.get
  H.modify_ regenBubbles
  let ints = A.range 0 model.maxInt
      emptyCache :: Map Int Coordinates
      emptyCache = M.empty

      cache = case model.coordType of
        Circular ->
          A.foldl (\acc x -> M.insert x (getCircleCoord model x) acc) emptyCache ints
        PadicVector ->
          A.foldl (\acc x -> M.insert x (getPadicCoord model x) acc) emptyCache ints

      colorCache = A.foldl (\acc x -> M.insert x (mkColor model x) acc) M.empty ints
  H.modify_ (_ { cache = cache
               , colorCache = colorCache
               })
  H.liftEffect $ redraw model
  pure next

toggleAnimation :: Model -> Model
toggleAnimation model = model { animate = not model.animate }

toggleRepr :: Model -> Model
toggleRepr model =
  let newRepr = case model.coordType of
        PadicVector -> Circular
        Circular -> PadicVector
  in model { coordType = newRepr }

regenBubbles :: Model -> Model
regenBubbles model = model { bubbles = mkBubbles model.maxInt
                           , numIncs = 0
                           }


changeMax :: Int -> Model -> Model
changeMax maxInt model = model { maxInt = maxInt }

redraw :: Model -> Effect Unit
redraw model =
  let propTick = Reader.runReader (An.proportionalTick model.time) model.maxTick
      alpha = 0.2 + 0.7 * (square $ Math.cos $ Math.pi * propTick)
      interpolater = Reader.runReader (An.sqrtInterpolate model.time) model.maxTick
      coordGetter = getCoordinates interpolater model.cache
      colorGetter v = fromMaybe "#000" $ M.lookup (numInPlace model v) model.colorCache
  in do
    mcanvas <- Ca.getCanvasElementById canvasId
    case mcanvas of
      Nothing -> pure unit
      Just canvas -> do
        ctx <- Ca.getContext2D canvas
        dim <- Ca.getCanvasDimensions canvas
        Ca.clearRect ctx {x: 0.0, y: 0.0, width: dim.width, height: dim.height}
        Ca.setGlobalAlpha ctx alpha
        for_ model.bubbles (drawBubble ctx coordGetter colorGetter model.radius)
        pure unit

eval :: Query ~> H.ComponentDSL Model Query Message Aff
eval (ChangeMax max reply) = H.modify_ (changeMax max) *> reinitCache (reply unit)
eval (ChangeNorm norm reply) = H.modify_ (_ { norm = norm }) *> reinitCache (reply unit)
eval (ChangeTick tick reply) =
  H.modify_ (_ { maxTick = tick } ) *> pure (reply unit)
eval (ChangeAddTo addTo reply) =
  H.modify_ (_ { addTo = addTo } ) *> reinitCache (reply unit)
eval (ChangeMultBy multBy reply) =
  H.modify_ (_ { multBy = multBy } ) *> reinitCache (reply unit)
eval (ChangeScale scale reply) =
  H.modify_ (_ { scale = scale } ) *> reinitCache (reply unit)
eval (ChangeRadius radius reply) =
  H.modify_ (_ { radius = radius } ) *> reinitCache (reply unit)
eval (ToggleRepr reply) = H.modify_ toggleRepr *> reinitCache (reply unit)
eval (ToggleAnimation reply) = H.modify_ toggleAnimation *> reinitCache (reply unit)
eval (IncValues next) = do
  model <- H.get
  H.modify_ incAll
  pure next
eval (MoveTick reply) = do
  model <- H.get
  case model.animate of
    false -> do
      H.liftEffect $ redraw model
      pure (reply unit)
    true -> do
      let newTick = model.time >>= \t -> Reader.runReader (incTick t) model.maxTick
      case newTick of
        Just _ -> do
          H.modify_ (_ { time = newTick })
          H.liftEffect $ redraw model
          pure (reply unit)
        Nothing -> do
          H.modify_ (_ { time = Just startTick })
          H.liftEffect $ redraw model
          eval (IncValues (reply unit))
eval (InitCaches next) =
  reinitCache next
