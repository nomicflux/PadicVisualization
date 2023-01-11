module Component.Canvas where

import Prelude

import Color as Co
import Component.Animation (Tick, incTick, startTick)
import Component.Animation as An
import Component.Bubble as B
import Control.Monad.Reader as Reader
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as A
import Data.Array.ST (STArray)
import Data.Array.ST as AST
import Data.Int (round, toNumber, pow)
import Data.List (List)
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, fromInt)
import Data.Rational as R
import Data.Traversable (for_, sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas as Ca
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenHelpers.Coordinates (Coordinates)
import HalogenHelpers.Coordinates as C
import Math as Math
import Norm (Norm(..), getPrime, isPadic, takeNorm)
import PadicVector (baseCoordinates)
import PadicVector as PV
import PolarCoordinates (PolarCoordinates)
import PolarCoordinates as PC

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data CoordType = Circular | PadicVector

type Model = { maxInt :: Int
             , power :: Int
             , norm :: Norm
             , bubbles :: Array (List Int)
             , time :: Maybe Tick
             , size :: Int
             , numIncs :: Int
             , coordType :: CoordType
             , cache :: Map Int Coordinates
             , colorCache :: Map Int String
             , maxTick :: Int
             , animate :: Boolean
             , lines :: Boolean
             , scale :: Int
             , radius :: Int
             , addTo :: Int
             , multBy :: Int
             , quadCoeff :: Int
             , cubeCoeff :: Int
             , sqrt :: Boolean
             , cbrt :: Boolean
             }

type Input = { size :: Int
             , power :: Int
             , norm :: Norm
             , coordType :: CoordType
             , maxTick :: Int
             , scale :: Int
             , radius :: Int
             , addTo :: Int
             , multBy :: Int
             , quadCoeff :: Int
             , cubeCoeff :: Int
             , sqrt :: Boolean
             , cbrt :: Boolean
             }

mkBubbles :: Int -> Array (List Int)
mkBubbles maxInt = L.singleton <$> (A.range 0 maxInt)

initialModel :: Input -> Model
initialModel input =
  let maxInt = pow (fromMaybe 1 (getPrime input.norm)) input.power
  in { power: input.power
     , norm: input.norm
     , maxInt: maxInt
     , bubbles: mkBubbles maxInt
     , time: Nothing
     , size: input.size
     , numIncs: 0
     , coordType: input.coordType
     , cache: M.empty
     , colorCache: M.empty
     , maxTick: input.maxTick
     , animate: true
     , lines: false
     , scale: input.scale
     , radius: input.radius
     , addTo: input.addTo
     , multBy: input.multBy
     , quadCoeff: input.quadCoeff
     , cubeCoeff: input.cubeCoeff
     , sqrt: input.sqrt
     , cbrt: input.cbrt
     }

getR :: Model -> Rational -> Rational
getR model value = value / fromInt maxValue
  where
    maxValue = if isPadic model.norm then 1 else model.maxInt

getTheta :: Model -> Int -> Number
getTheta model value = Math.pi * 2.0 * (toNumber value) / (toNumber model.maxInt)

toPolar :: Model -> Int -> PolarCoordinates
toPolar model n =
  let r = getR model (takeNorm model.norm n)
      theta = getTheta model n
  in {r: R.toNumber r, theta}

normalizeCoords :: Model -> Coordinates -> Coordinates
normalizeCoords model coords =
  let offset = { top: 1.5 * toNumber model.scale
               , left: 1.5 * toNumber model.scale
               }
  in C.addOffset (C.scale (toNumber model.scale) coords) offset

getCircleCoord :: Model -> Int -> Coordinates
getCircleCoord model n =
  normalizeCoords model $ PC.polarToCartesian (toPolar model n)

getPadicCoord :: Model -> Int -> Coordinates
getPadicCoord model n =
  let p = fromMaybe 0 (getPrime model.norm)
  in normalizeCoords model $ PV.toVector model.maxInt p n

getCoordinates :: (Number -> Number -> Number) ->
                  Map Int Coordinates -> Number ->
                  Tuple Int Int -> Coordinates
getCoordinates interpolater cache size (Tuple oldValue newValue) =
  let mold = M.lookup oldValue cache
      old = fromMaybe (C.addOffset baseCoordinates {top: size / 2.0, left: size}) mold
      new = fromMaybe baseCoordinates $ M.lookup newValue cache
  in { x: interpolater new.x old.x
     , y: interpolater new.y old.y
     }

getColor :: (Number -> Number -> Number) ->
            (Int -> Int) ->
            Map Int String ->
            Tuple Int Int -> String
getColor interpolater placer cache (Tuple oldValue newValue) =
  let interpolated = round $ interpolater (toNumber newValue) (toNumber oldValue)
  in  fromMaybe "#000" $ M.lookup (placer interpolated) cache

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

drawLine :: Ca.Context2D -> Coordinates -> Coordinates -> Number -> String -> Effect Unit
drawLine ctx start end r color = do
  Ca.beginPath ctx
  Ca.moveTo ctx start.x start.y
  Ca.setStrokeStyle ctx color
  Ca.setLineWidth ctx r
  Ca.lineTo ctx end.x end.y
  Ca.stroke ctx
  Ca.closePath ctx

distributeTuple :: (Tuple Int (List Int)) -> List (Tuple Int Int)
distributeTuple (Tuple a bs) = (\b -> Tuple a b) <$> bs

drawBubble :: Ca.Context2D ->
              Model ->
              (Tuple Int Int -> Coordinates) ->
              (Tuple Int Int -> Coordinates) ->
              (Tuple Int Int -> String) ->
              Int ->
              (Tuple Int Int) ->
              Effect Unit
drawBubble ctx model oldCoordGetter coordGetter colorGetter radius bubble =
  let coords = coordGetter bubble
      startCoords = oldCoordGetter bubble
      color = colorGetter bubble
  in drawCircle ctx coords (toNumber radius) color *>
     if model.lines then drawLine ctx startCoords coords (toNumber radius) color
                    else pure unit

drawBubbles :: Ca.Context2D ->
               Model ->
              (Tuple Int Int -> Coordinates) ->
              (Tuple Int Int -> Coordinates) ->
              (Tuple Int Int -> String) ->
              Int ->
              (Tuple Int (List Int)) ->
              Effect Unit
drawBubbles ctx model oldCoordGetter coordGetter colorGetter radius bubbles =
  for_ (distributeTuple bubbles) $ drawBubble ctx model oldCoordGetter coordGetter colorGetter radius

canvasId :: String
canvasId = "bubble-canvas"

getSize :: Model -> Int
getSize model = round $ 4.0 * toNumber model.scale

render :: Model -> H.ComponentHTML Query
render model =
  let size = getSize model
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
             | ChangeQuadBy Int (Unit -> a)
             | ChangeCubeBy Int (Unit -> a)
             | ChangeSqrt Boolean (Unit -> a)
             | ChangeCbrt Boolean (Unit -> a)
             | ToggleRepr (Unit -> a)
             | ToggleLines (Unit -> a)
             | ToggleAnimation (Unit -> a)
             | InitCaches a
             | Reset (Unit -> a)
             | MoveTick (Unit -> a)

data Message = Noop

inRange :: Int -> Int -> Boolean
inRange maxInt v = v <= maxInt && v >= 0

replaceBubble :: Int -> Inc -> Int -> Int
replaceBubble maxInt inc v =
  if inRange maxInt v
  then v
  else v `mod` (maxInt + 1)

type Inc = { addTo :: Int
           , multBy :: Int
           , sqrBy :: Int
           , cubeBy :: Int
           , sqrt :: Boolean
           , cbrt :: Boolean
           }

bubbleFn :: Norm -> Int -> Inc -> Int -> (List Int)
bubbleFn norm steps inc =
  let base = L.singleton <<< B.cubeBy inc.cubeBy inc.sqrBy inc.multBy inc.addTo
      sqrted = if inc.sqrt then base >=> B.normSqrt norm steps else base
      cbrted = if inc.cbrt then sqrted >=> B.normCbrt norm steps else sqrted
  in cbrted

setFromIdx :: forall h a. STArray h (List a) -> Int -> (Int -> List a) -> ST h Unit
setFromIdx array idx f = AST.modify idx (const (f idx)) array *> pure unit

incBubble :: forall h. Model -> Inc -> STArray h (List Int) -> Int -> ST h Unit
incBubble model inc bubbles idx =
  setFromIdx bubbles idx $
    map (replaceBubble model.maxInt inc) <<< bubbleFn model.norm model.power inc

incAll :: Model -> Model
incAll model =
  let
    inc = { addTo: model.addTo
          , multBy: model.multBy
          , sqrBy: model.quadCoeff
          , cubeBy: model.cubeCoeff
          , sqrt: model.sqrt
          , cbrt: model.cbrt
          }

    changer :: forall h. STArray h (List Int) -> Int -> ST h Unit
    changer = incBubble model inc

    newBubblesST :: forall h. Array (List Int) -> ST h (Array (List Int))
    newBubblesST bubbles = do
      bubblesST <- AST.thaw bubbles
      _ <- ST.for 0 (A.length bubbles) (changer bubblesST)
      AST.freeze bubblesST
    newBubbles = ST.run (newBubblesST model.bubbles)
  in
   model { bubbles = newBubbles
         , numIncs = (model.multBy * model.numIncs + model.addTo) `mod` (model.maxInt + 1)
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
  H.modify_ incAll
  pure next

toggleAnimation :: Model -> Model
toggleAnimation model = model { animate = not model.animate }

toggleLines :: Model -> Model
toggleLines model = model { lines = not model.lines }

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
changeMax power model =
  let x = case model.norm of
        Padic p -> p
        Inf -> 1
  in model { maxInt = pow x power - 1,
             power = power }

redraw :: Model -> Effect Unit
redraw model =
  let sqrtInterp = Reader.runReader (An.sqrtInterpolate model.time) model.maxTick
      sinInterp = Reader.runReader (An.sinInterpolate model.time) model.maxTick
      alpha = sinInterp 0.6 0.15
      coordGetter = getCoordinates sqrtInterp model.cache (toNumber $ getSize model)
      oldCoordGetter = getCoordinates (\x y -> y) model.cache (toNumber $ getSize model)
      colorGetter = getColor sqrtInterp (numInPlace model) model.colorCache
  in do
    mcanvas <- Ca.getCanvasElementById canvasId
    case mcanvas of
      Nothing -> pure unit
      Just canvas -> do
        ctx <- Ca.getContext2D canvas
        dim <- Ca.getCanvasDimensions canvas
        Ca.clearRect ctx {x: 0.0, y: 0.0, width: dim.width, height: dim.height}
        Ca.setGlobalAlpha ctx alpha
        sequence_ $
          A.mapWithIndex (\idx vs -> drawBubbles ctx model oldCoordGetter coordGetter colorGetter model.radius (Tuple idx vs)) model.bubbles
        pure unit

redrawStep :: H.ComponentDSL Model Query Message Aff Unit
redrawStep = do
  model <- H.get
  H.modify_ (_ { animate = false })
  H.liftEffect (redraw model)
  H.modify_ (_ { animate = true })
  pure unit

eval :: Query ~> H.ComponentDSL Model Query Message Aff
eval (ChangeMax power reply) =
  H.modify_ (changeMax power) *> reinitCache (reply unit)
eval (ChangeNorm norm reply) = H.modify_ (_ { norm = norm }) *> reinitCache (reply unit)
eval (ChangeTick tick reply) =
  H.modify_ (_ { maxTick = tick } ) *> pure (reply unit)
eval (ChangeAddTo addTo reply) =
  H.modify_ (_ { addTo = addTo } ) *> reinitCache (reply unit)
eval (ChangeMultBy multBy reply) =
  H.modify_ (_ { multBy = multBy } ) *> reinitCache (reply unit)
eval (ChangeQuadBy quadCoeff reply) =
  H.modify_ (_ { quadCoeff = quadCoeff } ) *> reinitCache (reply unit)
eval (ChangeCubeBy cubeCoeff reply) =
  H.modify_ (_ { cubeCoeff = cubeCoeff } ) *> reinitCache (reply unit)
eval (ChangeSqrt b reply) =
  H.modify_ (_ { sqrt = b }) *> reinitCache (reply unit)
eval (ChangeCbrt b reply) =
  H.modify_ (_ { cbrt = b }) *> reinitCache (reply unit)
eval (ChangeScale scale reply) =
  H.modify_ (_ { scale = scale } ) *> reinitCache (reply unit)
eval (ChangeRadius radius reply) =
  H.modify_ (_ { radius = radius } ) *> pure (reply unit)
eval (ToggleRepr reply) = H.modify_ toggleRepr *> reinitCache (reply unit)
eval (ToggleLines reply) = H.modify_ toggleLines *> pure (reply unit)
eval (ToggleAnimation reply) = H.modify_ toggleAnimation *> reinitCache (reply unit)
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
          redrawStep
          pure (reply unit)
        Nothing -> do
          H.modify_ (_ { time = Just startTick })
          redrawStep
          pure (reply unit)
eval (InitCaches next) = reinitCache next
eval (Reset reply) = do
  model <- H.get
  H.modify_ (changeMax model.power)
  reinitCache (reply unit)
