module Component.Canvas where

import Prelude

import Color as Co
import Component.Animation (Tick(..), incTick, startTick)
import Component.Animation as An
import Component.Bubble as B
import Control.Monad.Reader as Reader
import Data.Array as A
import Data.Int (round, toNumber, pow)
import Data.List (List(..), filter, (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Rational (Rational, fromInt)
import Data.Rational as R
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect as Eff
import Effect.Aff (Aff)
import Graphics.Canvas as Ca
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenHelpers.Coordinates (Coordinates)
import HalogenHelpers.Coordinates as C
import Norm (Norm(..), getPrime, isPadic, takeNorm)
import PadicVector as PV
import PolarCoordinates (PolarCoordinates)
import PolarCoordinates as PC

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data CoordType = Circular | PadicVector

type Model = { maxInt :: Int
             , maxValue :: Int
             , power :: Int
             , norm :: Norm
             , bubbles :: Array (List Int)
             , time :: Maybe Tick
             , size :: Int
             , coordType :: CoordType
             , cache :: Map Int Coordinates
             , drawnBuffers :: Set Tick
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
             , circuit :: Set Int
             , trajectoryFrom :: Maybe Int
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
             ,sqrt :: Boolean
             , cbrt :: Boolean
             , trajectoryFrom :: Maybe Int
             }

mkBubbles :: Int -> Array (List Int)
mkBubbles maxInt = L.singleton <$> (A.range 0 maxInt)

initialModel :: Input -> Model
initialModel input =
  let maxInt = pow (fromMaybe 1 (getPrime input.norm)) input.power
  in { power: input.power
     , norm: input.norm
     , maxInt: maxInt
     , maxValue: maxInt
     , bubbles: mkBubbles maxInt
     , time: Nothing
     , size: input.size
     , coordType: input.coordType
     , cache: M.empty
     , drawnBuffers: S.empty
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
     , circuit: fromMaybe S.empty (S.singleton <$> input.trajectoryFrom)
     , trajectoryFrom: input.trajectoryFrom
     }

getR :: Model -> Rational -> Rational
getR model value = value / fromInt maxValue
  where
    maxValue = if isPadic model.norm then 1 else model.maxValue

getTheta :: Model -> Int -> Number
getTheta model value = Number.tau * (toNumber (value `mod` model.maxInt)) / (toNumber model.maxInt)

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
  in normalizeCoords model $ PV.toVector model.maxValue p n

getCoord :: Model -> Int -> Coordinates
getCoord model x = case model.coordType of
    Circular -> getCircleCoord model x
    PadicVector -> getPadicCoord model x

getCoordinates :: (Number -> Number -> Number) ->
                  Map Int Coordinates ->
                  (Int -> Coordinates) ->
                  Tuple Int Int -> Coordinates
getCoordinates interpolater cache calcer (Tuple oldValue newValue) =
  let mold = M.lookup oldValue cache
      old = fromMaybe (calcer oldValue) mold
      new = fromMaybe (calcer newValue) $ M.lookup newValue cache
  in { x: interpolater new.x old.x
     , y: interpolater new.y old.y
     }

getColor :: (Number -> Number -> Number) ->
            Model ->
           Tuple Int Int -> String
getColor interpolater model (Tuple oldValue newValue) =
  mkColor model $ interpolater (toNumber oldValue) (toNumber newValue)

component :: forall output. H.Component Query Input output Aff
component = H.mkComponent { initialState: initialModel
                          , render
                          , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                                           , handleQuery = handleQuery
                                                           , initialize = Just InitCaches
                                                           }
                          }

square :: Number -> Number
square n = n * n

mkColor :: Model -> Number -> String
mkColor model value =
  let scale = (toNumber $ model.maxValue + 1)
      base =  Number.atan ( Number.log $ value / scale )
      sat = 1.0 - ( base * 0.4 + 0.6 )
      val = 1.0 - ( base * 0.6 + 0.4 )
      hue = 360.0 * base
  in Co.toHexString (Co.hsv hue sat val)

drawCircle :: Ca.Context2D -> Number -> String -> Coordinates -> Effect Unit
drawCircle ctx r color coords = 
  Ca.beginPath ctx <*
  Ca.moveTo ctx coords.x coords.y <*
  Ca.arc ctx { x: coords.x, y: coords.y
             , radius: r, useCounterClockwise: true
             , start: 0.0, end: 2.0 * Number.pi } <*
  Ca.setFillStyle ctx color <*
  Ca.fill ctx <*
  Ca.closePath ctx <*
  pure unit

foreign import setStrokeGradientStyle :: Ca.Context2D -> Ca.CanvasGradient -> Effect Unit

drawLine :: Ca.Context2D -> Coordinates -> Coordinates -> Number ->
            String -> String -> Effect Unit
drawLine ctx start end r startColor endColor = do
  gradient <- Ca.createLinearGradient ctx { x0: start.x
                                          , y0: start.y
                                          , x1: end.x
                                          , y1: end.y }
  Ca.addColorStop gradient 0.0 startColor
  Ca.addColorStop gradient 1.0 endColor
  setStrokeGradientStyle ctx gradient
  Ca.setLineWidth ctx r

  Ca.beginPath ctx
  Ca.moveTo ctx start.x start.y
  Ca.lineTo ctx end.x end.y
  Ca.stroke ctx
  Ca.closePath ctx
  pure unit

distributeTuple :: (Tuple Int (List Int)) -> List (Tuple Int Int)
distributeTuple (Tuple a bs) = (\b -> Tuple a b) <$> bs

drawBubble :: Ca.Context2D ->
              Model ->
              (Tuple Int Int -> Coordinates) ->
              (Tuple Int Int -> Coordinates) ->
              Number ->
              (String -> Coordinates -> Effect Unit) ->
              (Tuple Int Int) ->
              Effect Unit
drawBubble ctx model oldCoordGetter coordGetter radius drawFn (Tuple oldVal newVal) =
  let bubble = Tuple oldVal (if newVal < 0 then newVal `mod` model.maxValue else newVal)
      coords = coordGetter bubble
      startCoords = oldCoordGetter bubble
      linInterp = Reader.runReader (An.linInterpolate model.time) model.maxTick
      constInterp = Reader.runReader (An.constInterpolate model.time) model.maxTick
      color = getColor linInterp model bubble
      startColor = getColor constInterp model bubble
      (Tuple origin _) = bubble
  in drawFn color coords *>
     (if model.lines || S.member origin model.circuit
      then drawLine ctx startCoords coords radius startColor color
      else pure unit)

drawBubbles :: Ca.Context2D ->
               Model ->
              (Tuple Int Int -> Coordinates) ->
              (Tuple Int Int -> Coordinates) ->
              Int ->
              Array (List (Tuple Int Int)) ->
              Effect Unit
drawBubbles ctx model oldCoordGetter coordGetter radius bubbles =
  let drawFn = drawCircle ctx (toNumber radius)
  in Eff.foreachE bubbles $ \bubbleList ->
    Eff.foreachE (A.fromFoldable bubbleList) $
    drawBubble ctx 
      model 
      oldCoordGetter 
      coordGetter 
      (toNumber radius) 
      drawFn

getSize :: Model -> Int
getSize model = round $ 4.0 * toNumber model.scale

bufferCanvas :: forall action slots m. Int -> Tick -> Int -> HH.ComponentHTML action slots m
bufferCanvas size (Tick t) i = HH.canvas [ HP.id ( canvasForTick $ Tick i )
                                         , HP.width size
                                         , HP.height size
                                         , HP.class_ $ HH.ClassName $ "buffer-canvas-" <> if t == i then "show" else "hide"
                                         ]

render :: forall action slots m. Model -> H.ComponentHTML action slots m
render model =
  let size = getSize model
  in
   HH.div [ HP.id "animation-station" ]
   ( map (bufferCanvas size (fromMaybe (Tick 0) model.time)) $ A.range 0 model.maxInt )

data Query a = ReceiveAction Action a

data Action = ChangeMax Int
            | ChangeTick Int
            | ChangeNorm Norm
            | ChangeScale Int
            | ChangeRadius Int
            | ChangeAddTo Int
            | ChangeMultBy Int
            | ChangeQuadBy Int
            | ChangeCubeBy Int
            | ChangeSqrt Boolean
            | ChangeCbrt Boolean
            | TrajectoryFrom (Maybe Int)
            | ToggleRepr
            | ToggleLines
            | ToggleAnimation
            | InitCaches
            | Reset
            | MoveTick

inRange :: Int -> Int -> Boolean
inRange maxInt v = v <= (pow (maxInt + 1) 3) && v >= 0

replaceBubble :: Int -> Int -> Int
replaceBubble maxInt v =
  if inRange maxInt v
  then v
  else v `mod` (pow (maxInt + 1) 3)

bubbleFn :: Model -> Int -> (List Int)
bubbleFn model =
  let base = L.singleton <<< B.cubeBy model.cubeCoeff model.quadCoeff model.multBy model.addTo
      sqrted = if model.sqrt then base >=> B.normSqrt model.norm (2 * model.power) else base
      cbrted = if model.cbrt then sqrted >=> B.normCbrt model.norm (3 * model.power) else sqrted
  in cbrted

incAll :: Model -> Model
incAll model = model { bubbles = map (bubbleFn model) $ A.range 0 model.maxInt
                     }

followTrajectory :: Array (List Int) -> Maybe Int -> Set Int
followTrajectory lookup x = go (L.fromFoldable x) S.empty
  where
    go :: List Int -> Set Int -> Set Int
    go circuit acc = case circuit of
      Nil -> acc
      (input : q) -> let outputs = fromMaybe Nil $ A.index lookup input
                         newOutputs = filter (not <<< flip S.member acc) outputs
                     in go (q <> newOutputs) (S.insert input acc)

reinitCache :: forall output. H.HalogenM Model Action () output Aff Unit
reinitCache = do
  H.modify_ regenBubbles
  incedModel <- H.modify incAll
  let
      inputInts = A.range 0 incedModel.maxInt
      outputInts = A.concatMap A.fromFoldable incedModel.bubbles
      ints = A.nub $ inputInts <> outputInts
      maxValue = A.foldl max incedModel.maxInt ints
      newCircuit = followTrajectory incedModel.bubbles incedModel.trajectoryFrom
  modelWithInts <- H.modify (_ { maxValue = maxValue
                               , drawnBuffers = S.empty :: Set Tick
                               , circuit = newCircuit
                               })
  let
    emptyCache :: Map Int Coordinates
    emptyCache = M.empty

    cache = A.foldl (\acc x -> M.insert x (getCoord modelWithInts x) acc) emptyCache ints
  modelWithCache <- H.modify (_ { cache = cache
                                })
  H.liftEffect $ redraw modelWithCache
  pure unit

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
                           }
changeMax :: Int -> Model -> Model
changeMax power model =
  let x = case model.norm of
        Padic p -> p
        Inf -> 1
  in model { maxInt = pow x power - 1,
             power = power }

canvasForTick :: Tick -> String
canvasForTick (Tick t) = "bubble-canvas-" <> show t

redraw :: Model -> Effect Unit
redraw model =
  let sqrtInterp = Reader.runReader (An.sqrtInterpolate model.time) model.maxTick
      sinInterp = Reader.runReader (An.sinInterpolate model.time) model.maxTick
      alpha = sinInterp 0.6 0.25
      calcer = getCoord model
      coordGetter = getCoordinates sqrtInterp model.cache calcer
      oldCoordGetter = getCoordinates (\_ y -> y) model.cache calcer
      canvasName = canvasForTick (fromMaybe (Tick 0) model.time)
  in do
    mcanvas <- Ca.getCanvasElementById canvasName
    case mcanvas of
      Nothing -> pure unit
      Just canvas -> do
        if fromMaybe false ( flip S.member model.drawnBuffers <$> model.time )
           then pure unit
           else do
            ctx <- Ca.getContext2D canvas
            dim <- Ca.getCanvasDimensions canvas
            Ca.clearRect ctx {x: 0.0, y: 0.0, width: dim.width, height: dim.height}
            Ca.setGlobalAlpha ctx alpha
            drawBubbles ctx model oldCoordGetter coordGetter model.radius $
              A.mapWithIndex 
                (\idx vs -> distributeTuple (Tuple idx vs)) 
                model.bubbles
            pure unit

redrawStep :: forall action output. H.HalogenM Model action () output Aff Unit
redrawStep = do
  model <- H.get
  H.modify_ (_ { animate = false })
  H.liftEffect (redraw model)
  H.modify_ (_ { drawnBuffers = S.insert (fromMaybe (Tick 0) model.time) model.drawnBuffers })
  H.modify_ (_ { animate = true })
  pure unit

handleQuery :: forall a output. Query a -> H.HalogenM Model Action () output Aff (Maybe a)
handleQuery (ReceiveAction a next) = handleAction a *> pure (Just next)

handleAction :: forall output. Action -> H.HalogenM Model Action () output Aff Unit
handleAction (ChangeMax power) =
  H.modify_ (changeMax power) *> reinitCache
handleAction (ChangeNorm norm) = H.modify_ (_ { norm = norm }) *> reinitCache
handleAction (ChangeTick tick) =
  H.modify_ (_ { maxTick = tick } ) *> reinitCache
handleAction (ChangeAddTo addTo) =
  H.modify_ (_ { addTo = addTo } ) *> reinitCache
handleAction (ChangeMultBy multBy) =
  H.modify_ (_ { multBy = multBy } ) *> reinitCache
handleAction (ChangeQuadBy quadCoeff) =
  H.modify_ (_ { quadCoeff = quadCoeff } ) *> reinitCache
handleAction (ChangeCubeBy cubeCoeff) =
  H.modify_ (_ { cubeCoeff = cubeCoeff } ) *> reinitCache
handleAction (ChangeSqrt b) =
  H.modify_ (_ { sqrt = b }) *> reinitCache
handleAction (ChangeCbrt b) =
  H.modify_ (_ { cbrt = b }) *> reinitCache
handleAction (ChangeScale scale) =
  H.modify_ (_ { scale = scale } ) *> reinitCache
handleAction (ChangeRadius radius) =
  H.modify_ (_ { radius = radius } ) *> reinitCache
handleAction (TrajectoryFrom int) = do
  H.modify_ (_ { trajectoryFrom = int } )
  case int of
    Nothing -> H.modify_ (_ { circuit = S.empty :: Set Int })
    Just n -> H.modify_ (_ { circuit = S.singleton n } )
  reinitCache
handleAction ToggleRepr = H.modify_ toggleRepr *> reinitCache
handleAction ToggleLines = H.modify_ toggleLines *> reinitCache
handleAction ToggleAnimation = H.modify_ toggleAnimation *> reinitCache
handleAction MoveTick = do
  model <- H.get
  case model.animate of
    false -> do
      H.liftEffect $ redraw model
      pure unit
    true -> do
      let newTick = model.time >>= \t -> Reader.runReader (incTick t) model.maxTick
      case newTick of
        Just _ -> do
          H.modify_ (_ { time = newTick })
          redrawStep
          pure unit
        Nothing -> do
          H.modify_ (_ { time = Just startTick })
          redrawStep
          pure unit
handleAction InitCaches = reinitCache
handleAction Reset = do
  model <- H.get
  H.modify_ (changeMax model.power)
  reinitCache
