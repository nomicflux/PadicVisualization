module Component.App where

import Prelude

import Component.Canvas as CC
import Data.Array ((:))
import Data.Filterable (filter)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Norm (Norm(..), getPrime)
import Type.Proxy (Proxy(..))

baseInput :: CC.Input
baseInput = { size: 1024
            , power: 5
            , norm: Padic 3
            , coordType: CC.PadicVector
            , maxTick: 64
            , scale: 200
            , radius: 1
            , addTo: 1
            , multBy: 1
            , quadCoeff: 0
            , cubeCoeff: 0
            , sqrt: false
            , cbrt: false
            , trajectoryFrom: Nothing
            }

data Query a = ReceiveAction Action a

data Action = SetNorm Int
             | SetMax Int
             | SetTick Int
             | SetScale Int
             | SetRadius Int
             | SetAdd Int
             | SetMult Int
             | SetQuad Int
             | SetCube Int
             | ChangeSqrt Boolean
             | ChangeCbrt Boolean
             | TrajectoryFrom (Maybe Int)
             | ToggleRepr
             | ToggleAnimation
             | ToggleLines
             | Reset
             | Tick
             | Noop

component :: forall output. H.Component Query Unit output Aff
component = H.mkComponent { initialState: const unit
                          , render
                          , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                                           , handleQuery = handleQuery
                                                           }
                          }

toNatural :: String -> Maybe Int
toNatural = fromString >>> filter (_ >= 0)

type CanvasSlot = ( canvas :: H.Slot CC.Query Void Unit )
_canvas = Proxy :: Proxy "canvas"

render :: Unit -> H.ComponentHTML Action CanvasSlot Aff
render _ = HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
           [ renderSidebar, renderMain ]
  where
    withDescription :: H.ComponentHTML Action CanvasSlot Aff ->
                       Maybe String -> String ->
                       H.ComponentHTML Action CanvasSlot Aff
    withDescription inputDiv descr class_ =
      let descrDiv = maybe [] (\txt -> [ HH.div [ HP.class_ $ HH.ClassName "description"] [ HH.text txt ]]) descr
      in HH.div [ HP.class_ $ HH.ClassName class_ ] (inputDiv : descrDiv)

    mkButton :: String -> String -> Maybe String ->
                Action ->
                H.ComponentHTML Action CanvasSlot Aff
    mkButton text class_ description query =
      let buttonDiv = HH.button [ HP.class_ $ HH.ClassName ("pure-button button-" <> class_)
                                , HE.onClick \_ -> query
                                , HP.type_ $ HP.ButtonButton
                                ]
                      [ HH.text text ]
      in withDescription buttonDiv description "button-div"

    mkCheckbox :: String -> Maybe String ->
                  (Boolean -> Action) ->
                  H.ComponentHTML Action CanvasSlot Aff
    mkCheckbox text descr query =
      let inputDiv = HH.div_ [ HH.label_ [ HH.text ( text <> ": ")]
                             , HH.input [ HP.class_ (HH.ClassName "attr-boolean")
                                        , HP.type_ HP.InputCheckbox
                                        , HE.onChecked query
                                        , HP.title text
                                        ]
                             ]
      in withDescription inputDiv descr "checkbox-div"

    mkMaybeNumInput :: String -> Maybe String -> Maybe String ->
                  (Maybe Int -> Action) ->
                  H.ComponentHTML Action CanvasSlot Aff
    mkMaybeNumInput text placeholder descr query =
      let inputDiv = HH.div_ [ HH.label_ [HH.text (text <> ": ")]
                             , HH.input [ HP.class_ (HH.ClassName "attr-numeric")
                                        , HP.type_ HP.InputNumber
                                        , HE.onValueChange $ query <<< toNatural
                                        , HP.title text
                                        , HP.prop (HH.PropName "maxLength") 4
                                        , HP.placeholder $ fromMaybe "" placeholder
                                        , HP.min 0.0
                                        ]
                             ]
      in withDescription inputDiv descr "input-div"

    mkNumInput :: String -> String -> Maybe String ->
                  (Int -> Action) ->
                  H.ComponentHTML Action CanvasSlot Aff
    mkNumInput text placeholder descr query =
      mkMaybeNumInput text (Just placeholder) descr ( fromMaybe Noop <<< map query )

    renderSidebar :: H.ComponentHTML Action CanvasSlot Aff
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-1-4 sidebar" ]
      [ mkButton "Toggle Representation" "primary" (Just "Between fractal given by the p-adic representation, or circles given by the p-adic norm") ToggleRepr
      , mkButton "Toggle Animation" "warning" (Just "Turn animation on and off") ToggleAnimation
      , mkButton "Toggle Lines" "secondary" (Just "Turn lines on and off") ToggleLines
      , mkButton "Reset" "error" (Just "Reset animation") Reset
      , mkNumInput "_-adic Norm" (show $ fromMaybe 0 (getPrime baseInput.norm)) (Just "<= 1 yields normal absolute value; 2 and above use p-adic norm") SetNorm
      , mkNumInput "# of Frames" (show baseInput.maxTick) (Just "Frames between each position; controls speed of animation") SetTick
      , mkNumInput "p^# of Circles" (show baseInput.power) (Just "Use this p^(power) many circles; for example, in the 3-adic norm, setting this to 3 will use 27 circles") SetMax
      , mkNumInput "Scale" (show baseInput.scale) (Just "Controls distance between dots") SetScale
      , mkNumInput "Radius" (show baseInput.radius) (Just "Controls size of dots") SetRadius
      , mkNumInput "Constant Term" (show baseInput.addTo) (Just "Set constant term in ax^3 + bx^2 + cx + d") SetAdd
      , mkNumInput "Linear Coefficient" (show baseInput.multBy) (Just "Set linear component") SetMult
      , mkNumInput "Quad Coefficient" (show baseInput.quadCoeff) (Just "Set quadratic component") SetQuad
      , mkNumInput "Cubic Coefficient" (show baseInput.cubeCoeff) (Just "Set cubic component") SetCube
      , mkCheckbox "Square Root?" (Just "Take square root of the above") ChangeSqrt
      , mkCheckbox "Cube Root?" (Just "Take cube root of the above") ChangeCbrt
      , mkMaybeNumInput "Show Trajectory From" Nothing (Just "Draw line for full circuit from starting point") TrajectoryFrom
      ]

    renderMain :: H.ComponentHTML Action CanvasSlot Aff
    renderMain =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-3-4" ]
      [ HH.slot_ _canvas unit CC.component baseInput ]


passAlong :: forall state action output m. CC.Action -> H.HalogenM state action CanvasSlot output m Unit
passAlong action = H.tell _canvas unit $ toQuery action
                   where toQuery :: CC.Action -> (Unit -> CC.Query Unit)
                         toQuery action = CC.ReceiveAction action

handleQuery :: forall a output. Query a -> H.HalogenM Unit Action CanvasSlot output Aff (Maybe a)
handleQuery (ReceiveAction a next) = handleAction a *> pure (Just next)

handleAction :: forall output m. Action -> H.HalogenM Unit Action CanvasSlot output m Unit
handleAction Noop = pure unit
handleAction (SetNorm normNum) =
  let norm = if normNum <= 1 then Inf else Padic normNum
  in passAlong (CC.ChangeNorm norm) *>
     passAlong CC.Reset *>
     pure unit
handleAction (SetMax max) = passAlong (CC.ChangeMax max) *> pure unit
handleAction (SetTick max) = passAlong (CC.ChangeTick max) *> pure unit
handleAction (SetScale scale) = passAlong (CC.ChangeScale scale) *> pure unit
handleAction (SetRadius radius) = passAlong (CC.ChangeRadius radius) *> pure unit
handleAction (SetAdd x) = passAlong (CC.ChangeAddTo x) *> pure unit
handleAction (SetMult y) = passAlong (CC.ChangeMultBy y) *> pure unit
handleAction (SetQuad q) = passAlong (CC.ChangeQuadBy q) *> pure unit
handleAction (SetCube c) = passAlong (CC.ChangeCubeBy c) *> pure unit
handleAction (ChangeSqrt b) = passAlong (CC.ChangeSqrt b) *> pure unit
handleAction (ChangeCbrt b) = passAlong (CC.ChangeCbrt b) *> pure unit
handleAction (TrajectoryFrom t) = passAlong (CC.TrajectoryFrom t) *> pure unit
handleAction ToggleRepr = passAlong CC.ToggleRepr *> pure unit
handleAction ToggleLines = passAlong CC.ToggleLines *> pure unit
handleAction ToggleAnimation = passAlong CC.ToggleAnimation *> pure unit
handleAction Reset = passAlong CC.Reset *> pure unit
handleAction Tick = passAlong CC.MoveTick *> pure unit
