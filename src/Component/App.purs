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
import HalogenHelpers.Communication (passAlong)
import Norm (Norm(..), getPrime)

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
            }

data Query a = SetNorm Int a
             | SetMax Int a
             | SetTick Int a
             | SetScale Int a
             | SetRadius Int a
             | SetAdd Int a
             | SetMult Int a
             | SetQuad Int a
             | SetCube Int a
             | ChangeSqrt Boolean a
             | ChangeCbrt Boolean a
             | ToggleRepr a
             | ToggleAnimation a
             | Reset a
             | Tick a
             | HandleMessage CC.Message a

component :: H.Component HH.HTML Query Unit CC.Message Aff
component = H.parentComponent { initialState: const unit
                              , render
                              , eval
                              , receiver: const Nothing
                              }

toNatural :: String -> Maybe Int
toNatural = fromString >>> filter (_ >= 0)

render :: Unit -> H.ParentHTML Query CC.Query CC.Slot Aff
render _ = HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
           [ renderSidebar, renderMain ]
  where
    withDescription :: H.ParentHTML Query CC.Query CC.Slot Aff ->
                       Maybe String -> String ->
                       H.ParentHTML Query CC.Query CC.Slot Aff
    withDescription inputDiv descr class_ =
      let descrDiv = maybe [] (\txt -> [ HH.div [ HP.class_ $ HH.ClassName "description"] [ HH.text txt ]]) descr
      in HH.div [ HP.class_ $ HH.ClassName class_ ] (inputDiv : descrDiv)

    mkButton :: String -> String -> Maybe String ->
                (Unit -> Query Unit) ->
                H.ParentHTML Query CC.Query CC.Slot Aff
    mkButton text class_ description query =
      let buttonDiv = HH.button [ HP.class_ $ HH.ClassName ("pure-button button-" <> class_)
                                , HE.onClick $ HE.input_ query
                                , HP.type_ $ HP.ButtonButton
                                ]
                      [ HH.text text ]
      in withDescription buttonDiv description "button-div"

    mkCheckbox :: String -> Maybe String ->
                  (Boolean -> Unit -> Query Unit) ->
                  H.ParentHTML Query CC.Query CC.Slot Aff
    mkCheckbox text descr query =
      let inputDiv = HH.div_ [ HH.label_ [ HH.text ( text <> ": ")]
                             , HH.input [ HP.class_ (HH.ClassName "attr-boolean")
                                        , HP.type_ HP.InputCheckbox
                                        , HE.onChecked $ HE.input query
                                        , HP.title text
                                        ]
                             ]
      in withDescription inputDiv descr "checkbox-div"

    mkNumInput :: String -> String -> Maybe String ->
                  (Int -> Unit -> Query Unit) ->
                  H.ParentHTML Query CC.Query CC.Slot Aff
    mkNumInput text placeholder descr query =
      let inputDiv = HH.div_ [ HH.label_ [HH.text (text <> ": ")]
                             , HH.input [ HP.class_ (HH.ClassName "attr-numeric")
                                        , HP.type_ HP.InputNumber
                                        , HE.onValueChange (toNatural >=> HE.input query)
                                        , HP.title text
                                        , HP.prop (HH.PropName "maxLength") 4
                                        , HP.placeholder placeholder
                                        , HP.min 0.0
                                        ]
                             ]
      in withDescription inputDiv descr "input-div"

    renderSidebar :: H.ParentHTML Query CC.Query CC.Slot Aff
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-1-4 sidebar" ]
      [ mkButton "Toggle Representation" "primary" (Just "Between fractal given by the p-adic representation, or circles given by the p-adic norm") ToggleRepr
      , mkButton "Toggle Animation" "warning" (Just "Turn animation on and off") ToggleAnimation
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
      ]

    renderMain :: H.ParentHTML Query CC.Query CC.Slot Aff
    renderMain =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-3-4" ]
      [ HH.slot CC.Slot CC.component baseInput (HE.input HandleMessage) ]

eval :: forall m. Query ~> H.ParentDSL Unit Query CC.Query CC.Slot CC.Message m
eval (SetNorm normNum next) =
  let norm = if normNum <= 1 then Inf else Padic normNum
  in passAlong (CC.ChangeNorm norm) *> passAlong CC.Reset *> pure next
eval (SetMax max next) = passAlong (CC.ChangeMax max) *> pure next
eval (SetTick max next) = passAlong (CC.ChangeTick max) *> pure next
eval (SetScale scale next) = passAlong (CC.ChangeScale scale) *> pure next
eval (SetRadius radius next) = passAlong (CC.ChangeRadius radius) *> pure next
eval (SetAdd x next) = passAlong (CC.ChangeAddTo x) *> pure next
eval (SetMult y next) = passAlong (CC.ChangeMultBy y) *> pure next
eval (SetQuad q next) = passAlong (CC.ChangeQuadBy q) *> pure next
eval (SetCube c next) = passAlong (CC.ChangeCubeBy c) *> pure next
eval (ChangeSqrt b next) = passAlong (CC.ChangeSqrt b) *> pure next
eval (ChangeCbrt b next) = passAlong (CC.ChangeCbrt b) *> pure next
eval (ToggleRepr next) = passAlong CC.ToggleRepr *> pure next
eval (ToggleAnimation next) = passAlong CC.ToggleAnimation *> pure next
eval (Reset next) = passAlong CC.Reset *> pure next
eval (Tick next) = passAlong CC.MoveTick *> pure next
eval (HandleMessage msg next) = pure next
