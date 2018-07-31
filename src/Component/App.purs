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
            , maxInt: 728
            , norm: Padic 3
            , coordType: CC.PadicVector
            , maxTick: 64
            , scale: 200
            , radius: 1
            , addTo: 1
            , multBy: 1
            , quadCoeff: 0
            , cubeCoeff: 0
            , cycle: true
            }

data Query a = SetNorm Int a
             | SetMax Int a
             | SetTick Int a
             | SetScale Int a
             | SetRadius Int a
             | SetAdd Int a
             | SetMult Int a
             | ToggleRepr a
             | ToggleAnimation a
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
    mkButton :: String -> String -> Maybe String ->
                (Unit -> Query Unit) ->
                H.ParentHTML Query CC.Query CC.Slot Aff
    mkButton text class_ description query =
      let buttonDiv = HH.button [ HP.class_ $ HH.ClassName ("pure-button button-" <> class_)
                                , HE.onClick $ HE.input_ query
                                , HP.type_ $ HP.ButtonButton
                                ]
                      [ HH.text text ]
          descrDiv = maybe [] (\txt -> [ HH.div [ HP.class_ $ HH.ClassName "description"] [ HH.text txt ]]) description
      in HH.div [ HP.class_ $ HH.ClassName "button-div" ] (buttonDiv : descrDiv)

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
          descrDiv = maybe [] (\txt -> [ HH.div [ HP.class_ $ HH.ClassName "description" ] [ HH.text txt ] ]) descr
      in
       HH.div [ HP.class_ $ HH.ClassName "input-div" ] (inputDiv : descrDiv)

    renderSidebar :: H.ParentHTML Query CC.Query CC.Slot Aff
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-1-4 sidebar" ]
      [ mkButton "Toggle Representation" "primary" (Just "Between fractal given by the p-adic representation, or circles given by the p-adic norm") ToggleRepr
      , mkButton "Toggle Animation" "warning" (Just "Turn animation on and off") ToggleAnimation
      , mkNumInput "_-adic Norm" (show $ fromMaybe 0 (getPrime baseInput.norm)) (Just "<= 1 yields normal absolute value; 2 and above use p-adic norm") SetNorm
      , mkNumInput "# of Frames" (show baseInput.maxTick) (Just "Frames between each position; controls speed of animation") SetTick
      , mkNumInput "Max Int" (show baseInput.maxInt) (Just "Displays all numbers from 0 up to and incl. the max int; for best results, use a power of the number used for the p-adic norm minus one, especially if changing Add To and Mult By") SetMax
      , mkNumInput "Scale" (show baseInput.scale) (Just "Controls distance between dots") SetScale
      , mkNumInput "Radius" (show baseInput.radius) (Just "Controls size of dots") SetRadius
      , mkNumInput "Add To" (show baseInput.addTo) Nothing SetAdd
      , mkNumInput "Mult By" (show baseInput.multBy) (Just "For \"Add To\" b and \"Mult By\" a, this will change a number n to a*n + b") SetMult
      ]

    renderMain :: H.ParentHTML Query CC.Query CC.Slot Aff
    renderMain =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-3-4" ]
      [ HH.slot CC.Slot CC.component baseInput (HE.input HandleMessage) ]

eval :: forall m. Query ~> H.ParentDSL Unit Query CC.Query CC.Slot CC.Message m
eval (SetNorm normNum next) =
  let norm = if normNum <= 1 then Inf else Padic normNum
  in passAlong (CC.ChangeNorm norm) *> pure next
eval (SetMax max next) = passAlong (CC.ChangeMax max) *> pure next
eval (SetTick max next) = passAlong (CC.ChangeTick max) *> pure next
eval (SetScale scale next) = passAlong (CC.ChangeScale scale) *> pure next
eval (SetRadius radius next) = passAlong (CC.ChangeRadius radius) *> pure next
eval (SetAdd x next) = passAlong (CC.ChangeAddTo x) *> pure next
eval (SetMult y next) = passAlong (CC.ChangeMultBy y) *> pure next
eval (ToggleRepr next) = passAlong CC.ToggleRepr *> pure next
eval (ToggleAnimation next) = passAlong CC.ToggleAnimation *> pure next
eval (Tick next) = passAlong CC.MoveTick *> pure next
eval (HandleMessage msg next) = pure next
