module Component.App where

import Prelude

import Component.Canvas as CC
import Data.Array ((:))
import Data.Filterable (filter)
import Data.Int (fromString, toNumber, round)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenHelpers.Communication (passAlong)
import Norm (Norm(..), getPrime)

initMaxInt :: Int
initMaxInt = 729
initNorm :: Norm
initNorm = Padic 3
initTick :: Int
initTick = 32
initScale :: Int
initScale = 150
initZoom :: Number
initZoom = 1.5

zoomDiv :: Number
zoomDiv = 10.0

baseInput :: CC.Input
baseInput = { size: 1024
            , maxInt: initMaxInt
            , norm: initNorm
            , coordType: CC.PadicVector
            , maxTick: initTick
            , scale: initScale
            , zoom: initZoom
            }

data Query a = SetNorm Int a
             | SetMax Int a
             | SetTick Int a
             | SetScale Int a
             | SetZoom Int a
             | ToggleRepr a
             | ToggleAnimation a
             | Tick a
             | HandleMessage CC.Message a

component :: forall m. H.Component HH.HTML Query Unit CC.Message m
component = H.parentComponent { initialState: const unit
                              , render
                              , eval
                              , receiver: const Nothing
                              }

toNatural :: String -> Maybe Int
toNatural = fromString >>> filter (_ >= 0)

render :: forall m. Unit -> H.ParentHTML Query CC.Query CC.Slot m
render _ = HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
           [ renderSidebar, renderMain ]
  where
    mkButton :: String -> String ->
                Boolean ->
                (Unit -> Query Unit) ->
                H.ParentHTML Query CC.Query CC.Slot m
    mkButton text class_ disabled query =
      HH.button [ HP.class_ $ HH.ClassName ("pure-button button-" <> class_)
                , HE.onClick $ HE.input_ query
                , HP.disabled disabled
                , HP.type_ $ HP.ButtonButton
                ]
      [ HH.text text ]

    mkNumInput :: String -> String -> Maybe String ->
                  (Int -> Unit -> Query Unit) ->
                  H.ParentHTML Query CC.Query CC.Slot m
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

    renderSidebar :: H.ParentHTML Query CC.Query CC.Slot m
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-1-4 sidebar" ]
      [ mkButton "Toggle Representation" "primary" false ToggleRepr
      , mkButton "Toggle Animation" "warning" false ToggleAnimation
      , mkNumInput "Norm" (show $ fromMaybe 0 (getPrime initNorm)) (Just "<= 1 yields normal absolute value; 2 and above use p-adic norm") SetNorm
      , mkNumInput "# of Frames" (show initTick) (Just "Frames between each position; controls speed of animation") SetTick
      , mkNumInput "Max Int" (show initMaxInt) (Just "Displays all numbers from 0 up to and incl. the max int") SetMax
      , mkNumInput "Scale" (show initScale) (Just "Controls size of dots") SetScale
      , mkNumInput "Zoom" (show $ round $ zoomDiv * initZoom) (Just "Smaller numbers zoom closer, large farther away") SetZoom
      ]

    renderMain :: H.ParentHTML Query CC.Query CC.Slot m
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
eval (SetZoom zoom next) = passAlong (CC.ChangeZoom $ toNumber zoom / zoomDiv) *> pure next
eval (ToggleRepr next) = passAlong CC.ToggleRepr *> pure next
eval (ToggleAnimation next) = passAlong CC.ToggleAnimation *> pure next
eval (Tick next) = passAlong CC.MoveTick *> pure next
eval (HandleMessage msg next) = pure next
