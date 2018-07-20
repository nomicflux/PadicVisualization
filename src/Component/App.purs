module Component.App where

import Prelude

import Component.Canvas as CC
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import HalogenHelpers.Communication (passAlong)
import Norm (Norm(..))

baseInput :: CC.Input
baseInput = { size: 1024
            , maxInt: 1000
            , windingNumber: 729
            , norm: Padic 3
            }

data Query a = SetNorm Norm a
             | SetWinding Int a
             | SetMax Int a
             | Tick a
             | HandleMessage CC.Message a

component :: forall m. H.Component HH.HTML Query Unit CC.Message m
component = H.parentComponent { initialState: const unit
                              , render
                              , eval
                              , receiver: const Nothing
                              }

render :: forall m. Unit -> H.ParentHTML Query CC.Query CC.Slot m
render _ = HH.div_ [ HH.slot CC.Slot CC.component baseInput (HE.input HandleMessage) ]

eval :: forall m. Query ~> H.ParentDSL Unit Query CC.Query CC.Slot CC.Message m
eval (SetNorm norm next) = passAlong (CC.ChangeNorm norm) *> pure next
eval (SetWinding winding next) = passAlong (CC.ChangeWinding winding) *> pure next
eval (SetMax max next) = passAlong (CC.ChangeMax max) *> pure next
eval (Tick next) = passAlong CC.MoveTick *> pure next
eval (HandleMessage msg next) = pure next
