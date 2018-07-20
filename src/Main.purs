module Main where

import Prelude

import Component.App as CA
import Control.Monad.Rec.Class (forever)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

tickDelay :: Milliseconds
tickDelay = Milliseconds 16.7

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI CA.component unit body
  forever do
    _ <- delay tickDelay
    void $ io.query $ H.action $ CA.Tick
