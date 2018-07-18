module Component.Canvas where

import Prelude

import Component.Animation (Tick(..), incTick)
import Component.Bubble (Bubble(..), decValue, incValue, mkBubble)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Halogen as H
import Norm (Norm(..))

type Model = { maxInt :: Int
             , norm :: Norm
             , bubbles :: List Bubble
             , time :: Maybe Tick
             }

initialModel :: Int -> Model
initialModel maxInt = { maxInt
                      , norm: Inf
                      , bubbles: mkBubble <$> (L.range 0 maxInt)
                      , time: Nothing
                      }

data Query a = ChangeMax Int a
             | IncValues a
             | DecValues a
             | ChangeNorm Norm a
             | MoveTick a

data Message = Noop

eval :: forall m. Query ~> H.ComponentDSL Model Query Message m
eval (ChangeMax max next) = H.modify_ (_ { maxInt = max }) *> pure next
eval (IncValues next) = do
  model <- H.get
  H.modify_ (_ { bubbles = incValue model.maxInt <$> model.bubbles })
  pure next
eval (DecValues next) = do
  model <- H.get
  H.modify_ (_ { bubbles = decValue model.maxInt <$> model.bubbles })
  pure next
eval (ChangeNorm norm next) = H.modify_ (_ { norm = norm }) *> pure next
eval (MoveTick next) = do
  tick <- H.gets (_.time)
  H.modify_ (_ { time = tick >>= incTick })
  pure next
