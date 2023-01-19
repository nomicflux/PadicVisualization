module Component.Animation where

import Prelude

import Control.Monad.Reader (class MonadReader, ask)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number as Number

newtype Tick = Tick Int
derive instance eqTick :: Eq Tick
derive instance ordTick :: Ord Tick

startTick :: Tick
startTick = Tick 1

incTick :: forall m. MonadReader Int m => Tick -> m (Maybe Tick)
incTick t@(Tick n) = do
  maxxed <- atMax t
  case maxxed of
    true -> pure Nothing
    false -> pure $ Just (Tick (n + 1))

atMax :: forall m. MonadReader Int m => Tick -> m Boolean
atMax (Tick n) = do
  x <- ask
  pure (n >= x)

proportionalTick :: forall m. MonadReader Int m => Maybe Tick -> m Number
proportionalTick Nothing = pure 1.0
proportionalTick (Just (Tick n)) = do
  x <- ask
  pure $ toNumber n / toNumber x

interpolate :: forall m. MonadReader Int m =>
               (Number -> Number) ->
               Maybe Tick ->
               m (Number -> Number ->
                  Number)
interpolate f t = do
  prop <- proportionalTick t
  pure $ \to from -> (to - from) * f prop + from

sinInterpolate :: forall m. MonadReader Int m =>
                  Maybe Tick -> m (Number -> Number -> Number)
sinInterpolate = interpolate (flip Number.pow 2.0 <<< Number.sin <<< (\x -> Number.pi * x - Number.pi / 4.0))

cosInterpolate :: forall m. MonadReader Int m =>
                  Maybe Tick -> m (Number -> Number -> Number)
cosInterpolate = interpolate ((1.0 - _) <<< Number.cos <<< (\x -> Number.pi * x))

cubicInterpolate :: forall m. MonadReader Int m =>
                    Maybe Tick -> m (Number -> Number -> Number)
cubicInterpolate =
  let stretch x = x * 2.0 - 1.0
      unstretch x = (x + 1.0) * 0.5
  in
   interpolate $ unstretch <<< (\x -> Number.pow x 3.0) <<< stretch

sqrtInterpolate :: forall m. MonadReader Int m =>
                   Maybe Tick -> m (Number -> Number -> Number)
sqrtInterpolate =
  let stretch x = x * 2.0 - 1.0
      unstretch x = (x + 1.0) * 0.5
      cbrt x = if x >= 0.0
               then Number.pow x 0.5
               else -1.0 * Number.pow (-x) 0.5
  in
   interpolate $ unstretch <<< cbrt <<< stretch

linInterpolate :: forall m. MonadReader Int m =>
                  Maybe Tick -> m (Number -> Number -> Number)
linInterpolate = interpolate identity
