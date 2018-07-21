module Component.Animation where

import Prelude

import Control.Monad.Reader (class MonadReader, ask)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Math as Math

newtype Tick = Tick Int

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
               Number -> Number ->
               m Number
interpolate f t to from = do
  prop <- proportionalTick t
  pure $ (to - from) * f prop + from

sinInterpolate :: forall m. MonadReader Int m =>
                  Maybe Tick -> Number -> Number -> m Number
sinInterpolate = interpolate (Math.sin <<< (\x -> Math.pi / 2.0 * x))

cosInterpolate :: forall m. MonadReader Int m =>
                  Maybe Tick -> Number -> Number -> m Number
cosInterpolate = interpolate (Math.cos <<< (1.0 - _) <<< (\x -> Math.pi / 2.0 * x))

linInterpolate :: forall m. MonadReader Int m =>
                  Maybe Tick -> Number -> Number -> m Number
linInterpolate = interpolate identity
