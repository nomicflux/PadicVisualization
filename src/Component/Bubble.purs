module Component.Bubble where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Rational (Rational)
import Norm (Norm, takeNorm)

newtype Bubble = Bubble { oldValue :: Maybe Int
                        , value :: Int
                        }

mkBubble :: Int -> Bubble
mkBubble value = Bubble { oldValue: Nothing
                        , value
                        }

getValue :: Bubble -> Int
getValue (Bubble b) = b.value

getOldValue :: Bubble -> Maybe Int
getOldValue (Bubble b) = b.oldValue

setValue :: Int -> Bubble -> Bubble
setValue n (Bubble b) = Bubble (b { value = n })

changeValue :: Int -> Bubble -> Bubble
changeValue n = modifyBubble (\b -> setValue n b)

getNormedValue :: Norm -> Bubble -> Rational
getNormedValue norm bubble = takeNorm norm (getValue bubble)

getNormedOldValue :: Norm -> Bubble -> Maybe Rational
getNormedOldValue norm bubble = takeNorm norm <$> (getOldValue bubble)

modifyBubble :: (Bubble -> Bubble) -> Bubble -> Bubble
modifyBubble f b@(Bubble bubble) =
  let (Bubble new) = f b
  in Bubble (new { oldValue = Just bubble.value })

incValue :: Bubble -> Bubble
incValue = modifyBubble (\b -> setValue (getValue b + 1) b)

incValueBy :: Int -> Bubble -> Bubble
incValueBy by = modifyBubble (\b -> setValue (getValue b + by) b)

multValueBy :: Int -> Bubble -> Bubble
multValueBy by = modifyBubble (\b -> setValue (getValue b * by) b)

linearBy :: Int -> Int -> Bubble -> Bubble
linearBy by plus = modifyBubble (\b -> setValue (getValue b * by + plus) b)

quadBy :: Int -> Int -> Int -> Bubble -> Bubble
quadBy sqr by plus =
  modifyBubble (\b -> let v = getValue b in setValue (v*v*sqr + v*by + plus) b)

cubeBy :: Int -> Int -> Int -> Int -> Bubble -> Bubble
cubeBy cube sqr by plus =
  modifyBubble (\b -> let v = getValue b
                      in setValue (v*v*v*cube + v*v*sqr + v*by + plus) b)

decValue :: Bubble -> Bubble
decValue = modifyBubble (\b -> setValue (getValue b - 1) b)
