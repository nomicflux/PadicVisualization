module Roots where

import Prelude

import Data.List as L
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

joinLists :: forall a. List a -> List a -> List a
joinLists ls rs = L.concat (ls:rs:Nil)

modRoots :: Int -> Map Int (List Int)
modRoots p =
  let ints = L.range 0 (p - 1)
      squared = map (\x -> Tuple ((x * x) `mod` p) (L.singleton x)) ints
  in M.fromFoldableWith joinLists squared

modCubeRoots :: Int -> Map Int (List Int)
modCubeRoots p =
  let ints = L.range 0 (p - 1)
      squared = map (\x -> Tuple ((x * x * x) `mod` p) (L.singleton x)) ints
  in M.fromFoldableWith joinLists squared

recipMod :: Int -> Int -> Maybe Int
recipMod _ 1 = Just 1
recipMod p x = if gcd p x == 1
                 then Just $ go 0 1 p x
                 else Nothing
  where
    go :: Int -> Int -> Int -> Int -> Int
    go t _ _ 0 = t `mod` p
    go t nt r nr =
      let q = r `div` nr
      in go nt (t - q*nt) nr (r - q*nr)

modRecips :: Int -> Map Int (Maybe Int)
modRecips p =
   let ints = L.range 0 (p - 1)
       inverted = (\x -> Tuple x (recipMod p x)) <$> ints
   in M.fromFoldable inverted

lookupOr :: forall a b. Ord a => a -> b -> Map a b -> b
lookupOr key def m = fromMaybe def $ M.lookup key m

pSqrt :: Int -> Int -> Int -> List Int
pSqrt _ _ 0 = (0 : Nil)
pSqrt p steps x =
    getRoot x >>=
    \y -> getRecip (2 * y) >>=
    \df -> pure (go p 1 y df)
  where
    roots = modRoots p
    recips = modRecips p
    getRoot :: Int -> List Int
    getRoot = \n -> lookupOr (n `mod` p) Nil roots
    getRecip :: Int -> List Int
    getRecip = \n -> L.fromFoldable (lookupOr (n `mod` p) Nothing recips)
    go :: Int -> Int -> Int -> Int -> Int
    go pk step n df
       | step >= steps = n
       | otherwise = let f = (n*n - x) `div` pk
                         d = (f * (p - df)) `mod` p
                     in go
                        (p * pk)
                        (step + 1)
                        (n + d * pk)
                        df

pCbrt :: Int -> Int -> Int -> List Int
pCbrt _ _ 0 = (0 : Nil)
pCbrt p steps x =
    getRoot x >>=
    \y -> getRecip (3 * y * y) >>=
    \df -> pure (go p 1 y df)
  where
    roots = modCubeRoots p
    recips = modRecips p
    getRoot :: Int -> List Int
    getRoot = \n -> lookupOr (n `mod` p) Nil roots
    getRecip :: Int -> List Int
    getRecip = \n -> L.fromFoldable (lookupOr (n `mod` p) Nothing recips)
    go :: Int -> Int -> Int -> Int -> Int
    go pk step n df
       | step >= steps = n
       | otherwise = let f = (n*n*n - x) `div` pk
                         d = (f * (p - df)) `mod` p
                     in go
                        (p * pk)
                        (step + 1)
                        (n + d * pk)
                        df
