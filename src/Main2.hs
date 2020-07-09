{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}

module Main2 where

import Data.Function
import Control.Arrow
import Data.Coerce
import Unsafe.Coerce
import Propositional2
-- import GDP hiding ((&&))
import Theory.Named
import Logic.Proof
import Data.The
import Data.Refined
import Logic.Implicit
import qualified Logic.Propositional as L

main :: IO ()
main = do
  let raw_data = [1, 2, 3]
      preprocessed = pipeline raw_data
      -- _ = kmeans preprocessed
  return ()

note' = note . conjure

newtype Composable (p :: * -> *) (q :: * -> *) = Composable Defn
norm_dopped :: Proof (Composable NoMissingVals Normalized)
norm_dopped = axiom

any_shuffle :: f p -> Proof (Composable f Shuffled)
any_shuffle _ = axiom

(&?) :: (a ?p) ::: Composable p q -> (a -> a ?q) -> a ?(p && q)
(&?) a f = unsafeCoerce . f . the $ a

(&??) :: Fact (p n) => a ~~ n ::: Composable p q -> (a -> a ?q) -> (Fact ((p && q) n) => t) -> t
(&??) a f = unsafeCoerce . f . the $ a


-- pipeline :: [Int] -> [Int] ?(NoMissingVals && Normalized)
pipeline df =
     dropMissing df ...norm_dopped
  &? normalize
  -- &? shuffle
  &  kmeans

newtype Normalized df = Normalized Defn
type role Normalized nominal

newtype NoMissingVals df = NoMissingVals Defn
type role NoMissingVals nominal

newtype Shuffled df = Shuffled Defn
type role Shuffled nominal

normalize :: [Int] -> [Int] ?Normalized
normalize = assert

dropMissing :: [Int] -> [Int] ?NoMissingVals
dropMissing = assert

shuffle :: [Int] -> [Int] ?Shuffled
shuffle = assert

kmeans
  :: [Int] ?(NoMissingVals && Normalized)
  -> [Int]
kmeans = undefined
