{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}

module Simple where

import Data.Coerce  ( coerce )
import Data.Maybe   ( catMaybes )
import GDP

kmeans
  :: Proof (NoMissing df)
  -> Proof (Normalized df)
  -> a ~~ df
  -> [Int]
kmeans = undefined

newtype Composable (p :: * -> *) (q :: * -> *) = Composable Defn
norm_dropped :: Proof (Composable NoMissing Normalized)
norm_dropped = axiom

-- pipe
--   :: a ?p
--   -> Proof (Composable p q)
--   -> (a -> a ?q)
--   -> (forall name. a ~~ name ::: (p name && q name) -> t)
--   -> t
-- pipe a _ f k =
--   let r = f (the a)
--    in k (the r ... _)
   -- in r ...introAnd (the a) (the r)

-- pipeline :: [Maybe Float] -> (forall v. [Float] ~~ v ::: (NoMissing v && Normalized v) -> t) -> t
-- pipeline xs k = k $ rename (dropMissing xs) $ \xs' -> rename (normalize pipe _ norm_dropped 
  -- rename
  --   (dropMissing xs)
  --   \xs' ->
  --     pipe _ norm_dropped _
  -- pipe
  --   (rename (dropMissing xs) \)
  --   norm_dropped
  --   _
-- pipeline df = pipe (dropMissing df) norm_dropped normalize
-- pipeline df = pipe (_ $ dropMissing df) norm_dropped undefined

newtype NoMissing df = NoMissing Defn
type role NoMissing nominal
dropMissing :: [Maybe a] -> [a] ?NoMissing
dropMissing xs = assert (catMaybes xs)

newtype Normalized df = Normalized Defn
type role Normalized nominal
normalize :: (Fractional a, Ord a) => [a] -> [a] ?Normalized
normalize xs =
  let min_ = minimum xs
      max_ = maximum xs
      range = max_ - min_
   in assert [x - min_ / range | x <- xs]
