{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}

module Main where

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

main :: IO ()
main = do
  -- let raw_data = [1, 2, 3]
  -- name raw_data \df -> do
  --   let foo = Main.seq norm_dopped dropMissing normalize df
  --       p = conjure foo
  --       _ = note (symmetric p) $ kmeans (exorcise foo)
    return ()

-- newtype Composable (p :: * -> *) (q :: * -> *) = Composable Defn
-- newtype PredAnd (p :: * -> *) (q :: * -> *) = PredAnd Defn

-- type p &&? q = PredAnd p q

-- norm_dopped :: Proof (Composable NoMissingVals Normalized)
-- norm_dopped = axiom

-- ($@) :: The r a => (a -> b) -> r -> b
-- f $@ r = f (the r)
-- infixr 8 $@

-- (.|) g f r = g $@ f $@ r


-- comp :: (a -> a ?p) -> (a -> a ?q) -> Proof (Composable p q) -> a ~~ name -> a ~~ name ::: p name && q name
-- comp f g _ = unsafeCoerce . (g .| f)
-- -- infixl 1 .|?

-- -- pipeline = comp dropMissing normalize norm_dopped


-- seq
--   :: Proof (Composable p q)
--   -> (a -> a ?p)
--   -> (a -> a ?q)
--   -> a ~~ name
--   -> a ~~ name ::: p name && q name
-- seq c f g a =
--   let r = the (g $@ f $@ a)
--    in unsafeCoerce r ...axiom

-- newtype Normalized df = Normalized Defn
-- type role Normalized nominal

-- newtype NoMissingVals df = NoMissingVals Defn
-- type role NoMissingVals nominal

-- normalize :: [Int] -> [Int] ?Normalized
-- normalize = assert

-- dropMissing :: [Int] -> [Int] ?NoMissingVals
-- dropMissing = assert

-- kmeans
--   :: Fact (Normalized df && NoMissingVals df)
--   => [Int] ~~ df
--   -> [Int]
-- kmeans = undefined
