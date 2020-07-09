{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test where

import GDP

newtype NoMissing v = NoMissing Defn
newtype Normalized v = Normalized Defn

newtype Composable (p :: * -> *) (q :: * -> *) = Composable Defn

-- norm_dropped :: NoMissing df -> Proof (NoMissing (Normalized df))
-- norm_dropped _ = axiom
norm_dropped :: Proof (Composable NoMissing Normalized)
norm_dropped = axiom

-- pipeline :: a -> b ~~ name ::: NoMissing name && Normalized name
pipeline = note norm_dropped $ flattenProof . normalize . dropMissing

flattenProof :: Fact (Composable p q) => a ~~ q (p name) -> a ::: (p name && q name)
flattenProof a = the a ...axiom

data Pipeline (p :: * -> *) (q :: * -> *) a = Pipeline a
  deriving (Show, Functor, Applicative)

instance Fact (Composable p q) => Monad (Pipeline p q) where
  join = flattenProof

-- (.|)
--   :: Fact (Composable p q)
--   => (a ~~ n -> b ~~ p n)
--   -> (r ~~ m -> s ~~ q m)
--   -> s ::: (




normalize :: a ~~ name -> b ~~ Normalized name
normalize = undefined

dropMissing :: a ~~ name -> b ~~ NoMissing name
dropMissing = undefined
