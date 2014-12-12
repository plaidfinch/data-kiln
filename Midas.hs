<<<<<<< Updated upstream
{-# LANGUAGE RankNTypes #-}
=======
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs         #-}
>>>>>>> Stashed changes

module Midas
   ( Clay
   , ref
   , deref
   , modify
   , composedly
   , freeze
   , freezeWith
   , runFreezingWith
   , runFreezing
   , module Data.Fix
   , module Data.Functor.Compose
   , module Data.Traversable
   ) where

import Data.Fix
import Data.Functor.Compose
import Data.Traversable

import Control.Monad
import Control.Monad.IfElse
import Control.Applicative hiding ( empty )
import Control.Arrow

import Control.Monad.Squishy

import Prelude hiding ( lookup , foldr )
import Data.Map ( Map , insert , empty , lookup )

-- | A Clay is a recursive structure with a uniquely-identified mutable reference at each node
data Clay s f = Clay { getClay :: Distinct s (Ref s (f (Clay s f))) }

-- | Take a functor f containing a Clay of f, and wrap it in a mutable reference and a distinct tag, thus returning a new Clay of f.
ref :: f (Clay s f) -> Squishy s (Clay s f)
ref = (Clay <$>) . (distinguish =<<) . newRef

-- | Takes a Clay f and exposes the first level of f inside it.
deref :: Clay s f -> Squishy s (f (Clay s f))
deref = readRef . conflate . getClay

-- | Apply a function (destructively) to the STRef contained in a Clay.
modify :: Clay s f -> (f (Clay s f) -> f (Clay s f)) -> Squishy s ()
modify = modifyRef . conflate . getClay

-- | Apply a function to the value inside a Compose.
composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

-- | The type of natural transformations from f to g.
type f ~> g = forall a. f a -> g a

-- | Given a Clay s f, use a natural transformation (forall a. f a -> g a) to convert it into the fixed-point of a the functor g by eliminating the indirection of the mutable references and using the distinct tags on the structure's parts to tie knots where there are cycles in the original graph of references. TThe result is an immutable cyclic lazy data structure.
freezeWith :: (Traversable f) => (f ~> g) -> Clay s f -> Squishy s (Fix g)
freezeWith transform = (newRef empty >>=) . flip freeze'
   where
      freeze' seen mutable =
         aifM (lookup thisID <$> readRef seen) return $ do
            frozen <- (Fix . transform <$>) . traverse (freeze' seen) <=< readRef $ thisRef
            modifyRef seen . insert thisID `returning` frozen
         where
            (thisID, thisRef) =
               (identify &&& conflate) . getClay $ mutable

-- | Freeze a Clay using the identity transformation, so that a Clay s f turns into a Fix f.
freeze :: (Traversable f) => Clay s f -> Squishy s (Fix f)
freeze = freezeWith id

runFreezingWith :: (Traversable f) => (f ~> g) -> (forall s. Squishy s (Clay s f)) -> Fix g
runFreezingWith transform action = runSquishy (action >>= freezeWith transform)

runFreezing :: (Traversable f) => (forall s. Squishy s (Clay s f)) -> Fix f
runFreezing = runFreezingWith id
