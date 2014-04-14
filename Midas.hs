module Midas
   ( RefStruct
   , newStruct
   , modifyStruct
   , composedly
   , freeze
   , module Data.Fix
   , module Data.Functor.Compose
   , module Data.Traversable
   , module Control.Monad.ST.Lazy
   ) where

import STDistinct

import Data.Fix
import Data.Functor
import Data.Functor.Compose
import Data.Traversable

import Control.Monad
import Control.Applicative hiding ( empty )
import Control.Arrow

import Control.Monad.ST.Lazy
import Data.STRef.Lazy

import Prelude hiding ( lookup )
import Data.Map ( insert , member , empty , lookup )

-- | A RefStruct is a mutable ST reference, tagged with a unique identity, containing a Traversable value of type f, which itself contains more RefStructs; in other words, it's a structure made of mutable references with reference identity which can point to one another.
type RefStruct s f = Fix (Compose (Compose (STDistinct s) (STRef s)) f)

-- | Take a functor f containing a RefStruct of f, and wrap it in a mutable reference and a distinct tag, thus returning a new RefStruct of f.
newStruct :: f (RefStruct s f) -> ST s (RefStruct s f)
newStruct = (Fix . Compose . Compose <$>) . (distinguish =<<) . newSTRef

-- | Apply a function (destructively) to the STRef contained in a RefStruct.
modifyStruct :: RefStruct s f -> (f (RefStruct s f) -> f (RefStruct s f)) -> ST s ()
modifyStruct = modifySTRef . undistinguish . unFixCompose2

-- | Apply a function to the value inside a Compose.
composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

-- | Given a RefStruct s f, convert it into the fixed-point of a the functor f by eliminating the indirection of the mutable references and using the distinct tags on the structure's parts (that is, the pseudo-reference-identity we've made) to tie the structure into a knot where there are cycles in the original graph of references. The result is an immutable cyclic lazy data structure isomorphic to its input.
freeze :: (Traversable f) => RefStruct s f -> ST s (Fix f)
freeze = (newSTRef empty >>=) . flip freeze'
   where
      freeze' seen struct = do
         maybeSeen <- lookup structID <$> readSTRef seen
         flip (flip maybe return) maybeSeen $ do
            frozen <- (Fix <$>) $ traverse (freeze' seen) =<< readSTRef structRef
            modifySTRef seen (insert structID frozen) $> frozen
         where
            (structID, structRef) =
               (identity &&& undistinguish) . unFixCompose2 $ struct

-- Unwrap the three nested functors inside a (Fix (Compose (Compose f g) h)).
unFixCompose2 :: Fix (Compose (Compose f g) h) -> f (g (h (Fix (Compose (Compose f g) h))))
unFixCompose2 = getCompose . getCompose . unFix
