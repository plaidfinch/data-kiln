{-# LANGUAGE RankNTypes #-}

module Midas
   ( RefStruct
   , ref
   , deref
   , modify
   , composedly
   , freeze
   , freezeWith
   , module Data.Fix
   , module Data.Functor.Compose
   , module Data.Traversable
   , module Control.Monad.ST.Lazy
   ) where

import STDistinct

import Data.Fix
import Data.Functor.Compose
import Data.Traversable

import Control.Monad
import Control.Monad.IfElse
import Control.Applicative hiding ( empty )
import Control.Arrow

import Control.Monad.ST.Lazy
import Data.STRef.Lazy

import Prelude hiding ( lookup , foldr )
import Data.Map ( Map , insert , empty , lookup )

-- | A RefStruct is a mutable ST reference, tagged with a unique identity, containing a Traversable value of type f, which itself contains more RefStructs; in other words, it's a structure made of mutable references with reference identity which can point to one another.
newtype RefStruct s f = RefStruct { getStruct :: STDistinct s (STRef s (f (RefStruct s f))) }

-- | Take a functor f containing a RefStruct of f, and wrap it in a mutable reference and a distinct tag, thus returning a new RefStruct of f.
ref :: f (RefStruct s f) -> ST s (RefStruct s f)
ref = (RefStruct <$>) . (distinguish =<<) . newSTRef

-- | Takes a RefStruct f and exposes the first level of f inside it.
deref :: RefStruct s f -> ST s (f (RefStruct s f))
deref = readSTRef . undistinguish . getStruct

-- | Apply a function (destructively) to the STRef contained in a RefStruct.
modify :: RefStruct s f -> (f (RefStruct s f) -> f (RefStruct s f)) -> ST s ()
modify = modifySTRef . undistinguish . getStruct

-- | Apply a function to the value inside a Compose.
composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

-- | Given a RefStruct s f, use a natural transformation (forall a. f a -> g a) to convert it into the fixed-point of a the functor g by eliminating the indirection of the mutable references and using the distinct tags on the structure's parts to tie knots where there are cycles in the original graph of references. TThe result is an immutable cyclic lazy data structure.
freezeWith :: (Traversable f) => (forall a. f a -> g a) -> RefStruct s f -> ST s (Fix g)
freezeWith transform = (newSTRef empty >>=) . flip freeze'
   where
      freeze' seen struct =
         aifM (lookup structID <$> readSTRef seen) return $ do
            frozen <- (Fix . transform <$>)  $ traverse (freeze' seen) =<< readSTRef structRef
            modifySTRef seen . insert structID `returning` frozen
         where
            (structID, structRef) =
               (identity &&& undistinguish) . getStruct $ struct

freeze :: (Traversable f) => RefStruct s f -> ST s (Fix f)
freeze = freezeWith id
