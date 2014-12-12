{-# LANGUAGE RankNTypes #-}

module Data.Kiln
   ( Clay , newClay , readClay , modifyClay
   , kiln , kilnWith
   , runKilningWith , runKilning
   , module Data.Fix
   , module Data.Traversable
   , module Control.Monad.Squishy
   ) where

import Control.Monad.Squishy

import Data.Fix
import Data.Functor.Compose
import Data.Traversable

import Control.Monad
import Control.Monad.State
import Control.Monad.IfElse
import Control.Applicative hiding ( empty )
import Control.Arrow

import           Data.Map ( Map )
import qualified Data.Map as M

-- | A Clay is a recursive structure with a uniquely-identified mutable reference at each node
data Clay s f = Clay { getClay :: Distinct s (Ref s (f (Clay s f))) }

-- | Take a functor f containing a Clay of f, and wrap it in a mutable reference and a distinct tag, thus returning a new Clay of f.
newClay :: f (Clay s f) -> Squishy s (Clay s f)
newClay = (Clay <$>) . (distinguish =<<) . newRef

-- | Takes a Clay f and exposes the first level of f inside it.
readClay :: Clay s f -> Squishy s (f (Clay s f))
readClay = readRef . conflate . getClay

-- | Apply a function (destructively) to the STRef contained in a Clay.
modifyClay :: Clay s f -> (f (Clay s f) -> f (Clay s f)) -> Squishy s ()
modifyClay = modifyRef . conflate . getClay

-- | Given a Clay s f, use a natural transformation (forall a. f a -> g a) to convert it into the fixed-point of a the functor g by eliminating the indirection of the mutable references and using the distinct tags on the structure's parts to tie knots where there are cycles in the original graph of references. TThe result is an immutable cyclic lazy data structure.
kilnWith :: (Traversable f) => (forall a. f a -> g a) -> Clay s f -> Squishy s (Fix g)
kilnWith transform = flip evalStateT M.empty . kiln'
   where
      kiln' clay =
         aifM (M.lookup thisID <$> get) return $ do
            baked <- (Fix . transform <$>) . traverse kiln' =<< lift (readClay clay)
            modify (M.insert thisID baked) >> return baked
         where
            thisID = identify (getClay clay)

-- | Freeze a Clay using the identity transformation, so that a Clay s f turns into a Fix f.
kiln :: (Traversable f) => Clay s f -> Squishy s (Fix f)
kiln = kilnWith id

runKilningWith :: (Traversable f) => (forall a. f a -> g a) -> (forall s. Squishy s (Clay s f)) -> Fix g
runKilningWith transform action = runSquishy (action >>= kilnWith transform)

runKilning :: (Traversable f) => (forall s. Squishy s (Clay s f)) -> Fix f
runKilning = runKilningWith id
