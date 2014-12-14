{-# LANGUAGE RankNTypes #-}

module Data.Kiln
   ( Clay , newClay , readClay , modifyClay , writeClay , identifyClay
   , kiln , kilnWith
   , runKilningWith , runKilning
   , module Data.Fix
   , module Data.Traversable
   , module Control.Monad.Squishy
   ) where

import Control.Monad.Squishy

import Data.Fix
import Data.Traversable

import Control.Monad
import Control.Monad.State
import Control.Monad.IfElse
import Control.Applicative hiding ( empty )
import Control.Arrow

import           Data.Map ( Map )
import qualified Data.Map as M

-- | A @Clay s f@ is a recursive structure with a uniquely-identified mutable
--   reference at each node. It cannot escape the @Squishy@ monad, by means of
--   the same mechanism as used in @Control.Monad.ST@.
data Clay s f = Clay { getClay :: Distinct s (Ref s (f (Clay s f))) }

-- | Take a container @f (Clay s f)@, and wrap it in a new mutable reference and
--   distinct tag, returning a new @Clay s f@.
newClay :: f (Clay s f) -> Squishy s (Clay s f)
newClay = (Clay <$>) . (distinguish =<<) . newRef

-- | Takes a @Clay s f@ and exposes the first level of @f (Clay s f)@ inside it.
readClay :: Clay s f -> Squishy s (f (Clay s f))
readClay = readRef . conflate . getClay

-- | Use the provided function to destructively update the value at this node of
--   a @Clay s f@.
modifyClay :: Clay s f -> (f (Clay s f) -> f (Clay s f)) -> Squishy s ()
modifyClay = modifyRef . conflate . getClay

-- | Set a piece of @Clay@ to a particular value.
writeClay :: Clay s f -> (f (Clay s f)) -> Squishy s ()
writeClay = writeRef . conflate . getClay

-- | Get the unique @Identifier@ for a piece of @Clay@.
identifyClay :: Clay s f -> Identifier s
identifyClay = identify . getClay

-- | Given a @Clay s f@, use a natural transformation @(forall a. f a -> g a)@
--   to convert it into the fixed-point of a the functor @g@ by eliminating the
--   indirection of the mutable references and using the distinct tags on the
--   structure's parts to tie knots where there are cycles in the original graph
--   of references. TThe result is an immutable cyclic lazy data structure.
kilnWith :: (Traversable f) => (forall a. f a -> g a) -> Clay s f -> Squishy s (Fix g)
kilnWith transform = flip evalStateT M.empty . kiln'
   where
      kiln' clay =
         aifM (M.lookup (identifyClay clay) <$> get) return $ do
            baked <- (Fix . transform <$>) . traverse kiln' =<< lift (readClay clay)
            modify (M.insert (identifyClay clay) baked) >> return baked

-- | Freeze a Clay using the identity transformation, so that a Clay s f turns into a Fix f.
kiln :: (Traversable f) => Clay s f -> Squishy s (Fix f)
kiln = kilnWith id

-- | Given a @Squishy@ monad action which returns a @Clay s f@, run the action
--   and use the provided natural transformation during baking (see 'kilnWith').
runKilningWith :: (Traversable f) => (forall a. f a -> g a) -> (forall s. Squishy s (Clay s f)) -> Fix g
runKilningWith transform action = runSquishy (action >>= kilnWith transform)

-- | Given a @Squishy@ monad action which returns a @Clay s f@, run the action
--   and kiln the result.
runKilning :: (Traversable f) => (forall s. Squishy s (Clay s f)) -> Fix f
runKilning = runKilningWith id
