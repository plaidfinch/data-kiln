{-# LANGUAGE DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , FlexibleContexts #-}

module ConsExperiments where

import Data.STRef.Lazy
import Control.Monad.ST.Lazy hiding (unsafeIOToST)
import Control.Monad.ST.Lazy.Unsafe

import Data.Unique (Unique)
import qualified Data.Unique as U

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad
import Control.Applicative
import Control.Arrow

import Data.Traversable
import Data.Foldable
import Data.Monoid
import Data.Functor

import Data.Fix
import Data.Functor.Compose

data Distinct x = Distinct Unique x
   deriving ( Functor , Foldable , Traversable , Eq , Ord )

distinguish :: MonadUnique m => x -> m (Distinct x)
distinguish x = flip Distinct x <$> unsafeIOToST U.newUnique

identity :: Distinct x -> Unique
identity (Distinct u _) = u

undistinguish :: Distinct x -> x
undistinguish (Distinct _ x) = x

type RefStruct s f = Fix (Compose (Compose Distinct (STRef s)) f)

new :: f (RefStruct s f) -> ST s (RefStruct s f)
new = (Fix . Compose . Compose <$>) . (distinguish =<<) . newSTRef

modify :: RefStruct s f -> (f (RefStruct s f) -> f (RefStruct s f)) -> ST s ()
modify = modifySTRef . undistinguish . unFixCompose2

uncomposedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
uncomposedly f = Compose . f . getCompose

freeze :: (Traversable f) => RefStruct s f -> ST s (Fix f)
freeze = (newSTRef M.empty >>=) . flip freeze'
   where
      freeze' seen struct = do
         maybeSeen <- M.lookup structID <$> readSTRef seen
         flip (flip maybe return) maybeSeen $ do
            frozen <- (Fix <$>) $ traverse (freeze' seen) =<< readSTRef structRef
            modifySTRef seen (M.insert structID frozen) $> frozen
         where
            structID  = identity      unwrapped
            structRef = undistinguish unwrapped
            unwrapped = unFixCompose2 struct

unFixCompose2 :: Fix (Compose (Compose f g) h) -> f (g (h (Fix (Compose (Compose f g) h))))
unFixCompose2 = getCompose . getCompose . unFix

instance (Show (f (g a))) => Show (Compose f g a) where
   show = show . getCompose

test :: IO ()
test = print $ runST $ do
   x <- new (Compose ('x',[]))
   y <- new (Compose ('y',[]))
   z <- new (Compose ('z',[x,y]))
   freeze z
