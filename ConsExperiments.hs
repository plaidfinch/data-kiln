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

instance (Show (f (g a))) => Show (Compose f g a) where
    show = show . getCompose

data Distinct x = Distinct Unique x
   deriving ( Functor , Foldable , Traversable , Eq , Ord )

distinguish :: MonadUnique m => x -> m (Distinct x)
distinguish x = flip Distinct x <$> newUnique

identity :: Distinct x -> Unique
identity (Distinct u _) = u

undistinguish :: Distinct x -> x
undistinguish (Distinct _ x) = x

class (Functor m, Monad m) => MonadUnique m where
   newUnique :: m Unique
instance MonadUnique IO where
   newUnique = U.newUnique
instance MonadUnique (ST s) where
   newUnique = unsafeIOToST U.newUnique

data Pair x = Pair { car :: x , cdr :: x }
   deriving ( Functor , Foldable , Traversable , Show , Eq , Ord )

setCar, setCdr ::  x -> Pair x -> Pair x
setCar a' (Pair a b) = Pair a' b
setCdr b' (Pair a b) = Pair a  b'

instance Applicative Pair where
   pure x = Pair x x
   (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

type RefStruct s f = Fix (Compose (Compose Distinct (STRef s)) f)

unFixCompose2 :: Fix (Compose (Compose f g) h) -> f (g (h (Fix (Compose (Compose f g) h))))
unFixCompose2 = getCompose . getCompose . unFix

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

test :: IO ()
test = print $ runST $ do
   x <- new (Compose ('x',[]))
   y <- new (Compose ('y',[]))
   z <- new (Compose ('z',[x,y]))
   freeze z
