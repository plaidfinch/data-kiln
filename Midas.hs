{-# LANGUAGE FlexibleContexts #-}

module Midas
   ( Distinct
   , distinguish
   , undistinguish
   , identity
   , RefStruct
   , new
   , modify
   , composedly
   , freeze
   ) where

import Data.Fix
import Data.Functor
import Data.Functor.Compose
import Data.Traversable

import Control.Monad
import Control.Applicative
import Control.Arrow

import Control.Monad.ST.Lazy hiding (unsafeIOToST)
import Control.Monad.ST.Lazy.Unsafe
import Data.STRef.Lazy

import           Data.Unique (Unique)
import qualified Data.Unique as U

import           Data.Map (Map)
import qualified Data.Map as M

data Distinct x = Distinct
   { identity      :: Unique
   , undistinguish :: x
   } deriving ( Eq , Ord )

distinguish :: x -> IO (Distinct x)
distinguish x = flip Distinct x <$> U.newUnique

distinguishST :: x -> ST s (Distinct x)
distinguishST = unsafeIOToST . distinguish

type RefStruct s f = Fix (Compose (Compose Distinct (STRef s)) f)

new :: f (RefStruct s f) -> ST s (RefStruct s f)
new = (Fix . Compose . Compose <$>) . (distinguishST =<<) . newSTRef

modify :: RefStruct s f -> (f (RefStruct s f) -> f (RefStruct s f)) -> ST s ()
modify = modifySTRef . undistinguish . unFixCompose2

composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

freeze :: (Traversable f) => RefStruct s f -> ST s (Fix f)
freeze = (newSTRef M.empty >>=) . flip freeze'
   where
      freeze' seen struct = do
         maybeSeen <- M.lookup structID <$> readSTRef seen
         flip (flip maybe return) maybeSeen $ do
            frozen <- (Fix <$>) $ traverse (freeze' seen) =<< readSTRef structRef
            modifySTRef seen (M.insert structID frozen) $> frozen
         where
            (structID, structRef) =
               (identity &&& undistinguish) . unFixCompose2 $ struct

unFixCompose2 :: Fix (Compose (Compose f g) h) -> f (g (h (Fix (Compose (Compose f g) h))))
unFixCompose2 = getCompose . getCompose . unFix

instance (Show (f (g a))) => Show (Compose f g a) where
   show = show . getCompose
