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

import Data.IORef
import Data.Unique

import           Data.Map (Map)
import qualified Data.Map as M

data Distinct x = Distinct
   { identity      :: Unique
   , undistinguish :: x
   } deriving ( Eq , Ord )

distinguish :: x -> IO (Distinct x)
distinguish x = flip Distinct x <$> newUnique

type RefStruct f = Fix (Compose (Compose Distinct IORef) f)

new :: f (RefStruct f) -> IO (RefStruct f)
new = (Fix . Compose . Compose <$>) . (distinguish =<<) . newIORef

modify :: RefStruct f -> (f (RefStruct f) -> f (RefStruct f)) -> IO ()
modify = modifyIORef . undistinguish . unFixCompose2

composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

freeze :: (Traversable f) => RefStruct f -> IO (Fix f)
freeze = (newIORef M.empty >>=) . flip freeze'
   where
      freeze' seen struct = do
         maybeSeen <- M.lookup structID <$> readIORef seen
         flip (flip maybe return) maybeSeen $ do
            frozen <- (Fix <$>) $ traverse (freeze' seen) =<< readIORef structRef
            modifyIORef seen (M.insert structID frozen) $> frozen
         where
            (structID, structRef) =
               (identity &&& undistinguish) . unFixCompose2 $ struct

unFixCompose2 :: Fix (Compose (Compose f g) h) -> f (g (h (Fix (Compose (Compose f g) h))))
unFixCompose2 = getCompose . getCompose . unFix

instance (Show (f (g a))) => Show (Compose f g a) where
   show = show . getCompose
