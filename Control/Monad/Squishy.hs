{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Squishy 
   ( Squishy , runSquishy
   , Identifier
   , Distinct , distinguish , conflate , identify
   , Ref , newRef , readRef , writeRef , modifyRef
   ) where

import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

-- | The Squishy monad is a monad with mutable references and optional reference
--   identity
newtype Squishy s a =
   Squishy { getSquishy :: StateT ID (ST s) a }
   deriving ( Functor, Applicative, Monad )

-- | Runs a Squishy computation, returning a pure value
runSquishy :: forall a. (forall s. Squishy s a) -> a
runSquishy x = runST $ evalStateT (getSquishy x) initialID

-- | A unique identifier. Only possible to create while making a Distinct value.
newtype Identifier s = Identifier ID deriving ( Eq, Ord )

-- | Data with faked reference equality; the interface provided guarantees that
--   every Distinct value has a unique Identifier.
data Distinct s a =
   Distinct a (Identifier s)

instance Eq (Distinct s a) where
   a == b = identify a == identify b

instance Ord (Distinct s a) where
   compare a b = compare (identify a) (identify b)

-- | The only way to create a Distinct value is to generate a new identifier for
--   it in the Squishy monad.
distinguish :: a -> Squishy s (Distinct s a)
distinguish a = Distinct a <$> newIdentifier
   where
      newIdentifier = Squishy $ do
         x <- get
         put (succ x)
         return (Identifier x)

-- | Extracts the value stored in a Distinct
conflate :: Distinct s a -> a
conflate (Distinct a _) = a

-- | Extracts the unique identifier for a Distinct
identify :: Distinct s a -> Identifier s
identify (Distinct _ i) = i

-- The mutable ref API from Data.STRef, lifted through to the Squishy monad

-- | A mutable reference in the @Squishy@ monad.
newtype Ref s a = Ref (STRef s a)

-- | Make a new reference.
newRef :: a -> Squishy s (Ref s a)
newRef a = Squishy $ lift (Ref <$> newSTRef a)

-- | Read the value of a reference.
readRef :: Ref s a -> Squishy s a
readRef (Ref r) = Squishy $ lift (readSTRef r)

-- | Write a new value to a reference.
writeRef :: Ref s a -> a -> Squishy s ()
writeRef (Ref r) a = Squishy $ lift (writeSTRef r a)

-- | Use the provided function to modify the contained value in a reference.
modifyRef :: Ref s a -> (a -> a) -> Squishy s ()
modifyRef (Ref r) a = Squishy $ lift (modifySTRef r a)

-- This type is not exposed, it's used internally to implement unique identifiers
type ID = Int
initialID :: ID
initialID = minBound
