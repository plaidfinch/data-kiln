{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Squishy 
   ( Squishy , runSquishy
   , Identifier
   , Distinct , distinguish, conflate, identify
   , Ref , newRef , readRef , writeRef , modifyRef
   ) where

import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Control.Monad.Reader
import Control.Applicative

type ID = Int

initialID :: ID
initialID = 0

-- | The Squishy monad is a monad with mutable references and optional reference identity
newtype Squishy s a =
   Squishy { getSquishy :: ReaderT (STRef s ID) (ST s) a }
   deriving ( Functor, Applicative, Monad )

-- | Runs a Squishy computation, returning a pure value
runSquishy :: forall a. (forall s. Squishy s a) -> a
runSquishy x = runST $ newSTRef initialID >>= runReaderT (getSquishy x)
--runSquishy (Squishy x) = runST $ evalStateT x initialID -- doesn't work, though it should be equivalent

-- | A unique identifier. Only possible to create while making a Distinct value.
newtype Identifier s = Identifier ID deriving ( Eq, Ord )

-- | Data with faked reference equality; the interface provided guarantees that every Distinct value has a unique Identifier.
data Distinct s a =
   Distinct a (Identifier s)
   deriving ( Eq, Ord )

-- | The only way to create a Distinct value is to generate a new identifier for it in the Squishy monad.
distinguish :: a -> Squishy s (Distinct s a)
distinguish a = Distinct a <$> newIdentifier
   where
      newIdentifier = Squishy $ do
         r <- ask
         x <- lift $ readSTRef r
         lift $ writeSTRef r (succ x)
         return (Identifier x)

-- | Extracts the value stored in a Distinct
conflate :: Distinct s a -> a
conflate (Distinct a _) = a

-- | Extracts the unique identifier for a Distinct
identify :: Distinct s a -> Identifier s
identify (Distinct _ i) = i

-- | Compares by reference identity
(===) :: Distinct s a -> Distinct s b -> Bool
x === y = identify x == identify y

-- The mutable reference API from Data.STRef, lifted through to the Squishy monad

newtype Ref s a = Ref (STRef s a)

newRef :: a -> Squishy s (Ref s a)
newRef a = Squishy $ lift (Ref <$> newSTRef a)

readRef :: Ref s a -> Squishy s a
readRef (Ref r) = Squishy $ lift (readSTRef r)

writeRef :: Ref s a -> a -> Squishy s ()
writeRef (Ref r) a = Squishy $ lift (writeSTRef r a)

modifyRef :: Ref s a -> (a -> a) -> Squishy s ()
modifyRef (Ref r) a = Squishy $ lift (modifySTRef r a)
