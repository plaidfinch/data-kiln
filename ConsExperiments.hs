{-# LANGUAGE StandaloneDeriving , FlexibleInstances #-}

module ConsExperiments where

import Control.Applicative
import Control.Monad.ST.Lazy hiding (unsafeIOToST)
import Control.Monad.ST.Lazy.Unsafe
import Control.Monad.State

import Data.STRef.Lazy
import Data.Unique
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M

data MCons s a b = MCons { mconsID :: Unique , mcar :: STRef s a , mcdr :: STRef s b }

data Cons a b = Cons { consID :: Unique , car :: a , cdr :: b }

instance (Show a, Show b) => Show (ConsF (Either a) (Either b)) where
   show (TreeIn (Cons _ a b)) = "TreeIn (Cons _ (" ++ show a ++ ") (" ++ show b ++ "))"

instance Show (ConsF Maybe Maybe) where
   show (TreeIn (Cons _ a b)) = "TreeIn (Cons _ " ++ show a ++ " " ++ show b ++ ")"

data TreeFix n l r = TreeIn { treeOut :: n (l (TreeFix n l r))
                                           (r (TreeFix n l r)) }

type MConsF s l r = TreeFix (MCons s) l r
type  ConsF   l r = TreeFix  Cons     l r

type MEitherConsF s a b = MConsF s (Either a) (Either b)
type  EitherConsF   a b =  ConsF   (Either a) (Either b)

newUniqueST :: ST s Unique
newUniqueST = unsafeIOToST newUnique

mcons :: a -> b -> ST s (MCons s a b)
mcons a b = MCons <$> newUniqueST <*> newSTRef a <*> newSTRef b

mconsF :: l (MConsF s l r) -> r (MConsF s l r) -> ST s (MConsF s l r)
mconsF a b = TreeIn <$> mcons a b

consF :: Unique -> l (ConsF l r) -> r (ConsF l r) -> ConsF l r
consF u a b = TreeIn (Cons u a b)

getCar :: MCons s a b -> ST s a
getCar = readSTRef . mcar

getCarF :: MConsF s l r -> ST s (l (MConsF s l r))
getCarF = getCar . treeOut

getCdr :: MCons s a b -> ST s b
getCdr = readSTRef . mcdr

getCdrF :: MConsF s l r -> ST s (r (MConsF s l r))
getCdrF = getCdr . treeOut

setCar :: MCons s a b -> a -> ST s ()
setCar c x = writeSTRef (mcar c) x

setCarF :: MConsF s l r -> l (MConsF s l r) -> ST s ()
setCarF = setCar . treeOut

setCdr :: MCons s a b -> b -> ST s ()
setCdr c x = writeSTRef (mcdr c) x

setCdrF :: MConsF s l r -> r (MConsF s l r) -> ST s ()
setCdrF = setCdr . treeOut

instance Foldable (Either e) where
   foldMap f (Right m) = f m
   foldMap _ (Left _)  = mempty

instance Traversable (Either e) where
   traverse _ (Left e)  = pure (Left e)
   traverse f (Right x) = Right <$> f x

freeze :: (Traversable f, Traversable g) => MConsF s f g -> ST s (ConsF f g)
freeze c = flip freeze' c =<< newSTRef M.empty
   where
      freeze' seen cell = do
         maybeSeen <- M.lookup cellID <$> readSTRef seen
         flip (flip maybe return) maybeSeen $ do
            l <- traverse (freeze' seen) =<< getCarF cell
            r <- traverse (freeze' seen) =<< getCdrF cell
            let frozenCell = consF cellID l r
            modifySTRef seen (M.insert cellID frozenCell)
            return frozenCell
         where cellID = mconsID $ treeOut cell

main :: IO ()
main = print $ runST $ do
   x <- mconsF Nothing Nothing
   --setCdrF x (Right x)
   freeze x
