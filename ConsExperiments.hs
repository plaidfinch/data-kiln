{-# LANGUAGE StandaloneDeriving #-}

module ConsExperiments where

import Control.Applicative
import Data.IORef
import Data.Unique

data MCons a b = MCons { mconsID :: Unique , mcar :: IORef a , mcdr :: IORef b }

data Cons a b = Cons { consID :: Unique , car :: a , cdr :: b }

data TreeFix n l r = TreeIn { treeOut :: n (l (TreeFix n l r))
                                           (r (TreeFix n l r)) }

mcons :: a -> b -> IO (MCons a b)
mcons a b = MCons <$> newUnique <*> newIORef a <*> newIORef b

mconsF :: l (TreeFix MCons l r) -> r (TreeFix MCons l r) -> IO (TreeFix MCons l r)
mconsF a b = TreeIn <$> mcons a b

getCar :: MCons a b -> IO a
getCar = readIORef . mcar

getCarF :: TreeFix MCons l r -> IO (l (TreeFix MCons l r))
getCarF = getCar . treeOut

getCdr :: MCons a b -> IO b
getCdr = readIORef . mcdr

getCdrF :: TreeFix MCons l r -> IO (r (TreeFix MCons l r))
getCdrF = getCdr . treeOut

setCar :: MCons a b -> a -> IO ()
setCar c x = writeIORef (mcar c) x

setCarF :: TreeFix MCons l r -> l (TreeFix MCons l r) -> IO ()
setCarF = setCar . treeOut

setCdr :: MCons a b -> b -> IO ()
setCdr c x = writeIORef (mcdr c) x

setCdrF :: TreeFix MCons l r -> r (TreeFix MCons l r) -> IO ()
setCdrF = setCdr . treeOut

main :: IO ()
main = do
   x <- mconsF (Left 1) (Left 2)
   setCdrF x (Right x)
   either print (const $ return ()) =<< getCarF x
