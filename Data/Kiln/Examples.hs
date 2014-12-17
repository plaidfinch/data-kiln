module Data.Kiln.Examples where

import Data.Kiln

import Control.Arrow
import Data.List
import Data.Functor.Compose
import Data.Traversable
import Data.Foldable
import Control.Applicative

-- | Apply a function to the value inside a Compose.
composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

-- Mutable singly-linked lists built from cons-cells

type MSLL s a = Clay s (Compose ((,) a) Maybe)
type SLL    a =    Fix (Compose ((,) a) Maybe)

cons :: a -> Maybe (MSLL s a) -> Squishy s (MSLL s a)
cons car cdr = newClay (Compose (car, cdr))

setCar :: MSLL s a -> a -> Squishy s ()
setCar x = modifyClay x . composedly . first  . const

setCdr :: MSLL s a -> Maybe (MSLL s a) -> Squishy s ()
setCdr x = modifyClay x . composedly . second . const

list1 :: SLL Char
list1 = runKilning $ do
   a <- cons 'a' Nothing
   b <- cons 'b' (Just a)
   c <- cons 'c' (Just b)
   setCdr a $ Just c
   return c

sllToList :: SLL a -> [a]
sllToList sll = case (getCompose . unFix) sll of
   (x,Nothing) -> [x]
   (x,Just xs) -> x : sllToList xs

-- Mutable graphs with node and edge labels

type MNode s n e = Clay s (Compose (Compose ((,) n) []) ((,) e))
type Node    n e =    Fix (Compose (Compose ((,) n) []) ((,) e))

node :: n -> [(e, MNode s n e)] -> Squishy s (MNode s n e)
node n list = newClay (Compose (Compose (n,list)))

emptyNode :: n -> Squishy s (MNode s n e)
emptyNode n = node n []

readNode :: MNode s n e -> Squishy s (n, [(e, MNode s n e)])
readNode = fmap (getCompose . getCompose) . readClay

relabelNode :: n -> MNode s n e -> Squishy s ()
relabelNode n = flip modifyClay (composedly . composedly . first . const $ n)

editEdges :: ([(e, MNode s n e)] -> [(e, MNode s n e)]) -> MNode s n e -> Squishy s ()
editEdges f = flip modifyClay (composedly . composedly . second $ f)

addEdge :: e -> MNode s n e -> MNode s n e -> Squishy s ()
addEdge label from to = editEdges ((label, to) :) from

graph1 :: Node String String
graph1 = runKilning $ do
   a <- emptyNode "a"
   b <- emptyNode "b"
   c <- emptyNode "c"
   d <- emptyNode "d"
   addEdge "a -> b" a b
   addEdge "b -> c" b c
   addEdge "c -> d" c d
   addEdge "c -> a" c a
   addEdge "d -> a" d a
   return a
