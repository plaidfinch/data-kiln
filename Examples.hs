 {-# LANGUAGE RecursiveDo #-}

module MidasExamples where

import Midas

import Control.Arrow
import Data.List
import Data.Traversable

type MNode s n e = RefStruct s (Compose (Compose ((,) n) []) ((,) e))
type Node    n e =         Fix (Compose (Compose ((,) n) []) ((,) e))

node :: n -> [(e,MNode s n e)] -> ST s (MNode s n e)
node tag list = ref (Compose (Compose (tag,list)))

graph1 :: Node Char Char
graph1 = runST $ do
   rec a <- node 'a' [('b',b)]
       b <- node 'b' [('c',c),('d',d)]
       c <- node 'c' []
       d <- node 'd' []
   freeze a

type MSLL s a = RefStruct s (Compose ((,) a) Maybe)
type SLL    a =         Fix (Compose ((,) a) Maybe)

cons :: a -> Maybe (MSLL s a) -> ST s (MSLL s a)
cons car cdr = ref (Compose (car,cdr))

setCar :: MSLL s a -> a -> ST s ()
setCar x = modify x . composedly . first  . const

setCdr :: MSLL s a -> Maybe (MSLL s a) -> ST s ()
setCdr x = modify x . composedly . second . const

list1 :: SLL Char
list1 = runST $ do
   a <- cons 'a' Nothing
   b <- cons 'b' Nothing
   c <- cons 'c' Nothing
   setCdr a $ Just b
   setCdr b $ Just c
   setCdr c $ Just a
   freeze a

sllToList :: SLL a -> [a]
sllToList sll = case (getCompose . unFix) sll of
   (x,Nothing) -> [x]
   (x,Just xs) -> x : sllToList xs
