 {-# LANGUAGE RecursiveDo #-}

module MidasExamples where

import Midas
import Control.Monad.Squishy

import Control.Arrow
import Data.List
import Data.Traversable

type MNode s n e = Clay s (Compose (Compose ((,) n) []) ((,) e))
type Node    n e =    Fix (Compose (Compose ((,) n) []) ((,) e))

node :: n -> [(e,MNode s n e)] -> Squishy s (MNode s n e)
node tag list = ref (Compose (Compose (tag,list)))

--graph1 :: Node Char Char
--graph1 = runSquishy $ do
--   rec a <- node 'a' [('b',b)]
--       b <- node 'b' [('c',c),('d',d)]
--       c <- node 'c' []
--       d <- node 'd' []
--   freeze a

type MSLL s a = Clay s (Compose ((,) a) Maybe)
type SLL    a =    Fix (Compose ((,) a) Maybe)

cons :: a -> Maybe (MSLL s a) -> Squishy s (MSLL s a)
cons car cdr = ref (Compose (car,cdr))

setCar :: MSLL s a -> a -> Squishy s ()
setCar x = modify x . composedly . first  . const

setCdr :: MSLL s a -> Maybe (MSLL s a) -> Squishy s ()
setCdr x = modify x . composedly . second . const

list1 :: SLL Char
list1 = runFreezing $ do
   a <- cons 'a' Nothing
   b <- cons 'b' (Just a)
   c <- cons 'c' (Just b)
   setCdr a $ Just b
   return a

sllToList :: SLL a -> [a]
sllToList sll = case (getCompose . unFix) sll of
   (x,Nothing) -> [x]
   (x,Just xs) -> x : sllToList xs
