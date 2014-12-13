module Data.Kiln.Examples where

import Data.Kiln

import Control.Arrow
import Data.List
import Data.Functor.Compose
import Data.Traversable
import Data.Foldable

-- | Apply a function to the value inside a Compose.
composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

type MNode s n e = Clay s (Compose (Compose ((,) n) []) ((,) e))
type Node    n e =    Fix (Compose (Compose ((,) n) []) ((,) e))

node :: n -> [(e,MNode s n e)] -> Squishy s (MNode s n e)
node tag list = newClay (Compose (Compose (tag,list)))

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
