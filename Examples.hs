 {-# LANGUAGE RecursiveDo #-}

module MidasExamples where

import Midas

graph1 :: Node Char Char
graph1 = runST $ do
   rec a <- node 'a' [('b',b)]
       b <- node 'b' [('c',c),('d',d)]
       c <- node 'c' []
       d <- node 'd' []
   freeze a

node tag list = newStruct (Compose (Compose (tag,list)))

type Node n e = Fix (Compose (Compose ((,) n) []) ((,) e))

