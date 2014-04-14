 {-# LANGUAGE RecursiveDo #-}

module MidasExamples where

import Midas

graph1 :: Fix (Compose ((,) Char) [])
graph1 = runST $ do
   rec a <- node 'a' [b,c,d]
       b <- node 'b' [c,d]
       c <- node 'c' [d]
       d <- node 'd' [a]
   freeze a

node :: t -> f (RefStruct s (Compose ((,) t) f)) -> ST s (RefStruct s (Compose ((,) t) f))
node tag list = newStruct (Compose (tag,list))

