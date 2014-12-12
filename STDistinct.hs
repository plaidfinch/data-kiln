module STDistinct
   ( STDistinct
   , identify
   , conflate
   , distinguish
   ) where

import STUnique

import Control.Monad.ST.Lazy ( ST )
import Control.Applicative   ( (<$>) )

-- | An STDistinct is anything paired with a unique value. STDistinct values are guaranteed to be distinct from one another within any given run of a program; as such, they are intended to be used to implement reference identity in the ST monad.
data STDistinct s x = STDistinct
   { identify :: STUnique s -- The unique identity of an STDistinct
   , conflate :: x          -- The raw (and possibly non-distinct) value contained within
   } deriving ( Eq , Ord )

-- | The only way to construct an STDistinct value is to distinguish it; it is impossible to take the unique 'identity' of an STDistinct and swap it into the identity field of another.
distinguish :: x -> ST s (STDistinct s x)
distinguish x = flip STDistinct x <$> newSTUnique
