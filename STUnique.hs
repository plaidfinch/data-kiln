module STUnique
   ( STUnique
   , newSTUnique
   ) where

import Data.Unique                  ( Unique , newUnique )
import Control.Monad.ST.Lazy        ( ST )
import Control.Monad.ST.Lazy.Unsafe ( unsafeIOToST )
import Control.Applicative          ( (<$>) )

-- | An STUnique is a 'Unique' value which can't be leaked from the ST monad, and is thus safe.
newtype STUnique s = STUnique Unique deriving ( Eq , Ord )

-- | Generate a new STUnique in the ST monad.
newSTUnique :: ST s (STUnique s)
newSTUnique = STUnique <$> unsafeIOToST newUnique
