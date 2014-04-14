module STUnique
   ( STUnique
   , newSTUnique
   ) where

import           Data.Unique ( Unique )
import qualified Data.Unique as U
import Control.Monad.ST.Lazy hiding ( unsafeIOToST )
import Control.Monad.ST.Lazy.Unsafe
import Control.Applicative

-- | An STUnique is a 'Unique' value which can't be leaked from the ST monad, and is thus safe.
newtype STUnique s = STUnique Unique deriving ( Eq , Ord )

-- | Generate a new STUnique in the ST monad.
newSTUnique :: ST s (STUnique s)
newSTUnique = STUnique <$> unsafeIOToST U.newUnique
