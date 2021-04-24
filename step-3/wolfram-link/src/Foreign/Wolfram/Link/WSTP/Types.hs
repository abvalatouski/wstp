module Foreign.Wolfram.Link.WSTP.Types
    ( -- * WSTP types
      EnvParam
    , Env
    , Link

      -- * Convenient C type wrappers
    , Bool (Bool, getBool)
    , unlessM
    )
  where

import Control.Monad
import Foreign
import Foreign.C
import Prelude       hiding (Bool)

data EnvParam
data Env
data Link

newtype Bool = Bool { getBool :: CInt }
  deriving Show

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condition branch = do
    Bool result <- condition
    unless (result == 0)
        branch

-- Orphans!
deriving instance Show     Errno
deriving instance Storable Errno
