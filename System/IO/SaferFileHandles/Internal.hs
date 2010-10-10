{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , KindSignatures
           , CPP
  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.IO.SaferFileHandles.Internal
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
--------------------------------------------------------------------------------

module System.IO.SaferFileHandles.Internal
    ( RegionalFileHandle(RegionalFileHandle) ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad                     ( return, liftM )
import Data.Function                     ( ($) )
import Data.Maybe                        ( Maybe(Nothing, Just) )

-- from base-unicode-symbols:
import Data.Function.Unicode             ( (∘) )

-- from regions:
import Control.Monad.Trans.Region        ( Dup(dup) )
import Control.Monad.Trans.Region.OnExit ( FinalizerHandle )

-- from explicit-iomodes
import System.IO.ExplicitIOModes         ( Handle )

#ifdef __HADDOCK__
import System.IO.ExplicitIOModes         ( IOMode )
#endif


--------------------------------------------------------------------------------
-- * Files with explicit IO modes as scarce resources
--------------------------------------------------------------------------------

-- | A regional handle to an opened file parameterized by the 'IOMode' in which
-- you opened the file and the region in which it was created.
data RegionalFileHandle ioMode (r ∷ * → *) =
    RegionalFileHandle !(Handle ioMode) !(Maybe (FinalizerHandle r))

instance Dup (RegionalFileHandle ioMode) where
    dup (RegionalFileHandle h Nothing)   = return $ RegionalFileHandle h Nothing
    dup (RegionalFileHandle h (Just ch)) = liftM (RegionalFileHandle h ∘ Just) $ dup ch


-- The End ---------------------------------------------------------------------

