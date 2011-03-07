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
    ( RegionalFileHandle(RegionalFileHandle)
    , FileHandle(unsafeHandle)
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad                     ( liftM )

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
    RegionalFileHandle !(Handle ioMode) !(FinalizerHandle r)

instance Dup (RegionalFileHandle ioMode) where
    dup (RegionalFileHandle h ch) = liftM (RegionalFileHandle h) (dup ch)

class FileHandle (handle ∷ * → (* → *) → *) where
    unsafeHandle ∷ handle ioMode r → Handle ioMode

instance FileHandle RegionalFileHandle where
    unsafeHandle (RegionalFileHandle handle _) = handle


-- The End ---------------------------------------------------------------------

