{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , KindSignatures
           , CPP
  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Internal
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
--------------------------------------------------------------------------------

module System.IO.SaferFileHandles.Internal where

-- from base:
import Control.Monad    ( return, (>>=), fail )
import Data.Function    ( ($) )
import Data.Maybe       ( Maybe(Nothing, Just) )
import System.IO.Error  ( modifyIOError )
import GHC.IO.Exception ( ioe_handle )

-- from transformers:
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- from regions:
import Control.Monad.Trans.Region        ( Dup(dup) )
import Control.Monad.Trans.Region.OnExit ( CloseHandle )

-- from explicit-iomodes
import System.IO.ExplicitIOModes ( Handle, IO )

#ifdef __HADDOCK__
import System.IO.ExplicitIOModes (IOMode)
#endif


--------------------------------------------------------------------------------
-- * Files with explicit IO modes as scarce resources
--------------------------------------------------------------------------------

-- | A regional handle to an opened file parameterized by the 'IOMode' in which
-- you opened the file and the region in which it was created.
data RegionalFileHandle ioMode (r ∷ * → *) =
    RegionalFileHandle (Handle ioMode) (Maybe (CloseHandle r))

instance Dup (RegionalFileHandle ioMode) where
    dup (RegionalFileHandle h Nothing)   = return $ RegionalFileHandle h Nothing
    dup (RegionalFileHandle h (Just ch)) = do ch' ← dup ch
                                              return $ RegionalFileHandle h
                                                     $ Just ch'


--------------------------------------------------------------------------------
-- Utility wrapping functions
--------------------------------------------------------------------------------

regularHandle ∷ RegionalFileHandle ioMode r → Handle ioMode
regularHandle (RegionalFileHandle h _) = h

wrap ∷ MonadIO m
     ⇒ (Handle ioMode → IO α)
     → (RegionalFileHandle ioMode r → m α)
wrap f = \h → liftIO $ sanitizeIOError $ f (regularHandle h)

wrap2 ∷ MonadIO m
      ⇒ (Handle ioMode → β → IO α)
      → (RegionalFileHandle ioMode r → β → m α)
wrap2 f = \h y → liftIO $ sanitizeIOError $ f (regularHandle h) y

wrap3 ∷ MonadIO m
      ⇒ (Handle ioMode → γ → β → IO α)
      → (RegionalFileHandle ioMode r → γ → β → m α)
wrap3 f = \h z y → liftIO $ sanitizeIOError $ f (regularHandle h) z y

sanitizeIOError ∷ IO α → IO α
sanitizeIOError = modifyIOError $ \e → e { ioe_handle = Nothing }


-- The End ---------------------------------------------------------------------

