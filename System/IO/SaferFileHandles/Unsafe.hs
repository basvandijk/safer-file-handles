{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.IO.SaferFileHandles.Unsafe
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- /Unsafe/ functions for retrieving the native @Handle@ from a regional file
-- handle and for lifting operations on @Handles@ to @RegionalFileHandles@.
--
-- These operations are unsafe because they allow you to close regional file
-- handles before exiting their region. So they enable you to perform @IO@ with
-- already closed handles.
--
--------------------------------------------------------------------------------

module System.IO.SaferFileHandles.Unsafe ( unsafeHandle
                                         , wrap, wrap2, wrap3
                                         , sanitizeIOError
                                         ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function                     ( ($) )
import Data.Maybe                        ( Maybe(Nothing) )
import GHC.IO.Exception                  ( ioe_handle )
import System.IO.Error                   ( modifyIOError )

-- from transformers:
import Control.Monad.IO.Class            ( MonadIO, liftIO )

-- from explicit-iomodes
import System.IO.ExplicitIOModes         ( Handle, IO )

-- from ourselves:
import System.IO.SaferFileHandles.Internal ( RegionalFileHandle(RegionalFileHandle) )


--------------------------------------------------------------------------------
-- Getting the actual @Handle@
--------------------------------------------------------------------------------

unsafeHandle ∷ RegionalFileHandle ioMode r → Handle ioMode
unsafeHandle (RegionalFileHandle h _) = h

wrap ∷ MonadIO m
     ⇒ (Handle ioMode → IO α)
     → (RegionalFileHandle ioMode r → m α)
wrap f = \h → liftIO $ sanitizeIOError $ f (unsafeHandle h)

wrap2 ∷ MonadIO m
      ⇒ (Handle ioMode → β → IO α)
      → (RegionalFileHandle ioMode r → β → m α)
wrap2 f = \h y → liftIO $ sanitizeIOError $ f (unsafeHandle h) y

wrap3 ∷ MonadIO m
      ⇒ (Handle ioMode → γ → β → IO α)
      → (RegionalFileHandle ioMode r → γ → β → m α)
wrap3 f = \h z y → liftIO $ sanitizeIOError $ f (unsafeHandle h) z y

-- | Modify thrown @IOErrors@ in the given computation by erasing the
-- 'ioe_handle' field in the @IOError@ which may contain the @Handle@ which
-- caused the @IOError@.
--
-- I use this to ensure that @Handles@ don't /leak/ out the region via
-- exceptions.
sanitizeIOError ∷ IO α → IO α
sanitizeIOError = modifyIOError $ \e → e { ioe_handle = Nothing }


-- The End ---------------------------------------------------------------------
