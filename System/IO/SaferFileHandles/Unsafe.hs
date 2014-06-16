{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.IO.SaferFileHandles.Unsafe
-- Copyright   :  (c) 2010-2011 Bas van Dijk
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

-- from transformers-base:
import Control.Monad.Base                ( MonadBase, liftBase )

-- from explicit-iomodes
import System.IO.ExplicitIOModes         ( Handle, IO )

-- from ourselves:
import System.IO.SaferFileHandles.Internal ( FileHandle, unsafeHandle )


--------------------------------------------------------------------------------
-- Getting the actual @Handle@
--------------------------------------------------------------------------------

wrap :: (FileHandle handle, MonadBase IO m)
     => (Handle ioMode   -> IO a)
     -> (handle ioMode r -> m  a)
wrap f = \h -> liftBase $ sanitizeIOError $ f (unsafeHandle h)

wrap2 :: (FileHandle handle, MonadBase IO m)
      => (Handle ioMode   -> b -> IO a)
      -> (handle ioMode r -> b -> m  a)
wrap2 f = \h y -> liftBase $ sanitizeIOError $ f (unsafeHandle h) y

wrap3 :: (FileHandle handle, MonadBase IO m)
      => (Handle ioMode   -> γ -> b -> IO a)
      -> (handle ioMode r -> γ -> b -> m  a)
wrap3 f = \h z y -> liftBase $ sanitizeIOError $ f (unsafeHandle h) z y

-- | Modify thrown @IOErrors@ in the given computation by erasing the
-- 'ioe_handle' field in the @IOError@ which may contain the @Handle@ which
-- caused the @IOError@.
--
-- I use this to ensure that @Handles@ don't /leak/ out the region via
-- exceptions.
sanitizeIOError :: IO a -> IO a
sanitizeIOError = modifyIOError $ \e -> e { ioe_handle = Nothing }
