{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , CPP
           , GADTs
           , TypeFamilies
           , MultiParamTypeClasses
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Internal
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module System.IO.SaferFileHandles.Internal where

-- from base:
import Data.Function       ( ($) )
import Data.Functor        ( (<$>) )
import Data.Tuple          ( uncurry )
import Data.Bool           ( Bool(False, True) )
import Data.Char           ( String )
import Data.Maybe          ( Maybe(Nothing, Just) )
import System.IO.Error     ( modifyIOError )
import GHC.IO.Exception    ( ioe_handle )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- from regions:
import           Control.Resource        ( Resource, Handle, open, close )
import Control.Monad.Trans.Region        ( RegionalHandle )
import Control.Monad.Trans.Region.Unsafe ( internalHandle )

#if __HADDOCK__
import qualified Control.Monad.Trans.Region as Region ( open )
#endif

-- from explicit-iomodes
import System.IO.ExplicitIOModes ( IOMode(..), ReadWriteMode, IO, FilePath )

import qualified System.IO.ExplicitIOModes as E
                                 ( Handle
                                 , openFile
                                 , openBinaryFile
                                 , openTempFile
                                 , openBinaryTempFile
#if MIN_VERSION_base(4,2,0)
                                 , openTempFileWithDefaultPermissions
                                 , openBinaryTempFileWithDefaultPermissions
#endif
                                 , hClose
                                 )


-------------------------------------------------------------------------------
-- * Files with explicit IO modes as scarce resources
-------------------------------------------------------------------------------

{-| A file scarce resource parameterized by the 'IOMode' in which you want to
open the file.

Note that this module provides an instance for 'Resource' for 'File'
@ioMode@. This allows you to 'Region.open' files in a region which are automatically
closed when the region terminates but it disallows you to return handles to
these closed files from the region so preventing I/O with closed files.
-}
data File ioMode where
    File ∷ Binary → FilePath → IOMode ioMode → File ioMode
    TempFile ∷ Binary
             → FilePath
             → Template
#if MIN_VERSION_base(4,2,0)
             → DefaultPermissions
#endif
             → File ReadWriteMode

-- | Should the file be opened in binary mode?
type Binary = Bool

-- | The template of a temporary file path.
type Template = String

#if MIN_VERSION_base(4,2,0)
-- | Should default permissions be used when opening a temporary file?
type DefaultPermissions = Bool
#endif

instance Resource (File ioMode) where
    data Handle (File ioMode) =
        FileHandle { mbFilePath ∷ Maybe FilePath
                     -- ^ Get the optional file path. This is needed because
                     -- opening a temporary file also yields the generated file
                     -- path.
                   , handle ∷ E.Handle ioMode
                   }

    open (File isBinary filePath ioMode) =
        FileHandle Nothing <$>
            (if isBinary then E.openBinaryFile else E.openFile)
            filePath ioMode

#if MIN_VERSION_base(4,2,0)
    open (TempFile isBinary filePath template defaultPerms) =
        uncurry (FileHandle ∘ Just) <$>
             (case (isBinary, defaultPerms) of
               (False, False) → E.openTempFile
               (True,  False) → E.openBinaryTempFile
               (False, True)  → E.openTempFileWithDefaultPermissions
               (True,  True)  → E.openBinaryTempFileWithDefaultPermissions
             ) filePath template
#else
    open (TempFile isBinary filePath template) =
        uncurry (FileHandle ∘ Just) <$>
            (if isBinary then E.openBinaryTempFile else E.openTempFile)
            filePath template
#endif
    close = sanitizeIOError ∘ E.hClose ∘ handle

-- | A handy type synonym for a regional handle to an opened file parameterized
-- by the 'IOMode' in which you opened the file and the region in which it was
-- created.
type RegionalFileHandle ioMode r = RegionalHandle (File ioMode) r


--------------------------------------------------------------------------------
-- Utility wrapping functions
--------------------------------------------------------------------------------

regularHandle ∷ RegionalFileHandle ioMode r → E.Handle ioMode
regularHandle = handle ∘ internalHandle

wrap ∷ MonadIO m
     ⇒ (E.Handle ioMode → IO α)
     → (RegionalFileHandle ioMode r → m α)
wrap f = \h → liftIO $ sanitizeIOError $ f (regularHandle h)

wrap2 ∷ MonadIO m
      ⇒ (E.Handle ioMode → β → IO α)
      → (RegionalFileHandle ioMode r → β → m α)
wrap2 f = \h y → liftIO $ sanitizeIOError $ f (regularHandle h) y

wrap3 ∷ MonadIO m
      ⇒ (E.Handle ioMode → γ → β → IO α)
      → (RegionalFileHandle ioMode r → γ → β → m α)
wrap3 f = \h z y → liftIO $ sanitizeIOError $ f (regularHandle h) z y

sanitizeIOError ∷ IO α → IO α
sanitizeIOError = modifyIOError $ \e → e { ioe_handle = Nothing }


-- The End ---------------------------------------------------------------------

