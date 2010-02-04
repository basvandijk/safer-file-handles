{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , CPP
           , GADTs
           , TypeFamilies
           , RankNTypes
           , ViewPatterns
           , MultiParamTypeClasses
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Internal
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module System.IO.SaferFileHandles.Internal where

-- from base:
import Control.Monad ( fmap , return )
import Data.Function ( ($) )
import Data.Tuple    ( uncurry )
import Data.Bool     ( Bool(False, True) )
import Data.Char     ( String )
import Data.Maybe    ( Maybe(Nothing, Just) )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.Trans ( MonadIO, liftIO )

-- from regions:
import           Control.Resource        ( Resource, openResource, closeResource )
import qualified Control.Resource as R   ( Handle )
import Control.Monad.Trans.Region        ( RegionalHandle )
import Control.Monad.Trans.Region.Unsafe ( internalHandle )

-- from explicit-iomodes
import System.IO.ExplicitIOModes ( IOMode(..)
                                 , R, W, RW
                                 , IO
                                 , FilePath
                                 )

import qualified System.IO.ExplicitIOModes as E
                                 ( Handle
                                 , stdin, stdout, stderr
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

{-| A file scarce resource parameterized by the IOMode in which you want to open
the file.

Note that this module provides an instance for 'Resource' for 'File'
@ioMode@. This allows you to 'open' files in a region which are automatically
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
             → File RW

          -- TODO: I need to review the handling of standard files:
    Std ∷ Standard ioMode → File ioMode

-- | Should the file be opened in binary mode?
type Binary = Bool

-- | The template of a temporary file path.
type Template = String

#if MIN_VERSION_base(4,2,0)
-- | Should default permissions be used when opening a temporary file?
type DefaultPermissions = Bool
#endif

-- | The standard files parameterized by concrete IOModes which work for the
-- majority of cases.
data Standard ioMode where
    In  ∷ Standard R
    Out ∷ Standard W
    Err ∷ Standard W

-- | Internally used function to convert a standard file to the corresponding
-- handle.
stdHndl ∷ Standard ioMode → E.Handle ioMode
stdHndl In  = E.stdin
stdHndl Out = E.stdout
stdHndl Err = E.stderr

instance Resource (File ioMode) where
    data R.Handle (File ioMode) = FileHandle (Maybe FilePath)
                                    -- The optional file path is needed
                                    -- because opening a temporary file
                                    -- also yields the generated file
                                    -- path.
                                    (E.Handle ioMode)

    openResource (File isBinary filePath ioMode) =
        fmap (FileHandle Nothing) $
             (if isBinary then E.openBinaryFile else E.openFile)
             filePath ioMode

#if MIN_VERSION_base(4,2,0)
    openResource (TempFile isBinary filePath template defaultPerms) = do
        fmap (uncurry (FileHandle ∘ Just)) $
             (case (isBinary, defaultPerms) of
               (False, False) → E.openTempFile
               (True,  False) → E.openBinaryTempFile
               (False, True)  → E.openTempFileWithDefaultPermissions
               (True,  True)  → E.openBinaryTempFileWithDefaultPermissions
             ) filePath template
#else
    openResource (TempFile isBinary filePath template) = do
        fmap (uncurry (FileHandle ∘ Just)) $
             (if isBinary then E.openBinaryTempFile else E.openTempFile)
             filePath template
#endif
    -- TODO: I need to review the handling of standard files:
    openResource (Std std) = return $ FileHandle Nothing $ stdHndl std

    closeResource (FileHandle _ h) = E.hClose h

-- | A handy type synonym for a regional handle to an opened file parameterized
-- by the IOMode in which you opened the file and the region in which it was
-- created.
type RegionalFileHandle ioMode r = RegionalHandle (File ioMode) r


--------------------------------------------------------------------------------
-- Utility wrapping functions
--------------------------------------------------------------------------------

regularHandle ∷ RegionalFileHandle ioMode r → E.Handle ioMode
regularHandle (internalHandle → FileHandle _ h) = h

wrap ∷ MonadIO m
     ⇒ (E.Handle ioMode → IO α)
     → (RegionalFileHandle ioMode r → m α)
wrap f = \h → liftIO $ f (regularHandle h)

wrap2 ∷ MonadIO m
      ⇒ (E.Handle ioMode → β → IO α)
      → (RegionalFileHandle ioMode r → β → m α)
wrap2 f = \h y → liftIO $ f (regularHandle h) y

wrap3 ∷ MonadIO m
      ⇒ (E.Handle ioMode → γ → β → IO α)
      → (RegionalFileHandle ioMode r → γ → β → m α)
wrap3 f = \h z y → liftIO $ f (regularHandle h) z y


-- The End ---------------------------------------------------------------------

