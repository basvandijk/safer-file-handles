{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  System.IO.SafeFiles
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides the data type 'File' which represents an actual file. A
-- file is a scarce resource, that is, it can only be used by one user at a
-- time. Because of the scarcity, a file needs to be /opened/ to grant temporary
-- sole access to the file. When the file is no longer needed it should be
-- /closed/ a.s.a.p to grant others access to the file.
--
-- The contribution of this module are as follows:
--
-- * First of all this module provides an instance for 'Resource' for a 'File'
-- which allows it to be used with the @regions@ package. The @region@ package
-- provides the region monad transformer. Scarce resources like files for
-- example can be opened in a region. When the region terminates, all opened
-- resources will be automatically closed. The main advantage of regions is that
-- the opened resources can not be returned from the region which ensures no I/O
-- with closed resources is possible.  The primary technique used in @regions@
-- is called \"Lightweight monadic regions\" which was invented by Oleg Kiselyov
-- and Chung-chieh Shan. See:
-- <http://okmij.org/ftp/Haskell/regions.html#light-weight>
--
-- * Secondly this module provides all the file operations of @System.IO@ lifted
-- to the region monad.
--
-- * The final contribution of this module is that files and file handles are
-- parameterised with the IOMode the file is in. This can be eiter 'R', 'W', 'A'
-- or 'RW'. All operations on files explicitly specify the needed IOMode using
-- the 'ReadModes' and 'WriteModes' type classes. This way it is impossible to
-- read from a write-only handle or write to a read-only handle for example.
--
-- /WARNING:/ Currenly the handling of the standard files ('stdin', 'stdout' and
-- 'stderr') is not to my liking. See the documentation for details.
--
-- /NOTE:/ This module also provides functions from @System.IO@ which don't
-- directly work with file handles like 'putStrLn' or 'getLine' for
-- example. These functions implicitly use the standard handles. I actually
-- provide more general versions of these that work in any 'MonadIO'. It could
-- be argued that these functions don't belong in this module because they don't
-- have anything to do with regions and explicit IOModes. However I provide them
-- as a convenience. But be warned that in the future these lifted functions
-- move to their own package!
--
-------------------------------------------------------------------------------

module System.IO.SafeFiles
    ( -- * Files with explicit IO modes as scarce resources
      File(..)

    , Binary
#if MIN_VERSION_base(4,2,0)
    , DefaultPermissions
#endif
    , Template

    , Standard(..)

      -- ** IO Modes
      -- | Types that represent the IOMode of a file.
    , R, W, A, RW

    , ReadModes, WriteModes

      -- ** Opening files in a region

      {-| Note that this module re-exports the @Control.Monad.Trans.Region@
      module from the @regions@ package which allows you to:

      * 'open' files in a region.

      * Run regions using 'runRegionT'.

      * Concurrently run regions inside another region using 'forkTopRegion'.

       * Duplicate regional file handles to a parent region using 'dup'.
      -}
    , module Control.Monad.Trans.Region

    , RegionalFileHandle

    , openFile, withFile

    , IOMode(..)

      -- ** Standard handles
      -- $stdHndls

    , stdin, stdout, stderr

      -- $TODO_cast

      -- *  Operations on regional file handles
      -- ** Determining and changing the size of a file
    , hFileSize

#ifdef __GLASGOW_HASKELL__
    , hSetFileSize
#endif

      -- ** Detecting the end of input
    , hIsEOF
    , isEOF

      -- ** Buffering operations
    , BufferMode(..)
    , hSetBuffering
    , hGetBuffering
    , hFlush

    -- ** Repositioning handles
    , hGetPosn
    , hSetPosn
    , HandlePosn

    , hSeek
    , SeekMode(..)
#if !defined(__NHC__)
    , hTell
#endif

    -- ** Handle properties
    , hIsOpen, hIsClosed
    , hIsReadable, hIsWritable
    , hIsSeekable

    -- ** Terminal operations (not portable: GHC/Hugs only)
#if !defined(__NHC__)
    , hIsTerminalDevice

    , hSetEcho
    , hGetEcho
#endif
    -- ** Showing handle state (not portable: GHC only)
#ifdef __GLASGOW_HASKELL__
    , hShow
#endif
    -- * Text input and output
    -- ** Text input
    -- | Note that the following text input operations are polymorphic in the
    -- IOMode of the given handle. However the IOModes are restricted to
    -- 'ReadModes' only which can be either 'R' or 'RW'.
    , hWaitForInput
    , hReady
    , hGetChar
    , hGetLine
    , hLookAhead
    , hGetContents

    -- ** Text ouput
    -- | Note that the following text output operations are polymorphic in the
    -- IOMode of the given handle. However the IOModes are restricted to
    -- 'WriteModes' only which can be either 'W', 'A' or 'RW'.
    , hPutChar
    , hPutStr
    , hPutStrLn
    , hPrint

    -- ** Special cases for standard input and output
    , interact
    , putChar
    , putStr
    , putStrLn
    , print
    , getChar
    , getLine
    , getContents
    , readIO
    , readLn

    -- * Binary input and output
    , withBinaryFile, openBinaryFile

    , hSetBinaryMode
    , hPutBuf
    , hGetBuf

#if !defined(__NHC__) && !defined(__HUGS__)
    , hPutBufNonBlocking
    , hGetBufNonBlocking
#endif

    -- * Temporary files
    , openTempFile
    , openBinaryTempFile

#if MIN_VERSION_base(4,2,0)
   , openTempFileWithDefaultPermissions
   , openBinaryTempFileWithDefaultPermissions
#endif

#if MIN_VERSION_base(4,2,0) && !defined(__NHC__) && !defined(__HUGS__)
    -- * Unicode encoding/decoding
    , hSetEncoding
    , hGetEncoding

    -- ** Unicode encodings
    , TextEncoding
    , latin1
    , utf8, utf8_bom
    , utf16, utf16le, utf16be
    , utf32, utf32le, utf32be
    , localeEncoding
    , mkTextEncoding

    -- * Newline conversion
    , hSetNewlineMode
    , Newline(..)
    , nativeNewline
    , NewlineMode(..)
    , noNewlineTranslation, universalNewlineMode, nativeNewlineMode
#endif
    )
    where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Prelude       ( Integer )
import Control.Monad ( fmap
                     , return, (>>=), fail
                     )
import Data.Function ( ($) )
import Data.Tuple    ( uncurry )
import Data.Bool     ( Bool(False, True) )
import Data.Char     ( Char, String )
import Data.Int      ( Int )
import Data.Maybe    ( Maybe(Nothing, Just) )
import Text.Show     ( Show )
import Text.Read     ( Read )
import Foreign.Ptr   ( Ptr )

import System.IO ( IO
                 , FilePath
                 , BufferMode(..)
                 , HandlePosn
                 , SeekMode(..)
                 , TextEncoding
                 , latin1
                 , utf8, utf8_bom
                 , utf16, utf16le, utf16be
                 , utf32, utf32le, utf32be
                 , localeEncoding
                 , Newline(..)
                 , nativeNewline
                 , NewlineMode(..)
                 , noNewlineTranslation, universalNewlineMode, nativeNewlineMode
                 )
import qualified System.IO as SIO ( isEOF
                                  , hSetPosn

                                  , interact
                                  , putChar
                                  , putStr
                                  , putStrLn
                                  , print
                                  , getChar
                                  , getLine
                                  , getContents
                                  , readIO
                                  , readLn

                                  , mkTextEncoding
                                  )

-- TODO: I only want the following import when running haddock, however the
-- following #ifdef does not seem to work:
-- #ifdef __HADDOCK__
import qualified System.IO as SIO
-- #endif

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO ( MonadCatchIO )

-- from transformers:
import Control.Monad.Trans ( MonadIO, liftIO )

-- from regions:
import Control.Monad.Trans.Region -- (re-exported entirely)
import Control.Monad.Trans.Region.Internal ( Resource
                                           , openResource
                                           , closeResource
                                           , internalHandle
                                           , ParentOf
                                           )
import qualified Control.Monad.Trans.Region.Internal as R ( Handle )

-- from explicit-iomodes
import System.IO.ExplicitIOModes ( IOMode(..)
                                 , hClose
                                 , R, W, A, RW
                                 , ReadModes
                                 , WriteModes
                                 -- TODO:, CheckMode
                                 )
import qualified System.IO.ExplicitIOModes as E ( Handle
                                                , openFile

                                                , stdin, stdout, stderr

                                                , hFileSize
#ifdef __GLASGOW_HASKELL__
                                                , hSetFileSize
#endif
                                                , hIsEOF

                                                , hSetBuffering
                                                , hGetBuffering
                                                , hFlush

                                                , hGetPosn
                                                , hSeek
#if !defined(__NHC__)
                                                , hTell
#endif
                                                , hIsOpen, hIsClosed
                                                , hIsReadable, hIsWritable
                                                , hIsSeekable
#if !defined(__NHC__)
                                                , hIsTerminalDevice

                                                , hSetEcho
                                                , hGetEcho
#endif
#ifdef __GLASGOW_HASKELL__
                                                , hShow
#endif
                                                , hWaitForInput
                                                , hReady
                                                , hGetChar
                                                , hGetLine
                                                , hLookAhead
                                                , hGetContents

                                                , hPutChar
                                                , hPutStr
                                                , hPutStrLn
                                                , hPrint

                                                , openBinaryFile
                                                , hSetBinaryMode
                                                , hPutBuf
                                                , hGetBuf
#if !defined(__NHC__) && !defined(__HUGS__)
                                                , hPutBufNonBlocking
                                                , hGetBufNonBlocking
#endif
                                                , openTempFile
                                                , openBinaryTempFile
#if MIN_VERSION_base(4,2,0)
                                                , openTempFileWithDefaultPermissions
                                                , openBinaryTempFileWithDefaultPermissions
#endif

#if MIN_VERSION_base(4,2,0) && !defined(__NHC__) && !defined(__HUGS__)
                                                , hSetEncoding
                                                , hGetEncoding

                                                , hSetNewlineMode
#endif
                                                )


-------------------------------------------------------------------------------
-- * Files with explicit IO modes as scarce resources
-------------------------------------------------------------------------------

{-| A file scarce resource.

You can open a file explicitly using 'open' or implicitly with one of the
following convenience functions:

* 'openFile'

* 'openBinaryFile'

* 'openTempFile'

* 'openBinaryTempFile'
#if MIN_VERSION_base(4,2,0)
* 'openTempFileWithDefaultPermissions'

* 'openBinaryTempFileWithDefaultPermissions'
#endif
-}
data File ioMode where
    File ∷ FilePath → IOMode ioMode → Binary → File ioMode

#if MIN_VERSION_base(4,2,0)
    TempFile ∷ FilePath → Template → DefaultPermissions → Binary → File RW
#else
    TempFile ∷ FilePath → Template → Binary → File RW
#endif
    -- TODO: I need to review this:
    Std ∷ Standard ioMode → File ioMode

-- | Should the file be opened in binary mode?
type Binary = Bool

-- | The template of a temporary file path.
type Template = String

#if MIN_VERSION_base(4,2,0)
-- | Should default permissions be used when opening a temporary file?
type DefaultPermissions = Bool
#endif

-- | The standard files parameterized by their corresponding IOModes.
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

    openResource (File filePath ioMode isBinary) =
        fmap (FileHandle Nothing) $
             (if isBinary then E.openBinaryFile else E.openFile)
             filePath ioMode

#if MIN_VERSION_base(4,2,0)
    openResource (TempFile filePath template defaultPerms isBinary) = do
        fmap (uncurry (FileHandle ∘ Just)) $
             (case (isBinary, defaultPerms) of
               (False, False) → E.openTempFile
               (True,  False) → E.openBinaryTempFile
               (False, True)  → E.openTempFileWithDefaultPermissions
               (True,  True)  → E.openBinaryTempFileWithDefaultPermissions
             ) filePath template
#else
    openResource (TempFile filePath template isBinary) = do
        fmap (uncurry (FileHandle ∘ Just)) $
             (if isBinary then E.openBinaryTempFile else E.openTempFile)
             filePath template
#endif
    -- TODO: I need to review this:
    openResource (Std std) = return $ FileHandle Nothing $ stdHndl std

    closeResource (FileHandle _ h) = hClose h


-- ** Opening files in a region

-- | Handy type synonym for a regional handle to a file.
type RegionalFileHandle ioMode = RegionalHandle (File ioMode)

-- | Convenience function for implicitly opening a file which should provide a
-- replacement for @System.IO.@'SIO.openFile'.
openFile ∷ MonadCatchIO pr
         ⇒ FilePath
         → IOMode ioMode
         → RegionT (File ioMode) s pr
                   (RegionalFileHandle ioMode (RegionT (File ioMode) s pr))
openFile filePath ioMode = open $ File filePath ioMode False

{-| A convenience function which opens a file, applies the given continuation
function to the resulting regional file handle and runs the resulting
region. This should provide a replacement for @System.IO.@'SIO.withFile'.

Note that: @withFile filePath ioMode =@ 'with' @$@ 'File' @filePath ioMode False@.
-}
withFile ∷ MonadCatchIO pr
         ⇒ FilePath
         → IOMode ioMode
         →  (∀ s. RegionalFileHandle ioMode (RegionT (File ioMode) s pr)
            → RegionT (File ioMode) s pr α
            )
         → pr α
withFile filePath ioMode = with $ File filePath ioMode False


-- ** Standard handles

{- $stdHndls

/BIG WARNING:/ I'm not satisfied with my current implementation of the standard
handles ('stdin', 'stdout' and 'stderr')! Currently the standard handles are
regional computations that return the regional file handles to the respected
standard handles. There are 4 problems with this approach:

* When the region terminates in which you call one of the standard handles the
respected handle will be closed. I think this is not the expected behaviour. I
would expect the standard handles to always remain open.

* In 'System.IO' the standard handles are pure values. My standard handles are
monadic computations which makes them harder to work with.

* There is no way to explicitly close a standard handle. Indeed, the whole
purpose of lightweight monadic regions is to automatically close
handles. However, when writing a Unix daemon for example, you need to be able to
explicitly close the standard handles.

* When reading 'man stdin' I'm confused if the standard handles are /always/
open on program startup:

quote 'man stdin':

\".../Under normal circumstances/ every Unix program has three streams opened for
it when it starts up, one for input, one for output, and one for printing
diagnostic or error messages...\"

\"...The stdin, stdout, and stderr macros conform to C89 and this standard also
stipulates that these three streams /shall be open/ at program startup....\"

So now I'm confused... are these standard file handles always open on program
startup or are there /abnormal/ situations when they are closed?

Maybe I just have to believe the documentation in @System.IO@ which specifies
that they are always initially open.

If the standard handles are closed on startup using a handle returned from one
of the standard handles will result in an exception! This would be a violation
of my safety guarantees which is unacceptable.

Does anyone have a solution?
-}

-- TODO: I need to review these:

stdin ∷ MonadCatchIO pr
      ⇒ RegionT (File R) s pr (RegionalFileHandle R (RegionT (File R) s pr))
stdin = open $ Std In

stdout ∷ MonadCatchIO pr
       ⇒ RegionT (File W) s pr (RegionalFileHandle W (RegionT (File W) s pr))
stdout = open $ Std Out

stderr ∷ MonadCatchIO pr
       ⇒ RegionT (File W) s pr (RegionalFileHandle W (RegionT (File W) s pr))
stderr = open $ Std Err

{- $TODO_cast

/TODO:/

The standard handles have concrete IOModes by default which work for the
majority of cases. In the rare occasion that you know these handles have
different IOModes you should be able to 'cast' them to the expected IOMode.

The @explicit-iomodes@ package defines this @cast@ function. I should also
define it here:

@
cast :: forall anyIOMode castedIOMode
     . (pr \`ParentOf\` cr, LiftIO cr, CheckMode castedIOMode)
     => RegionalFileHandle anyIOMode pr
     -> cr (Maybe (RegionalFileHandle castedIOMode pr))
@

However I'm not sure yet how to implement it...
-}

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


-------------------------------------------------------------------------------
-- * Operations on regional file handles
-------------------------------------------------------------------------------

-- **  Determining and changing the size of a file

-- | Wraps @System.IO.@'SIO.hFileSize'.
hFileSize ∷ (pr `ParentOf` cr, MonadIO cr)
          ⇒ RegionalFileHandle ioMode pr → cr Integer
hFileSize = wrap E.hFileSize

#ifdef __GLASGOW_HASKELL__
-- | Wraps @System.IO.@'SIO.hSetFileSize'.
hSetFileSize ∷ (pr `ParentOf` cr, MonadIO cr)
             ⇒ RegionalFileHandle ioMode pr → Integer → cr ()
hSetFileSize = wrap2 E.hSetFileSize
#endif


-- ** Detecting the end of input

-- | Wraps @System.IO.@'SIO.hIsEOF'.
hIsEOF ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
       ⇒ RegionalFileHandle ioMode pr → cr Bool
hIsEOF = wrap E.hIsEOF

-- | Wraps @System.IO.@'SIO.isEOF'.
isEOF ∷ MonadIO m ⇒ m Bool
isEOF = liftIO $ SIO.isEOF


-- ** Buffering operations

-- | Wraps @System.IO.@'SIO.hSetBuffering'.
hSetBuffering ∷ (pr `ParentOf` cr, MonadIO cr)
              ⇒ RegionalFileHandle ioMode pr → BufferMode → cr ()
hSetBuffering = wrap2 E.hSetBuffering

-- | Wraps @System.IO.@'SIO.hGetBuffering'.
hGetBuffering ∷ (pr `ParentOf` cr, MonadIO cr)
              ⇒ RegionalFileHandle ioMode pr → cr BufferMode
hGetBuffering = wrap E.hGetBuffering

-- | Wraps @System.IO.@'SIO.hFlush'.
hFlush ∷ (pr `ParentOf` cr, MonadIO cr)
       ⇒ RegionalFileHandle ioMode pr → cr ()
hFlush = wrap E.hFlush


-- ** Repositioning handles

-- | Wraps @System.IO.@'SIO.hGetPosn'.
hGetPosn ∷ (pr `ParentOf` cr, MonadIO cr)
         ⇒ RegionalFileHandle ioMode pr → cr HandlePosn
hGetPosn = wrap E.hGetPosn

-- | Wraps @System.IO.@'SIO.hSetPosn'.
hSetPosn ∷ MonadIO m ⇒ HandlePosn → m ()
hSetPosn = liftIO ∘ SIO.hSetPosn

-- | Wraps @System.IO.@'SIO.hSeek'.
hSeek ∷ (pr `ParentOf` cr, MonadIO cr)
      ⇒ RegionalFileHandle ioMode pr → SeekMode → Integer → cr ()
hSeek = wrap3 E.hSeek

#if !defined(__NHC__)
-- | Wraps @System.IO.@'SIO.hTell'.
hTell ∷ (pr `ParentOf` cr, MonadIO cr)
      ⇒ RegionalFileHandle ioMode pr → cr Integer
hTell = wrap E.hTell
#endif


-- ** Handle properties

-- | Wraps @System.IO.@'SIO.hIsOpen'.
hIsOpen ∷ (pr `ParentOf` cr, MonadIO cr)
         ⇒ RegionalFileHandle ioMode pr → cr Bool
hIsOpen = wrap E.hIsOpen

-- | Wraps @System.IO.@'SIO.hIsClosed'.
hIsClosed ∷ (pr `ParentOf` cr, MonadIO cr)
           ⇒ RegionalFileHandle ioMode pr → cr Bool
hIsClosed = wrap E.hIsClosed

-- | Wraps @System.IO.@'SIO.hIsReadable'.
hIsReadable ∷ (pr `ParentOf` cr, MonadIO cr)
            ⇒ RegionalFileHandle ioMode pr → cr Bool
hIsReadable = wrap E.hIsReadable

-- | Wraps @System.IO.@'SIO.hIsWritable'.
hIsWritable ∷ (pr `ParentOf` cr, MonadIO cr)
            ⇒ RegionalFileHandle ioMode pr → cr Bool
hIsWritable = wrap E.hIsWritable

-- | Wraps @System.IO.@'SIO.hIsSeekable'.
hIsSeekable ∷ (pr `ParentOf` cr, MonadIO cr)
            ⇒ RegionalFileHandle ioMode pr → cr Bool
hIsSeekable = wrap E.hIsSeekable


-- ** Terminal operations (not portable: GHC/Hugs only)

#if !defined(__NHC__)
-- | Wraps @System.IO.@'SIO.hIsTerminalDevice'.
hIsTerminalDevice ∷ (pr `ParentOf` cr, MonadIO cr)
                  ⇒ RegionalFileHandle ioMode pr → cr Bool
hIsTerminalDevice = wrap E.hIsTerminalDevice

-- | Wraps @System.IO.@'SIO.hSetEcho'.
hSetEcho ∷ (pr `ParentOf` cr, MonadIO cr)
         ⇒ RegionalFileHandle ioMode pr → Bool → cr ()
hSetEcho = wrap2 E.hSetEcho

-- | Wraps @System.IO.@'SIO.hGetEcho'.
hGetEcho ∷ (pr `ParentOf` cr, MonadIO cr)
         ⇒ RegionalFileHandle ioMode pr → cr Bool
hGetEcho = wrap E.hGetEcho
#endif


-- ** Showing handle state (not portable: GHC only)

#ifdef __GLASGOW_HASKELL__
-- | Wraps @System.IO.@'SIO.hShow'.
hShow ∷ (pr `ParentOf` cr, MonadIO cr)
      ⇒ RegionalFileHandle ioMode pr → cr String
hShow = wrap E.hShow
#endif


--------------------------------------------------------------------------------
-- * Text input and output
--------------------------------------------------------------------------------

-- ** Text input

-- | Wraps @System.IO.@'SIO.hWaitForInput'.
hWaitForInput ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
              ⇒ RegionalFileHandle ioMode pr → Int → cr Bool
hWaitForInput = wrap2 E.hWaitForInput

-- | Wraps @System.IO.@'SIO.hReady'.
hReady ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
       ⇒ RegionalFileHandle ioMode pr → cr Bool
hReady = wrap E.hReady

-- | Wraps @System.IO.@'SIO.hGetChar'.
hGetChar ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
         ⇒ RegionalFileHandle ioMode pr → cr Char
hGetChar = wrap E.hGetChar

-- | Wraps @System.IO.@'SIO.hGetLine'.
hGetLine ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
         ⇒ RegionalFileHandle ioMode pr → cr String
hGetLine = wrap E.hGetLine

-- | Wraps @System.IO.@'SIO.hLookAhead'.
hLookAhead ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
           ⇒ RegionalFileHandle ioMode pr → cr Char
hLookAhead = wrap E.hLookAhead

-- | Wraps @System.IO.@'SIO.hGetContents'.
hGetContents ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
             ⇒ RegionalFileHandle ioMode pr → cr String
hGetContents = wrap E.hGetContents


-- ** Text ouput

-- | Wraps @System.IO.@'SIO.hPutChar'.
hPutChar ∷ (pr `ParentOf` cr, MonadIO cr, WriteModes ioMode)
         ⇒ RegionalFileHandle ioMode pr → Char → cr ()
hPutChar = wrap2 E.hPutChar

-- | Wraps @System.IO.@'SIO.hPutStr'.
hPutStr ∷ (pr `ParentOf` cr, MonadIO cr, WriteModes ioMode)
        ⇒ RegionalFileHandle ioMode pr → String → cr ()
hPutStr = wrap2 E.hPutStr

-- | Wraps @System.IO.@'SIO.hPutStrLn'.
hPutStrLn ∷ (pr `ParentOf` cr, MonadIO cr, WriteModes ioMode)
          ⇒ RegionalFileHandle ioMode pr → String → cr ()
hPutStrLn = wrap2 E.hPutStrLn

-- | Wraps @System.IO.@'SIO.hPrint'.
hPrint ∷ (pr `ParentOf` cr, MonadIO cr, WriteModes ioMode, Show α)
       ⇒ RegionalFileHandle ioMode pr → α → cr ()
hPrint = wrap2 E.hPrint


-- ** Special cases for standard input and output

-- | Generalizes @System.IO.@'SIO.interact' to any 'MonadIO'.
interact ∷ MonadIO m ⇒ (String → String) → m ()
interact f = liftIO $ SIO.interact f

-- | Generalizes @System.IO.@'SIO.putChar' to any 'MonadIO'.
putChar ∷ MonadIO m ⇒ Char → m ()
putChar c = liftIO $ SIO.putChar c

-- | Generalizes @System.IO.@'SIO.putStr' to any 'MonadIO'.
putStr ∷ MonadIO m ⇒ String → m ()
putStr s = liftIO $ SIO.putStr s

-- | Generalizes @System.IO.@'SIO.putStrLn' to any 'MonadIO'.
putStrLn ∷ MonadIO m ⇒ String → m ()
putStrLn s = liftIO $ SIO.putStrLn s

-- | Generalizes @System.IO.@'SIO.print' to any 'MonadIO'.
print ∷ (MonadIO m, Show α) ⇒ α → m ()
print x = liftIO $ SIO.print x

-- | Generalizes @System.IO.@'SIO.getChar' to any 'MonadIO'.
getChar ∷ MonadIO m ⇒ m Char
getChar = liftIO SIO.getChar

-- | Generalizes @System.IO.@'SIO.getLine' to any 'MonadIO'.
getLine ∷ MonadIO m ⇒ m String
getLine = liftIO SIO.getLine

-- | Generalizes @System.IO.@'SIO.getContents' to any 'MonadIO'.
getContents ∷ MonadIO m ⇒ m String
getContents = liftIO SIO.getContents

-- | Generalizes @System.IO.@'SIO.readIO' to any 'MonadIO'.
readIO ∷ (MonadIO m, Read α) ⇒ String → m α
readIO s = liftIO $ SIO.readIO s

-- | Generalizes @System.IO.@'SIO.readLn' to any 'MonadIO'.
readLn ∷ (MonadIO m, Read α) ⇒ m α
readLn = liftIO SIO.readLn


--------------------------------------------------------------------------------
-- * Binary input and output
--------------------------------------------------------------------------------

{-| A convenience function which opens a file in binary mode, applies the given
continuation function to the resulting regional file handle and runs the
resulting region. This should provide a replacement for
@System.IO.@'SIO.withBinaryFile'.

Note that: @withBinaryFile filePath ioMode =@ 'with' @$@ 'File' @filePath ioMode True@.
-}
withBinaryFile ∷ MonadCatchIO pr
               ⇒ FilePath
               → IOMode ioMode
               →  (∀ s. RegionalFileHandle ioMode (RegionT (File ioMode) s pr)
                  → RegionT (File ioMode) s pr α
                  )
               → pr α
withBinaryFile filePath ioMode = with $ File filePath ioMode True

-- | Convenience function for implicitly opening a file in binary mode which
-- should provide a replacement for @System.IO.@'SIO.openBinaryFile'.
openBinaryFile ∷ MonadCatchIO pr
               ⇒ FilePath
               → IOMode ioMode
               → RegionT (File ioMode) s pr
                         (RegionalFileHandle ioMode (RegionT (File ioMode) s pr))
openBinaryFile filePath ioMode = open $ File filePath ioMode True

-- | Wraps @System.IO.@'SIO.hSetBinaryMode'.
hSetBinaryMode ∷ (pr `ParentOf` cr, MonadIO cr)
               ⇒ RegionalFileHandle ioMode pr → Bool → cr ()
hSetBinaryMode = wrap2 E.hSetBinaryMode

-- | Wraps @System.IO.@'SIO.hPutBuf'.
hPutBuf ∷ (pr `ParentOf` cr, MonadIO cr, WriteModes ioMode)
        ⇒ RegionalFileHandle ioMode pr → Ptr α → Int → cr ()
hPutBuf = wrap3 E.hPutBuf

-- | Wraps @System.IO.@'SIO.hGetBuf'.
hGetBuf ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
        ⇒ RegionalFileHandle ioMode pr → Ptr α → Int → cr Int
hGetBuf = wrap3 E.hGetBuf

#if !defined(__NHC__) && !defined(__HUGS__)
-- | Wraps @System.IO.@'SIO.hPutBufNonBlocking'.
hPutBufNonBlocking ∷ (pr `ParentOf` cr, MonadIO cr, WriteModes ioMode)
                   ⇒ RegionalFileHandle ioMode pr → Ptr α → Int → cr Int
hPutBufNonBlocking = wrap3 E.hPutBufNonBlocking

-- | Wraps @System.IO.@'SIO.hGetBufNonBlocking'.
hGetBufNonBlocking ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
                   ⇒ RegionalFileHandle ioMode pr → Ptr α → Int → cr Int
hGetBufNonBlocking = wrap3 E.hGetBufNonBlocking
#endif


--------------------------------------------------------------------------------
-- * Temporary files
--------------------------------------------------------------------------------

genOpenTempFile ∷ MonadCatchIO pr
                ⇒ Binary
                → DefaultPermissions
                → FilePath
                → Template
                → RegionT (File RW) s pr
                          ( FilePath
                          , RegionalFileHandle RW (RegionT (File RW) s pr)
                          )
genOpenTempFile binary defaultPerms filePath template = do
  rh@(internalHandle → FileHandle (Just fp) _) ← open $ TempFile filePath
                                                                 template
                                                                 binary
                                                                 defaultPerms
  return (fp, rh)

-- | Convenience function for implicitly opening a temporary file which should
-- provide a replacement for @System.IO.@'SIO.openTempFile'.
openTempFile ∷ MonadCatchIO pr
             ⇒ FilePath
             → Template
             → RegionT (File RW) s pr
                       ( FilePath
                       , RegionalFileHandle RW (RegionT (File RW) s pr)
                       )
openTempFile = genOpenTempFile False False

-- | Convenience function for implicitly opening a temporary file in binary mode
-- which should provide a replacement for @System.IO.@'SIO.openBinaryTempFile'.
openBinaryTempFile ∷ MonadCatchIO pr
                   ⇒ FilePath
                   → Template
                   → RegionT (File RW) s pr
                             ( FilePath
                             , RegionalFileHandle RW (RegionT (File RW) s pr)
                             )
openBinaryTempFile = genOpenTempFile True False

#if MIN_VERSION_base(4,2,0)
-- | Convenience function for implicitly opening a temporary file with default
-- permissions which should provide a replacement for
-- @System.IO.@'SIO.openTempFileWithDefaultPermissions'.
openTempFileWithDefaultPermissions ∷
    MonadCatchIO pr
  ⇒ FilePath
  → Template
  → RegionT (File RW) s pr
            ( FilePath
            , RegionalFileHandle RW (RegionT (File RW) s pr)
            )
openTempFileWithDefaultPermissions = genOpenTempFile False True

-- | Convenience function for implicitly opening a temporary file in binary mode
-- with default permissions which should provide a replacement for
-- @System.IO.@'SIO.openBinaryTempFileWithDefaultPermissions'.
openBinaryTempFileWithDefaultPermissions ∷
    MonadCatchIO pr
  ⇒ FilePath
  → Template
  → RegionT (File RW) s pr
            ( FilePath
            , RegionalFileHandle RW (RegionT (File RW) s pr)
            )
openBinaryTempFileWithDefaultPermissions = genOpenTempFile True True
#endif


#if MIN_VERSION_base(4,2,0) && !defined(__NHC__) && !defined(__HUGS__)
--------------------------------------------------------------------------------
-- * Unicode encoding/decoding
--------------------------------------------------------------------------------

-- | Wraps @System.IO.@'SIO.hSetEncoding'.
hSetEncoding ∷ (pr `ParentOf` cr, MonadIO cr)
             ⇒ RegionalFileHandle ioMode pr → TextEncoding → cr ()
hSetEncoding = wrap2 E.hSetEncoding

-- | Wraps @System.IO.@'SIO.hGetEncoding'.
hGetEncoding ∷ (pr `ParentOf` cr, MonadIO cr)
             ⇒ RegionalFileHandle ioMode pr → cr (Maybe TextEncoding)
hGetEncoding = wrap E.hGetEncoding

-- | Generalizes @System.IO.@'SIO.mkTextEncoding' to any 'MonadIO'.
mkTextEncoding ∷ MonadIO m ⇒ String → m TextEncoding
mkTextEncoding = liftIO ∘ SIO.mkTextEncoding


--------------------------------------------------------------------------------
-- * Newline conversion
--------------------------------------------------------------------------------

-- | Wraps @System.IO.@'SIO.hSetNewlineMode'.
hSetNewlineMode ∷ (pr `ParentOf` cr, MonadIO cr)
                ⇒ RegionalFileHandle ioMode pr → NewlineMode → cr ()
hSetNewlineMode = wrap2 E.hSetNewlineMode
#endif


-- The End ---------------------------------------------------------------------
