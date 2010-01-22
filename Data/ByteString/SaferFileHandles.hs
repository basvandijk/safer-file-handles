{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.SaferFileHandles
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module lifts the bytestring IO operations into the region monad.
--
-------------------------------------------------------------------------------

module Data.ByteString.SaferFileHandles
    ( hGetLine
    , hGetContents
    , hGet
    , hGetNonBlocking

    , hPut
    , hPutStr
    , hPutStrLn
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Int ( Int )

-- from transformers:
import Control.Monad.Trans ( MonadIO )

-- from bytestring:
import qualified Data.ByteString as B

-- from regions:
import Control.Monad.Trans.Region ( ParentOf )

-- from explicit-iomodes:
import qualified Data.ByteString.ExplicitIOModes as E ( hGetLine
                                                      , hGetContents
                                                      , hGet
                                                      , hGetNonBlocking

                                                      , hPut
                                                      , hPutStr
                                                      , hPutStrLn
                                                      )

-- from ourselves:
import System.IO.SaferFileHandles          ( RegionalFileHandle
                                           , ReadModes, WriteModes
                                           )
import System.IO.SaferFileHandles.Internal ( wrap, wrap2 )


-------------------------------------------------------------------------------
-- ByteString I/O with regional file handles
-------------------------------------------------------------------------------

-- | Wraps @Data.ByteString.@'B.hGetLine'.
hGetLine ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
         ⇒ RegionalFileHandle ioMode pr → cr B.ByteString
hGetLine = wrap E.hGetLine

-- | Wraps @Data.ByteString.@'B.hGetContents'.
hGetContents ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
             ⇒ RegionalFileHandle ioMode pr → cr B.ByteString
hGetContents = wrap E.hGetContents

-- | Wraps @Data.ByteString.@'B.hGet'.
hGet ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
     ⇒ RegionalFileHandle ioMode pr → Int → cr B.ByteString
hGet = wrap2 E.hGet

-- | Wraps @Data.ByteString.@'B.hGetNonBlocking'.
hGetNonBlocking ∷ (pr `ParentOf` cr, MonadIO cr, ReadModes ioMode)
                ⇒ RegionalFileHandle ioMode pr → Int → cr B.ByteString
hGetNonBlocking = wrap2 E.hGetNonBlocking

-- | Wraps @Data.ByteString.@'B.hPut'.
hPut ∷ (pr `ParentOf` cr, MonadIO cr, WriteModes ioMode)
     ⇒ RegionalFileHandle ioMode pr → B.ByteString → cr ()
hPut = wrap2 E.hPut

-- | Wraps @Data.ByteString.@'B.hPutStr'.
hPutStr ∷ (pr `ParentOf` cr, MonadIO cr, WriteModes ioMode)
        ⇒ RegionalFileHandle ioMode pr → B.ByteString → cr ()
hPutStr = wrap2 E.hPutStr

-- | Wraps @Data.ByteString.@'B.hPutStrLn'.
hPutStrLn ∷ (pr `ParentOf` cr, MonadIO cr, WriteModes ioMode)
          ⇒ RegionalFileHandle ioMode pr → B.ByteString → cr ()
hPutStrLn = wrap2 E.hPutStrLn


-- The End ---------------------------------------------------------------------
