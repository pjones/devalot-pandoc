{-

This file is part of the package devalot-pandoc. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/devalot-pandoc/LICENSE. No
part of the devalot-pandoc package, including this file, may be
copied, modified, propagated, or distributed except according to the
terms contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Functions for stitching files together.
module Text.Devalot.FileStitch (hStitch, stitch) where

--------------------------------------------------------------------------------
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Text.Devalot.FileList

--------------------------------------------------------------------------------
-- | Copy the contents of each file to the given handle.
hStitch :: FileList -> Handle -> IO ()
hStitch srcs = go (files srcs) where
  go :: [Text] -> Handle -> IO ()
  go [] _     = return ()
  go (x:xs) h = T.readFile (T.unpack x) >>= T.hPutStr h >> go xs h

--------------------------------------------------------------------------------
-- | Copy the contents of each file in the 'FileList' to the
-- destination file.
stitch :: FileList -> FilePath -> IO ()
stitch srcs dest = withFile dest WriteMode (hStitch srcs)
