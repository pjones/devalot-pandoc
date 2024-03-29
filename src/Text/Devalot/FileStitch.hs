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
-- | Copy the contents of each file to the given handle.  The @Text@
-- argument is a delimiter to use between the files.
hStitch :: FileList -> Text -> Handle -> IO ()
hStitch srcs delimiter = go (files srcs) where
  go :: [Text] -> Handle -> IO ()
  go [] _     = return ()
  go (x:xs) h = do content <- T.readFile (T.unpack x)
                   T.hPutStr h content
                   T.hPutStr h delimiter
                   go xs h

--------------------------------------------------------------------------------
-- | Copy the contents of each file in the 'FileList' to the
-- destination file.  See 'hStitch' for more information.
stitch :: FileList -> Text -> FilePath -> IO ()
stitch srcs delm dest = withFile dest WriteMode (hStitch srcs delm)
