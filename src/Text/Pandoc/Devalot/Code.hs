{-

This file is part of the package devalot-pandoc. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/devalot-pandoc/LICENSE. No
part of the devalot-pandoc package, including this file, may be
copied, modified, propagated, or distributed except according to the
terms contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Extra Pandoc features for dealing with source code.
module Text.Pandoc.Devalot.Code
       ( includeFile
       ) where

--------------------------------------------------------------------------------
import Text.Pandoc

--------------------------------------------------------------------------------
-- | Copies the contents of a file into the body of a code block.  The
-- name of the file is given in the attributes of the code block like
-- so:
--
-- > ~~~~ {include="README"}
-- > this will be replaced by contents of README
-- > ~~~~
--
-- Taken from <http://johnmacfarlane.net/pandoc/scripting.html#include-files>.
includeFile :: Block -> IO Block
includeFile cb@(CodeBlock (x, y, alist) _) =
  case lookup "include" alist of
       Just f  -> return . (CodeBlock (x, y, alist)) =<< readFile f
       Nothing -> return cb
includeFile x = return x
