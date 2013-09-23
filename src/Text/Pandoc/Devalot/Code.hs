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
import Control.Monad ((<=<))
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
-- The file can be narrowed to the text between delimiters.  The
-- delimiters are @{BEGIN: token}@ and @{END}@ and can be used like
-- this:
--
-- > ~~~~ {include="README" token="foo"}
-- > ~~~~
--
-- Taken from <http://johnmacfarlane.net/pandoc/scripting.html#include-files>.
includeFile :: Block -> IO Block
includeFile cb@(CodeBlock (x, y, alist) _) =
  case lookup "include" alist of
       Just f  -> return . (CodeBlock (x, y, alist)) =<< newtxt f
       Nothing -> return cb
  where newtxt f = readCodeFile f (lookup "token" alist)
includeFile x = return x

--------------------------------------------------------------------------------
-- | Read a file with code in it, possibly narrowing to a token.
readCodeFile :: FilePath -> Maybe String -> IO String
readCodeFile path Nothing      = readFile path
readCodeFile path (Just token) = do
  contents <- T.readFile path
  case narrowToToken token contents of
    Nothing  -> error $ "can't find token '" ++ token ++ "' in " ++ path
    Just txt -> return $! T.unpack txt

--------------------------------------------------------------------------------
-- | Narrow the given text to a beginning and ending delimiter.
narrowToToken :: String -> Text -> Maybe Text
narrowToToken token = matchEnd <=< matchStart
  where
    start :: Text
    start = "{BEGIN: " <> T.pack token <> "}"

    end :: Text
    end = "{END}"

    matchStart :: Text -> Maybe Text
    matchStart txt = case T.breakOn start txt of
      (_, match) | T.null match -> Nothing
                 | otherwise    -> Just $! stripStart match

    matchEnd :: Text -> Maybe Text
    matchEnd txt = case T.breakOn end txt of
      (prefix, match) | T.null match -> Nothing
                      | otherwise    -> Just $! stripEnd prefix

    stripStart :: Text -> Text
    stripStart = T.stripStart . T.drop (T.length start)

    stripEnd :: Text -> Text
    stripEnd = T.dropWhileEnd (/= '\n')
