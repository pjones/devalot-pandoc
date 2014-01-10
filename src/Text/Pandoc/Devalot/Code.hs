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
import Control.Applicative
import Control.Exception
import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Text.Pandoc

--------------------------------------------------------------------------------
data MissingTokenError = MissingTokenError String String
  deriving (Typeable)

instance Show MissingTokenError where
  show (MissingTokenError file token) =
    "MissingTokenError: " ++ "can't find token '" ++
    token ++ "' in " ++ file

instance Exception MissingTokenError

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
-- delimiters are @<<: token@ and @:>>@.  Older delimiters are also
-- supported: @{BEGIN: token}@ and @{END}@.
--
-- > ~~~~ {include="README" token="foo"}
-- > ~~~~
--
-- Taken from <http://johnmacfarlane.net/pandoc/scripting.html#include-files>.
includeFile :: Block -> IO Block
includeFile cb@(CodeBlock (x, y, alist) _) =
  case lookup "include" alist of
    Just f  -> return . CodeBlock (x, y, alist) =<< newtxt f
    Nothing -> return cb
  where newtxt f = readCodeFile f (lookup "token" alist)
includeFile x = return x

--------------------------------------------------------------------------------
-- | Read a file with code in it, possibly narrowing to a token.
readCodeFile :: FilePath -> Maybe String -> IO String
readCodeFile path Nothing      = readFile path
readCodeFile path (Just token) = do
  contents <- T.readFile path
  case newNarrow token contents <|> oldNarrow token contents of
    Nothing  -> throwIO (MissingTokenError path token)
    Just txt -> return $! T.unpack (removeIndent txt)

--------------------------------------------------------------------------------
-- | New style @<<:@/@:>>@ tokens.
newNarrow :: String -> Text -> Maybe Text
newNarrow token = narrowToToken (start, end) where
  start, end :: Text
  start = "<<: " <> T.pack token <> "\n"
  end   = ":>>\n"

--------------------------------------------------------------------------------
-- | Old style @{BEGIN}@/@{END}@ tokens.
oldNarrow :: String -> Text -> Maybe Text
oldNarrow token = narrowToToken (start, end) where
  start, end :: Text
  start = "{BEGIN: " <> T.pack token <> "}"
  end   = "{END}"

--------------------------------------------------------------------------------
-- | Narrow the given text to a beginning and ending delimiter.
narrowToToken :: (Text, Text) -> Text -> Maybe Text
narrowToToken (start, end) = matchEnd <=< matchStart
  where
    matchStart :: Text -> Maybe Text
    matchStart txt = case T.breakOn start txt of
      (_, match) | T.null match -> Nothing
                 | otherwise    -> Just $! stripStart match

    matchEnd :: Text -> Maybe Text
    matchEnd txt = case T.breakOn end txt of
      (prefix, match) | T.null match -> Nothing
                      | otherwise    -> Just $! stripEnd prefix

    stripStart :: Text -> Text
    stripStart = T.drop (T.length start)

    stripEnd :: Text -> Text
    stripEnd = T.dropWhileEnd (/= '\n')

--------------------------------------------------------------------------------
-- | Remove indentation found in the code snippet based on the first line.
removeIndent :: Text -> Text
removeIndent txt = T.unlines . strip (indent txt) . T.lines $ txt
  where
    indent :: Text -> Int
    indent = T.length . T.takeWhile isSpace

    strip :: Int -> [Text] -> [Text]
    strip n = map (T.drop n)
