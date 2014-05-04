{-

This file is part of the package devalot-pandoc. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/devalot-pandoc/LICENSE. No
part of the devalot-pandoc package, including this file, may be
copied, modified, propagated, or distributed except according to the
terms contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Replace special comments in a source code file.
module Text.Devalot.SpecialComments (specialComments) where

--------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Control.Monad (void)
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (parse)
import Text.Parsec.Text

--------------------------------------------------------------------------------
-- | Comment styles.
data CommentStyle = SingleLine Text

--------------------------------------------------------------------------------
-- | Map programming languages to their comment style via their file
-- extension.
type CommentMarkers = M.Map Text CommentStyle

--------------------------------------------------------------------------------
-- | Return type for comment replacement functions.
data MatchType = NoMatch         -- ^ No match, leave comment alone.
               | Replace Text    -- ^ Replace comment with 'Text'.
               | Substitute Text -- ^ Keep comment markers, replace comment.

--------------------------------------------------------------------------------
-- | Make it easy to find the first match.
instance Monoid MatchType where
  mempty            = NoMatch
  mappend NoMatch x = x
  mappend x _       = x

--------------------------------------------------------------------------------
-- | Update source code comments which contain special instructions.
specialComments :: FilePath -> String -> Text -> Text
specialComments file lang txt =
  maybe txt parse (lookupParsers $ T.pack lang)

  where
    lookupParsers :: Text -> Maybe CommentStyle
    lookupParsers = flip M.lookup commentMarkers

    parser :: CommentStyle -> Parser Text
    parser style = searchAndReplace style replaceFunctions

    parse :: CommentStyle -> Text
    parse style = case runParser (parser style) () file txt of
      Left  _ -> txt -- Ignore parse error, leave unchanged.
      Right x -> x   -- Return modified source code.

--------------------------------------------------------------------------------
-- | Map of comment styles for each supported language.
commentMarkers :: CommentMarkers
commentMarkers = M.fromList [ ("ruby",    SingleLine "# ")
                            , ("haskell", SingleLine "-- ")
                            ]

--------------------------------------------------------------------------------
-- | Parse the start of a comment.  Returns the text leading up to the
-- comment and the comment starting marker, if found.
commentStart :: CommentStyle -> Parser (Text, Maybe Text)
commentStart style = case style of
  SingleLine txt ->
    do before <- manyTill anyChar (try (void $ string (T.unpack txt)) <|> eof)
       marker <- (eof >> return Nothing) <|> return (Just txt)
       return (T.pack before, marker)

--------------------------------------------------------------------------------
-- | Parse a comment up to the end marker.  Returns the comment text
-- and the end marker.
commentEnd :: CommentStyle -> Parser (Text, Text)
commentEnd style = case style of
  SingleLine _ -> do comment <- manyTill anyChar (void newline <|> eof)
                     return (T.pack comment, "\n")

--------------------------------------------------------------------------------
-- | A list of functions which perform replacements of special
-- characters between comment markers.
replaceFunctions :: [Text -> MatchType]
replaceFunctions = [ replaceEllipses
                   ]

--------------------------------------------------------------------------------
-- | Replace three dots with UTF-8 ellipses.  If the comment is solely
-- three dots then the comment markers are removed.
replaceEllipses :: Text -> MatchType
replaceEllipses txt = case T.breakOn asciiEllipses txt of
  (before, after) | T.null after  -> NoMatch
                  | T.null before -> Replace (replaced after)
                  | otherwise     -> Substitute (before <> replaced after)
  where
    asciiEllipses :: Text
    asciiEllipses = "..."

    utf8Ellipses :: Text
    utf8Ellipses = "…"

    replaced :: Text -> Text
    replaced = (utf8Ellipses <>) . T.drop (T.length asciiEllipses)

--------------------------------------------------------------------------------
-- | Try to update all found comments.
searchAndReplace :: CommentStyle -> [Text -> MatchType] -> Parser Text
searchAndReplace style fs = mconcat <$> manyTill (updateComment style fs) eof

--------------------------------------------------------------------------------
-- | A parser that performs search and replace with special text that
-- falls between comment markers.
updateComment :: CommentStyle -> [Text -> MatchType] -> Parser Text
updateComment style fs = do
  (before, start') <- commentStart style
  case start' of
    Nothing    -> return before
    Just start -> do
      (comment, end)  <- commentEnd style
      return $ case mconcat $ map ($comment) fs of
        NoMatch      -> before <> start <> comment <> end
        Replace x    -> before <> x                <> end
        Substitute x -> before <> start <> x       <> end
