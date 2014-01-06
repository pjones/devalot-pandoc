{-

This file is part of the package devalot-pandoc. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/devalot-pandoc/LICENSE. No
part of the devalot-pandoc package, including this file, may be
copied, modified, propagated, or distributed except according to the
terms contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Devalot.FileList (FileList, files, parseFile) where

--------------------------------------------------------------------------------
import Control.Applicative hiding ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec hiding (parse)
import Text.Parsec.Text

--------------------------------------------------------------------------------
-- | Wrapper around a list of file names.
newtype FileList = FileList
  { files :: [Text] -- ^ List of file names.
  } deriving Show

--------------------------------------------------------------------------------
-- | Parse a file and return a 'FileList'.
parseFile :: FilePath -> IO (Either String FileList)
parseFile file = (convert . parse) <$> T.readFile file where
  -- Parse the contents of a file.
  parse :: Text -> Either ParseError FileList
  parse = runParser fileList () file

  -- Convert the parsec error to a string.
  convert :: Either ParseError FileList -> Either String FileList
  convert = either (Left . show) (Right . id)

--------------------------------------------------------------------------------
-- | Parses a list of file names.
fileList :: Parser FileList
fileList = FileList <$> manyTill (fileName <* spaces) eof

--------------------------------------------------------------------------------
-- | Parse a file name.
fileName :: Parser Text
fileName = do
  comment <|> spaces
  T.pack <$> manyTill anyChar (newline <|> space) <?> "file name"

--------------------------------------------------------------------------------
-- | Parse a comment.
comment :: Parser ()
comment = spaces >> char '#' >> (manyTill anyChar newline <?> "comment") >> spaces
