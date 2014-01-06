{-

This file is part of the package devalot-pandoc. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/devalot-pandoc/LICENSE. No
part of the devalot-pandoc package, including this file, may be
copied, modified, propagated, or distributed except according to the
terms contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Generic counters for headers, figures, etc.
module Text.Pandoc.Devalot.Counter (newCounterRef, counterFilter) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.IORef
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc

--------------------------------------------------------------------------------
-- | Counter type.
type Counter = M.Map Text Int

--------------------------------------------------------------------------------
-- | Return a reference to a new 'Counter'.
newCounterRef :: IO (IORef Counter)
newCounterRef = newIORef M.empty

--------------------------------------------------------------------------------
-- | Filter @Inline@ types replacing counter references.
counterFilter :: IORef Counter -> Inline -> IO Inline
counterFilter ref (Str string) = Str <$> matchMap (replaceCounter ref) string
counterFilter _    x           = return x

--------------------------------------------------------------------------------
replaceCounter :: IORef Counter -> Text -> IO Text
replaceCounter ref name = do
  counter <- (+1) <$> currentValue name ref
  modifyIORef' ref (M.insert name counter)
  return $! T.pack (show counter)

--------------------------------------------------------------------------------
currentValue :: Text -> IORef Counter-> IO Int
currentValue name = fmap (M.findWithDefault 0 name) . readIORef

--------------------------------------------------------------------------------
matchMap :: (Text -> IO Text) -> String -> IO String
matchMap f str = T.unpack <$> go T.empty (match $ T.pack str)
  where
    go :: Text -> Either Text (Text, Text, Text) -> IO Text
    go x (Left y) = return (x <> y)
    go txt (Right (before, name, after)) = do
      replacement <- f name
      go (txt <> before <> replacement) (match after)

    match :: Text -> Either Text (Text, Text, Text)
    match txt = do
      (before, after) <- matchStart txt
      (name, after')  <- matchEnd after
      return (before, name, after')

    matchStart :: Text -> Either Text (Text, Text)
    matchStart txt = case T.breakOn start txt of
      (pre, post) | T.null post  -> Left txt
                  | otherwise    -> Right (pre, T.drop (T.length start) post)

    matchEnd :: Text -> Either Text (Text, Text)
    matchEnd txt = case T.breakOn end txt of
      (pre, post) | T.null pre -> Left txt
                  | otherwise  -> Right (pre, T.drop (T.length end) post)

    start, end :: Text
    start = "{{"
    end   = "}}"
