{-

This file is part of the package devalot-pandoc. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/devalot-pandoc/LICENSE. No
part of the devalot-pandoc package, including this file, may be
copied, modified, propagated, or distributed except according to the
terms contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Execute a script and insert the output into Pandoc.
module Text.Pandoc.Devalot.Exec
       ( executeBlock
       ) where

--------------------------------------------------------------------------------
import Control.Exception hiding (handle)
import Data.Typeable
import System.Exit
import System.Process
import Text.Pandoc

--------------------------------------------------------------------------------
-- | Exception thrown if the command to execute fails.
data FailedCommandError = FailedCommandError String
  deriving (Typeable, Show)

instance Exception FailedCommandError

--------------------------------------------------------------------------------
executeBlock :: Block -> IO Block
executeBlock cb@(CodeBlock (x, y, alist) input) =
  case lookup "exec" alist of
    Just cmd -> return . CodeBlock (x, y, alist) =<< execute input cmd
    Nothing  -> return cb
executeBlock x = return x

--------------------------------------------------------------------------------
execute :: String -> String -> IO String
execute input cmd = do
  (exitcode, sout, _) <- readProcessWithExitCode "/bin/sh" args input'

  case (exitcode, sout) of
    (ExitSuccess, x) -> return x
    _ -> throwIO (FailedCommandError cmd)

  where
    -- Pandoc doesn't include final newline.
    input' :: String
    input' = if null input then input else input ++ "\n"

    args :: [String]
    args = ["-c", cmd]
