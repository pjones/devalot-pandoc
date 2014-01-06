{-

This file is part of the package devalot-pandoc. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/devalot-pandoc/LICENSE. No
part of the devalot-pandoc package, including this file, may be
copied, modified, propagated, or distributed except according to the
terms contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad
import Data.Monoid
import Options.Applicative
import System.IO
import Text.Devalot (parseFile, hStitch)
import Text.Pandoc.Devalot
import Text.Pandoc.JSON

--------------------------------------------------------------------------------
-- | Type for the command line parser.
data Command = Filter | Stitch FilePath deriving Show

--------------------------------------------------------------------------------
-- | Command line parser.
parser :: Parser Command
parser = subparser $ mconcat
    [ command "filter" (info (pure Filter) (progDesc filterDesc))
    , command "stitch" (info stitchCmd     (progDesc stitchDesc))
    ]
  where
    filterDesc = "Filter Pandoc JSON content from STDIN to STDOUT"
    stitchDesc = "Stitch together files listed in FILE"

--------------------------------------------------------------------------------
-- | The "stitch" Command.
stitchCmd :: Parser Command
stitchCmd = Stitch <$> stitchOpts where
  stitchOpts = argument str (metavar "FILE")

--------------------------------------------------------------------------------
-- | Stitch files together and dump to STDOUT.
stitch :: FilePath -> IO ()
stitch = go <=< parseFile where
  go (Left err)    = hPutStrLn stderr err
  go (Right files) = hStitch files stdout

--------------------------------------------------------------------------------
-- | Dispatch the subcommand to the correct function.
dispatch :: Command -> IO ()
dispatch (Filter)      = toJSONFilter devalotTransform
dispatch (Stitch file) = stitch file

--------------------------------------------------------------------------------
main :: IO ()
main = dispatch =<< execParser opts
  where opts = info (helper <*> parser) idm
