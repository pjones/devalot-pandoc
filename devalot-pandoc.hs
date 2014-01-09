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
import Data.Monoid
import qualified Data.Text as T
import Options.Applicative
import System.IO
import Text.Devalot (FileList, files, parseFile, hStitch)
import Text.Pandoc.Devalot
import Text.Pandoc.JSON

--------------------------------------------------------------------------------
-- | Type for the command line parser.
data Command = Filter | Stitch FilePath String Bool
  deriving Show

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
stitchCmd = Stitch <$> argument str (metavar "FILE")
                   <*> (strOption (long "delimiter") <|> pure "\n")
                   <*> (switch (long "list"))

--------------------------------------------------------------------------------
loadFileList :: FilePath -> (FileList -> IO ()) -> IO ()
loadFileList file f = go =<< parseFile file where
  go (Left err)       = hPutStrLn stderr err
  go (Right fileList) = f fileList

--------------------------------------------------------------------------------
-- | Stitch files together and dump to STDOUT.
stitch :: FilePath -> String -> IO ()
stitch file delimiter = loadFileList file go where
  go fileList = hStitch fileList (T.pack delimiter) stdout

--------------------------------------------------------------------------------
dumpFiles :: FilePath -> IO ()
dumpFiles file = loadFileList file go where
  go fileList = mapM_ (putStrLn . T.unpack) (files fileList)

--------------------------------------------------------------------------------
-- | Dispatch the subcommand to the correct function.
dispatch :: Command -> IO ()
dispatch (Filter) = toJSONFilter devalotTransform
dispatch (Stitch file delimiter False) = stitch file delimiter
dispatch (Stitch file _         True)  = dumpFiles file

--------------------------------------------------------------------------------
main :: IO ()
main = dispatch =<< execParser opts
  where opts = info (helper <*> parser) idm
