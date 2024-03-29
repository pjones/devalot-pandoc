-----------------------------------------------------------------------------
-- |
-- Module      :  Main (hlint)
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file https://github.com/ekmett/succinct/blob/master/LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module runs HLint on the lens source tree.
-----------------------------------------------------------------------------
module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    hints <- hlint $ ["src", "devalot-pandoc.hs", "--cpp-define=HLINT"] ++ args
    unless (null hints) exitFailure
