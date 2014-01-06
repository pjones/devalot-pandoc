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
import qualified FileListTest
import Test.Tasty

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ FileListTest.tests
  ]
