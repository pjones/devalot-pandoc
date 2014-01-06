{-

This file is part of the package devalot-pandoc. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/devalot-pandoc/LICENSE. No
part of the devalot-pandoc package, including this file, may be
copied, modified, propagated, or distributed except according to the
terms contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module FileListTest (tests) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.Hspec
import Text.Devalot.FileList

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "FileList"
  [ testCase "Parsing" parsedSpec
  ]

--------------------------------------------------------------------------------
parsedSpec :: Spec
parsedSpec = it "Test file" (go `shouldReturn` fileList)
  where
    go :: IO (Either String [Text])
    go = convert <$> parseFile "test/file-list.txt"

    convert :: Either String FileList -> Either String [Text]
    convert = fmap files

    fileList :: Either String [Text]
    fileList = Right [ "file1"
                     , "file2"
                     , "file3"
                     , "file4"
                     , "file5/a/b.md"
                     ]
