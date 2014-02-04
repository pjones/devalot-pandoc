{-

This file is part of the package devalot-pandoc. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/devalot-pandoc/LICENSE. No
part of the devalot-pandoc package, including this file, may be
copied, modified, propagated, or distributed except according to the
terms contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Pandoc.Devalot
       ( devalotTransformStrDef
       , devalotTransformStr
       , devalotTransform
       , module Text.Pandoc.Devalot.Code
       , module Text.Pandoc.Devalot.Counter
       ) where

--------------------------------------------------------------------------------
import Text.Pandoc.Devalot.Code
import Text.Pandoc.Devalot.Counter
import Text.Pandoc.Devalot.Exec

--------------------------------------------------------------------------------
import Control.Monad (foldM)
import Text.Pandoc

--------------------------------------------------------------------------------
devalotTransform :: Pandoc -> IO Pandoc
devalotTransform p = do
  counterRef <- newCounterRef
  foldM (flip ($)) p [ bottomUpM includeFile
                     , bottomUpM executeBlock
                     , bottomUpM (counterFilter counterRef)
                     ]

--------------------------------------------------------------------------------
devalotTransformStrDef :: String -> IO String
devalotTransformStrDef = devalotTransformStr def def

--------------------------------------------------------------------------------
devalotTransformStr :: ReaderOptions -> WriterOptions -> String -> IO String
devalotTransformStr r w s = do pandoc' <- devalotTransform pandoc
                               return $ writeMarkdown w pandoc'
                         where pandoc = readMarkdown r s
