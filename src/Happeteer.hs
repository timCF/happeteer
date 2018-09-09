module Happeteer
    ( someFunc
    ) where

import           Control.Monad
import           System.Process

someFunc :: IO ()
someFunc =
  void . createProcess . shell $ "echo 'hello'"
