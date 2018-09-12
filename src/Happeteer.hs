{-# LANGUAGE OverloadedStrings #-}

module Happeteer
    ( resolveURI
    ) where

import qualified Control.Concurrent (threadDelay)
import qualified Data.Text          (pack, strip)
import qualified GHC.IO.Handle      (hGetContents)
import qualified System.Exit        (ExitCode,
                                     ExitCode (ExitFailure, ExitSuccess))
import qualified System.Process     (CreateProcess (..), ProcessHandle,
                                     StdStream (CreatePipe), createProcess,
                                     getProcessExitCode, proc, terminateProcess)
import qualified Text.URI           (URI, mkURI, renderStr)

-- import qualified Control.Monad.Catch (MonadThrow (..))
-- import qualified Data.Text           (Text)
-- {-# SPECIALIZE Text.URI.mkURI :: Control.Monad.Catch.MonadThrow m0 => Data.Text.Text -> m0 Text.URI.URI #-}

data AbstractScraped = AbstractScraped {
  exitCode :: System.Exit.ExitCode,
  stdOut   :: String,
  stdErr   :: String
} deriving (Show)

data Scraped content = Scraped {
  content  :: Maybe content,
  abstract :: AbstractScraped
} deriving (Show)

data TimeMS = TimeMS Integer deriving (Eq, Ord, Show)
instance Num TimeMS where
  TimeMS t1 + TimeMS t2 = TimeMS $ t1 + t2
  TimeMS t1 - TimeMS t2 = TimeMS $ t1 - t2
  TimeMS t1 * TimeMS t2 = TimeMS $ t1 * t2
  abs (TimeMS t) = TimeMS $ abs t
  signum (TimeMS t)
    | t == 0 = TimeMS t
    | t < 0 = TimeMS (-1)
    | otherwise = TimeMS 1
  fromInteger = TimeMS

-- resolve URL (follow redirect etc)
resolveURI :: Text.URI.URI -> IO (Scraped Text.URI.URI)
resolveURI uri = do
  abstract_scraped <- scrap "js/resolve-uri.js" [Text.URI.renderStr uri]
  case abstract_scraped of
    AbstractScraped{
      exitCode = System.Exit.ExitSuccess,
      stdOut   = std_out
    } -> do
      --
      --  TODO : Text.URI.mkURI can throw exception here!!!
      --
      result_uri <- Text.URI.mkURI $ Data.Text.strip $ Data.Text.pack std_out
      return Scraped{
        content  = Just result_uri,
        abstract = abstract_scraped
      }
    AbstractScraped{} ->
      return Scraped{
        content  = Nothing,
        abstract = abstract_scraped
      }

-- TimeMS type utils
sleepMS :: TimeMS -> IO ()
sleepMS (TimeMS t) =
  Control.Concurrent.threadDelay $ fromIntegral t

-- Wait for process 1 min and if no exit - terminate it
safeWaitForProcess :: System.Process.ProcessHandle -> TimeMS -> IO System.Exit.ExitCode
safeWaitForProcess ph elapsed_time
  | elapsed_time > processLimit = do
    _ <- System.Process.terminateProcess ph
    mb_exit_code <- System.Process.getProcessExitCode ph
    case mb_exit_code of
      Just _ ->
        return $ System.Exit.ExitFailure 504
      Nothing -> do
        _ <- sleepMS processInterval
        safeWaitForProcess ph elapsed_time
  | otherwise = do
    mb_exit_code <- System.Process.getProcessExitCode ph
    case mb_exit_code of
      Just exit_code ->
        return exit_code
      Nothing        -> do
        _ <- sleepMS processInterval
        safeWaitForProcess ph (elapsed_time + processInterval)
  where
    processLimit = TimeMS 60000000
    processInterval = TimeMS 500000

-- abstract scraping of something
scrap :: String -> [String] -> IO AbstractScraped
scrap script params =
  let
    process_spec = (System.Process.proc "node" ([script] ++ params ++ ["--no-sandbox"])){
      System.Process.std_out = System.Process.CreatePipe,
      System.Process.std_err = System.Process.CreatePipe
    }

  in do
    (Nothing, Just std_out_h, Just std_err_h, ph) <- System.Process.createProcess process_spec
    exit_code <- safeWaitForProcess ph (TimeMS 0)
    std_out <- GHC.IO.Handle.hGetContents std_out_h
    std_err <- GHC.IO.Handle.hGetContents std_err_h
    return AbstractScraped{
      exitCode = exit_code,
      stdOut   = std_out,
      stdErr   = std_err
    }
