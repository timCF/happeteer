{-# LANGUAGE OverloadedStrings #-}

module Happeteer
    ( resolveURL
    ) where

import qualified Control.Concurrent (threadDelay)
import qualified Data.Text          (pack, strip, unpack)
import qualified GHC.IO.Handle      (hGetContents)
import qualified Network.URL        (URL, exportURL, importURL)
import qualified System.Exit        (ExitCode,
                                     ExitCode (ExitFailure, ExitSuccess))
import qualified System.Process     (CreateProcess (..), ProcessHandle,
                                     StdStream (CreatePipe), createProcess,
                                     getProcessExitCode, proc, terminateProcess)

data AbstractScraped = AbstractScraped {
  exitCode :: System.Exit.ExitCode,
  stdOut   :: String,
  stdErr   :: String
} deriving (Show)

data Scraped content = Scraped {
  content  :: Maybe content,
  abstract :: AbstractScraped
} deriving (Show)

-- resolve URL (follow redirect etc)
resolveURL :: Network.URL.URL -> IO (Scraped Network.URL.URL)
resolveURL url = do
  abstract_scraped <- scrap "js/resolve-url.js" [Network.URL.exportURL url]
  case abstract_scraped of
    AbstractScraped{
      exitCode = System.Exit.ExitSuccess,
      stdOut   = std_out
    } ->
      return Scraped{
        content  = Network.URL.importURL $ Data.Text.unpack $ Data.Text.strip $ Data.Text.pack std_out,
        abstract = abstract_scraped
      }
    AbstractScraped{} ->
      return Scraped{
        content  = Nothing,
        abstract = abstract_scraped
      }

sleepMS :: Integer -> IO ()
sleepMS t =
  Control.Concurrent.threadDelay $ fromIntegral t

-- Wait for process 1 min and if no exit - terminate it
safeWaitForProcess :: System.Process.ProcessHandle -> Integer -> IO System.Exit.ExitCode
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
    processLimit :: Integer
    processLimit = 60000000
    processInterval :: Integer
    processInterval = 500000

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
    exit_code <- safeWaitForProcess ph 0
    std_out <- GHC.IO.Handle.hGetContents std_out_h
    std_err <- GHC.IO.Handle.hGetContents std_err_h
    return AbstractScraped{
      exitCode = exit_code,
      stdOut   = std_out,
      stdErr   = std_err
    }
