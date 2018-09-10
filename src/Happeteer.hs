{-# LANGUAGE OverloadedStrings #-}

module Happeteer
    ( resolveURI
    ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text          as Text
import           GHC.IO.Handle
import           System.Exit
import           System.Process
import qualified Text.URI           as URI

data AbstractScraped = AbstractScraped {
  exitCode :: ExitCode,
  stdOut   :: String,
  stdErr   :: String
}

data Scraped content = Scraped {
  content  :: Maybe content,
  abstract :: AbstractScraped
}

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
resolveURI :: URI.URI -> IO (Scraped URI.URI)
resolveURI uri = do
  abstract_scraped <- scrap "js/resolve-uri.js" [URI.renderStr uri]
  case abstract_scraped of
    AbstractScraped{
      exitCode = ExitSuccess,
      stdOut   = std_out
    } -> do
      --
      --  TODO : URI.mkURI can throw exception here!!!
      --
      result_uri <- URI.mkURI $ Text.strip $ Text.pack std_out
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
  threadDelay $ fromIntegral t

-- Wait for process 1 min and if no exit - terminate it
safeWaitForProcess :: ProcessHandle -> TimeMS -> IO ExitCode
safeWaitForProcess ph elapsed_time
  | elapsed_time > processLimit = do
    _ <- terminateProcess ph
    mb_exit_code <- getProcessExitCode ph
    case mb_exit_code of
      Just _ ->
        return $ ExitFailure 504
      Nothing -> do
        _ <- sleepMS processInterval
        safeWaitForProcess ph elapsed_time
  | otherwise = do
    mb_exit_code <- getProcessExitCode ph
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
    process_spec = (proc "node" ([script] ++ params ++ ["--no-sandbox"])){
      std_out = CreatePipe,
      std_err = CreatePipe
    }
  in do
    (Nothing, Just std_out_h, Just std_err_h, ph) <- createProcess process_spec
    exit_code <- safeWaitForProcess ph (TimeMS 0)
    std_out <- hGetContents std_out_h
    std_err <- hGetContents std_err_h
    return AbstractScraped{
      exitCode = exit_code,
      stdOut   = std_out,
      stdErr   = std_err
    }
