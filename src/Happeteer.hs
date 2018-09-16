{-# LANGUAGE OverloadedStrings #-}

module Happeteer
    ( scrapURL,
      scrapIMG
    ) where

import qualified Codec.Picture          (DynamicImage, decodeImageWithMetadata)
import qualified Codec.Picture.Metadata (Metadatas)
import qualified Control.Concurrent     (threadDelay)
import qualified Data.ByteString.Base64 (decode)
import qualified Data.ByteString.Char8  (pack)
import qualified Data.Text              (pack, strip, unpack)
import qualified GHC.IO.Handle          (hGetContents)
import qualified Network.URL            (URL, exportURL, importURL)
import qualified System.Exit            (ExitCode,
                                         ExitCode (ExitFailure, ExitSuccess))
import qualified System.Process         (CreateProcess (..), ProcessHandle,
                                         StdStream (CreatePipe), createProcess,
                                         getProcessExitCode, proc,
                                         terminateProcess)

data AbstractScraped = AbstractScraped {
  exitCode :: System.Exit.ExitCode,
  stdOut   :: String,
  stdErr   :: String
} deriving (Show)

data Scraped content = Scraped {
  content  :: Either String content,
  abstract :: AbstractScraped
} deriving (Show)

-- resolve URL (follow redirect etc)
scrapURL :: Network.URL.URL -> IO (Scraped Network.URL.URL)
scrapURL url = do
  abstract_scraped <- scrap "js/scrap-url.js" [Network.URL.exportURL url]
  case abstract_scraped of
    AbstractScraped{
      exitCode = System.Exit.ExitSuccess,
      stdOut   = std_out
    } ->
      case Network.URL.importURL std_out of
        Just parsed_url ->
          return Scraped{
            content  = Right parsed_url,
            abstract = abstract_scraped
          }
        Nothing ->
          return Scraped{
            content  = Left $ "can not parse URL, bad stdout: " ++ std_out,
            abstract = abstract_scraped
          }
    AbstractScraped{
      exitCode = exit_code
    } ->
      return Scraped{
        content  = Left $ "bad nodejs exit code: " ++ show exit_code,
        abstract = abstract_scraped
      }

-- download picture
scrapIMG :: Network.URL.URL -> IO (Scraped (Codec.Picture.DynamicImage, Codec.Picture.Metadata.Metadatas))
scrapIMG url = do
  abstract_scraped <- scrap "js/scrap-image-base64.js" [Network.URL.exportURL url]
  case abstract_scraped of
    AbstractScraped{
      exitCode = System.Exit.ExitSuccess,
      stdOut   = std_out
    } ->
      case Data.ByteString.Base64.decode $ Data.ByteString.Char8.pack std_out of
        Right std_out_bytes ->
          return Scraped{
            content  = Codec.Picture.decodeImageWithMetadata std_out_bytes,
            abstract = abstract_scraped
          }
        Left error_message ->
          return Scraped{
            content  = Left error_message,
            abstract = abstract_scraped
          }
    AbstractScraped{
      exitCode = exit_code
    } ->
      return Scraped{
        content  = Left $ "bad nodejs exit code: " ++ show exit_code,
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
      stdOut   = Data.Text.unpack $ Data.Text.strip $ Data.Text.pack std_out,
      stdErr   = std_err
    }
