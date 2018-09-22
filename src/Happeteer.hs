{-# LANGUAGE OverloadedStrings #-}

module Happeteer
    ( scrapData,
      scrapURL,
      scrapIMG,
      scrapJSON,
      AbstractScraped (..),
      Scraped (..),
      NodeScript (..),
      NodeArgs (..)
    ) where

import qualified Codec.Picture              (DynamicImage,
                                             decodeImageWithMetadata)
import qualified Codec.Picture.Metadata     (Metadatas)
import qualified Control.Concurrent         (threadDelay)
import qualified Data.Aeson                 (Value, eitherDecode)
import qualified Data.ByteString.Base64     (decode)
import qualified Data.ByteString.Char8      (pack)
import qualified Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text                  (pack, strip, unpack)
import qualified GHC.IO.Handle              (hGetContents)
import qualified Network.URL                (Host, URL, exportHost, exportURL,
                                             importURL)
import qualified System.Exit                (ExitCode,
                                             ExitCode (ExitFailure, ExitSuccess))
import qualified System.Process             (CreateProcess (..), ProcessHandle,
                                             StdStream (CreatePipe),
                                             createProcess, getProcessExitCode,
                                             proc, terminateProcess)

newtype NodeScript = NodeScript String

data NodeArgs = NodeArgs {
  targetURL :: Network.URL.URL,
  proxyHost :: Maybe Network.URL.Host
} deriving (Show, Eq, Ord)

data AbstractScraped = AbstractScraped {
  exitCode :: System.Exit.ExitCode,
  stdOut   :: String,
  stdErr   :: String
} deriving (Show, Eq, Ord)

data Scraped content = Scraped {
  content  :: Either String content,
  abstract :: AbstractScraped
} deriving (Show, Eq, Ord)

--------------------------
-- public API functions --
--------------------------

-- scrap some data t
scrapData :: NodeScript -> NodeArgs -> (String -> Either String t) -> IO (Scraped t)
scrapData node_script node_args parser = do
  abstract_scraped <- scrap node_script node_args
  case abstract_scraped of
    AbstractScraped{
      exitCode = System.Exit.ExitSuccess,
      stdOut   = std_out
    } ->
      return Scraped{
        content  = parser std_out,
        abstract = abstract_scraped
      }
    AbstractScraped{
      exitCode = exit_code
    } ->
      return Scraped{
        content  = Left $ "bad nodejs exit code: " ++ show exit_code,
        abstract = abstract_scraped
      }

-- resolve URL (follow redirect etc)
scrapURL :: NodeArgs -> IO (Scraped Network.URL.URL)
scrapURL node_args =
  scrapData (NodeScript "js/scrap-url.js") node_args parser
  where
    parser :: String -> Either String Network.URL.URL
    parser std_out =
      case Network.URL.importURL std_out of
        Just parsed_url -> Right parsed_url
        Nothing         -> Left "scrapURL failed - can't parse URL"

-- download picture
scrapIMG :: NodeArgs -> IO (Scraped (Codec.Picture.DynamicImage, Codec.Picture.Metadata.Metadatas))
scrapIMG node_args =
  scrapData (NodeScript "js/scrap-image-base64.js") node_args parser
  where
    parser :: String -> Either String (Codec.Picture.DynamicImage, Codec.Picture.Metadata.Metadatas)
    parser std_out =
      case eitherExplain "scrapIMG failed - can't decode Base64, error: " $ Data.ByteString.Base64.decode $ Data.ByteString.Char8.pack std_out of
        Right std_out_bytes -> eitherExplain "scrapIMG failed - can't decode ByteString, error: " $ Codec.Picture.decodeImageWithMetadata std_out_bytes
        Left error_message -> Left error_message

scrapJSON :: NodeScript -> NodeArgs -> IO (Scraped Data.Aeson.Value)
scrapJSON node_script node_args =
  scrapData node_script node_args parser
  where
    parser :: String -> Either String Data.Aeson.Value
    parser std_out =
      eitherExplain "scrapJSON failed - can't decode JSON, error: " $ Data.Aeson.eitherDecode $ Data.ByteString.Lazy.Char8.pack std_out


-----------------------
-- private functions --
-----------------------

eitherExplain :: String -> Either String t -> Either String t
eitherExplain explanation_prefix (Left error_message) = Left $ explanation_prefix ++ error_message
eitherExplain _ (Right success) = Right success

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

-- abstract scraping of stdout
scrap :: NodeScript -> NodeArgs -> IO AbstractScraped
scrap (NodeScript string_script) NodeArgs{targetURL = target_url, proxyHost = mb_proxy_host} =
  let
    proxy_host =
      case mb_proxy_host of
        Just host -> ["--proxy-server=" ++ Network.URL.exportHost host]
        Nothing   -> []
    process_spec = (System.Process.proc "node" ([string_script, Network.URL.exportURL target_url] ++ proxy_host ++ ["--no-sandbox"])){
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
