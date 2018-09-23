{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.URL            (URL, importURL)
import           System.Exit            (ExitCode (ExitSuccess))

import           Test.Tasty             (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit       (assertEqual, testCase)

import           Happeteer              (AbstractScraped (AbstractScraped, exitCode, stdErr, stdOut),
                                         ChromiumArgs (ChromiumArgs, proxyHost, sandbox, targetURL),
                                         JS2Eval (JS2Eval),
                                         Scraped (Scraped, abstract, content),
                                         scrapIMG, scrapJSON, scrapURL)

import           Codec.Picture          (DynamicImage, readImageWithMetadata)
import           Codec.Picture.Metadata as Meta (Keys (DpiX, DpiY, Format, Height, Width),
                                                 Metadatas, lookup)
import           Data.Aeson             (Value (Object))
import           Data.HashMap.Strict    (fromList)

newtype IMG = IMG DynamicImage deriving (Eq)

instance Show IMG where
  show _ = "<DynamicImage>"

main :: IO ()
main = do
  scraped_url <- scrapURL ChromiumArgs{targetURL = raw_url, proxyHost = Nothing, sandbox = True}
  scraped_img <- scrapIMG ChromiumArgs{targetURL = img_url, proxyHost = Nothing, sandbox = True}
  scraped_json <- scrapJSON scrap_json_js ChromiumArgs{targetURL = json_url, proxyHost = Nothing, sandbox = True}
  Right (expected_img_value, expected_img_metadatas) <- readImageWithMetadata "test/wiki-logo.jpg"
  defaultMain (testGroup "Happeteer Tests" (
    scrapURLTest scraped_url ++
    scrapIMGMetadatasTest scraped_img expected_img_metadatas ++
    scrapIMGValueTest scraped_img expected_img_value ++
    scrapJSONTest scraped_json))
  where
    scrap_json_js = JS2Eval "() => JSON.stringify({\"hello\":\"world\"})"
    Just raw_url = importURL "http://google.com"
    Just img_url = importURL "https://upload.wikimedia.org/wikipedia/commons/3/31/Wiki_logo_Nupedia.jpg"
    Just json_url = importURL "https://www.wikipedia.org/"

scrapURLTest :: Scraped URL -> [TestTree]
scrapURLTest scraped_url =
  [testCase "Testing scrapURL" (assertEqual "Follow redirect google.com" expected_url scraped_url)]
  where
    expected_std_out :: String
    expected_std_out =
      "https://www.google.com/?gws_rd=ssl"
    Just expected_url_content =
      importURL expected_std_out
    expected_url =
      Scraped{
        content = Right expected_url_content,
        abstract = AbstractScraped {
          exitCode = ExitSuccess,
          stdOut   = expected_std_out,
          stdErr   = ""
        }
      }

scrapIMGMetadatasTest :: Scraped (DynamicImage, Metadatas) -> Metadatas -> [TestTree]
scrapIMGMetadatasTest Scraped{content = Right (_, scraped_metadatas)} expected_metadatas = [
    testCase "Testing scrapIMG Metadatas DpiX" (assertEqual "DpiX" (Meta.lookup DpiX scraped_metadatas) (Meta.lookup DpiX expected_metadatas)),
    testCase "Testing scrapIMG Metadatas DpiY" (assertEqual "DpiY" (Meta.lookup DpiY scraped_metadatas) (Meta.lookup DpiY expected_metadatas)),
    testCase "Testing scrapIMG Metadatas Format" (assertEqual "Format" (Meta.lookup Format scraped_metadatas) (Meta.lookup Format expected_metadatas)),
    testCase "Testing scrapIMG Metadatas Width" (assertEqual "Width" (Meta.lookup Width scraped_metadatas) (Meta.lookup Width expected_metadatas)),
    testCase "Testing scrapIMG Metadatas Height" (assertEqual "Height" (Meta.lookup Height scraped_metadatas) (Meta.lookup Height expected_metadatas))
  ]

scrapIMGValueTest :: Scraped (DynamicImage, Metadatas) -> DynamicImage -> [TestTree]
scrapIMGValueTest Scraped{content = Right (scraped_image, _)} expected_image = [
    testCase "Testing scrapIMG Value" (assertEqual "DynamicImage" (IMG scraped_image) (IMG expected_image))
  ]

scrapJSONTest :: Scraped Value -> [TestTree]
scrapJSONTest scraped_json =
  [testCase "Testing scrapJSON" (assertEqual "hello world object" expected_json scraped_json)]
  where
    expected_std_out :: String
    expected_std_out =
      "{\"hello\":\"world\"}"
    expected_json =
      Scraped{
        content = Right $ Object $ fromList [("hello", "world")],
        abstract = AbstractScraped {
          exitCode = ExitSuccess,
          stdOut   = expected_std_out,
          stdErr   = ""
        }
      }
