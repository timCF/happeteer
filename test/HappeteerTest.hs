module Main where

import           Network.URL      (URL, importURL)
import           System.Exit      (ExitCode (ExitSuccess))

import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

import           Happeteer        (AbstractScraped (AbstractScraped, exitCode, stdErr, stdOut),
                                   Scraped (Scraped, abstract, content),
                                   scrapURL)

main :: IO ()
main = do
  actual_scraped_url <- scrapURL raw_url
  defaultMain (testGroup "Happeteer Tests" [scrapURLTest actual_scraped_url])
  where
    Just raw_url = importURL "http://google.com"

scrapURLTest :: Scraped URL -> TestTree
scrapURLTest actual_scraped_url =
  testCase "Testing scrapURL" (assertEqual "Follow redirect google.com" expected_result actual_scraped_url)
  where
    expected_std_out =
      "https://www.google.com/?gws_rd=ssl"
    Just expected_url =
      importURL expected_std_out
    expected_result =
      Scraped{
        content = Right expected_url,
        abstract = AbstractScraped {
          exitCode = ExitSuccess,
          stdOut   = expected_std_out,
          stdErr   = ""
        }
      }
