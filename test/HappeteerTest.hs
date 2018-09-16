module Main where

import           Network.URL
import           System.Exit

import           Test.Tasty
import           Test.Tasty.HUnit

import           Happeteer

main :: IO ()
main =
  defaultMain (testGroup "Happeteer Tests" [scrapURLTest])

scrapURLTest :: TestTree
scrapURLTest = testCase "Testing scrapURL"
    (assertEqual "Follow redirect google.com" expected_result (scrapURL raw_url))
    where
      Just raw_url = importURL "http://google.com"
      Just expected_url = importURL "https://www.google.com"
      expected_result :: IO(Scraped URL)
      expected_result =
        return Scraped{
          content = Right expected_url,
          abstract = AbstractScraped {
            exitCode = ExitSuccess,
            stdOut   = "https://www.google.com",
            stdErr   = ""
          }
        }
