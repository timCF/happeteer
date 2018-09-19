module Main where

import           Network.URL            (URL, importURL)
import           System.Exit            (ExitCode (ExitSuccess))

import           Test.Tasty             (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit       (assertEqual, testCase)

import           Happeteer              (AbstractScraped (AbstractScraped, exitCode, stdErr, stdOut),
                                         Scraped (Scraped, abstract, content),
                                         scrapIMG, scrapURL)

import           Codec.Picture          (DynamicImage, readImageWithMetadata)
import           Codec.Picture.Metadata as Meta (Keys (DpiX, DpiY, Format, Height, Width),
                                                 Metadatas, lookup)

main :: IO ()
main = do
  scraped_url <- scrapURL raw_url
  scraped_img <- scrapIMG img_url
  Right (_, expected_img_metadatas) <- readImageWithMetadata "test/wiki-logo.jpg"
  defaultMain (testGroup "Happeteer Tests" (
    scrapURLTest scraped_url ++
    scrapIMGMetadatasTest scraped_img expected_img_metadatas))
  where
    Just raw_url = importURL "http://google.com"
    Just img_url = importURL "https://upload.wikimedia.org/wikipedia/commons/3/31/Wiki_logo_Nupedia.jpg"

scrapURLTest :: Scraped URL -> [TestTree]
scrapURLTest scraped_url =
  [testCase "Testing scrapURL" (assertEqual "Follow redirect google.com" expected_result scraped_url)]
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

scrapIMGMetadatasTest :: Scraped (DynamicImage, Metadatas) -> Metadatas -> [TestTree]
scrapIMGMetadatasTest Scraped{content = Right (_, scraped_metadatas)} expected_metadatas = [
    testCase "Testing scrapIMG Metadatas DpiX" (assertEqual "DpiX" (Meta.lookup DpiX scraped_metadatas) (Meta.lookup DpiX expected_metadatas)),
    testCase "Testing scrapIMG Metadatas DpiY" (assertEqual "DpiY" (Meta.lookup DpiY scraped_metadatas) (Meta.lookup DpiY expected_metadatas)),
    testCase "Testing scrapIMG Metadatas Format" (assertEqual "Format" (Meta.lookup Format scraped_metadatas) (Meta.lookup Format expected_metadatas)),
    testCase "Testing scrapIMG Metadatas Width" (assertEqual "Width" (Meta.lookup Width scraped_metadatas) (Meta.lookup Width expected_metadatas)),
    testCase "Testing scrapIMG Metadatas Height" (assertEqual "Height" (Meta.lookup Height scraped_metadatas) (Meta.lookup Height expected_metadatas))
  ]
