module JpgTo
( config
, linksOfQuery
, luckyLinkOfQuery
) where

import           BasePrelude hiding ((&))
import           Control.Lens
import           Data.Aeson.Lens
import           Data.Text (Text)
import qualified Network.Wreq as Wreq
import           System.Random

data Gapi = Gapi { apiKey :: Text, cx :: Text }

type PartialQuery = Wreq.Options -> Wreq.Options

imgApiRoot :: String
imgApiRoot = "https://www.googleapis.com/customsearch/v1"

-- https://developers.google.com/custom-search/json-api/v1/reference/cse/list
imgApiQuery :: Gapi -> PartialQuery
imgApiQuery s = (Wreq.param "key" .~ [apiKey s])
              . (Wreq.param "cx" .~ [cx s])
              . (Wreq.param "searchType" .~ ["image"])
              . (Wreq.param "safe" .~ ["high"])
              . (Wreq.param "imgSize" .~ ["large"])

config :: Text -> Text -> Gapi
config = Gapi

linksOfQuery :: Gapi -> Text -> IO [Text]
linksOfQuery gapi query = do
  let opts = Wreq.defaults
           & imgApiQuery gapi
           & (Wreq.param "q" .~ [query])
  r <- Wreq.getWith opts imgApiRoot
  return (r ^.. links)
  where
    links = Wreq.responseBody . key "items" . values . key "link" . _String

luckyLinkOfQuery :: Gapi -> Text -> IO (Maybe Text)
luckyLinkOfQuery gapi query = linksOfQuery gapi query >>= sample

sample :: [a] -> IO (Maybe a)
sample [] = return Nothing
sample xs = do
  rand <- randomIO :: IO Int
  return . Just $ xs !! (rand `mod` length xs)
