{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module JpgTo
(
  config
, findBest
) where

import           BasePrelude
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

findBest :: Gapi -> Text -> IO (Maybe Text)
findBest gapi query = do
  let opts = Wreq.defaults
           & imgApiQuery gapi
           & (Wreq.param "q" .~ [query])
  r <- Wreq.getWith opts imgApiRoot
  let items = toList . (r ^.) $ Wreq.responseBody . key "items" . _Array
  rand <- randomIO :: IO Int
  let len = length items
  if len > 0
    then do
      let pick = items !! (rand `mod` len)
      return . return . (pick ^.) $ key "link" . _String
    else
      return Nothing
