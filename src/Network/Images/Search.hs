module Network.Images.Search
( config
, linksOfQuery
, luckyLinkOfQuery
) where

import           Data.Random.Extras (safeChoice)
import           Data.Random.RVar (runRVar)
import           Data.Random.Source.DevRandom (DevRandom(..))
import           Data.Text (Text)
import qualified Network.Wreq as Wreq

import           BasePrelude hiding ((&))
import           Control.Lens
import           Data.Aeson.Lens

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
    links =
      Wreq.responseBody . key "items" . values . key "link" . _String

luckyLinkOfQuery :: Gapi -> Text -> IO (Maybe Text)
luckyLinkOfQuery gapi query = do
  links <- linksOfQuery gapi query
  case safeChoice links of
   Just link ->
     Just <$> runRVar link DevRandom
   Nothing ->
     return Nothing
