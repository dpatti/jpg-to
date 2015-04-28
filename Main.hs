{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

import           BasePrelude
import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Configured, Name)
import           Data.Text (Text, pack, unpack)
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

findBest :: Gapi -> Text -> IO Text
findBest gapi query = do
  let opts = Wreq.defaults
           & imgApiQuery gapi
           & (Wreq.param "q" .~ [query])
  r <- Wreq.getWith opts imgApiRoot
  let items = toList . (r ^.) $ Wreq.responseBody . key "items" . _Array
  rand <- randomIO :: IO Int
  let pick = items !! (rand `mod` length items)

  return . (pick ^.) $ key "link" . _String

conf :: Configured a => Name -> IO a
conf name = do
  config <- Conf.load [Conf.Required "conf"]
  Conf.require config name

main :: IO ()
main = do
  gapi <- Gapi <$> conf "key" <*> conf "cx"

  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: jpg-cli <query>"
      exitWith $ ExitFailure 1
    query:_ -> do
      url <- findBest gapi (pack query)
      putStrLn . unpack $ url
