{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Main where

import           BasePrelude hiding ((&))
import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Configured, Name)
import           Data.Text (pack, unpack)
import qualified Network.Images.Search as Search

conf :: Configured a => Name -> IO a
conf name = do
  config <- Conf.load [Conf.Required "conf"]
  Conf.require config name

main :: IO ()
main = do
  gapi <- Search.config <$> conf "key" <*> conf "cx"

  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: jpg-cli <query>"
      exitWith $ ExitFailure 1
    query:_ -> do
      url <- Search.luckyLinkOfQuery gapi (pack query)
      putStrLn . unpack . fromJust $ url
