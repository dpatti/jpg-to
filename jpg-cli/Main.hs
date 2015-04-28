{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

import           BasePrelude
import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Configured, Name)
import           Data.Text (pack, unpack)
import qualified JpgTo

conf :: Configured a => Name -> IO a
conf name = do
  config <- Conf.load [Conf.Required "conf"]
  Conf.require config name

main :: IO ()
main = do
  gapi <- JpgTo.config <$> conf "key" <*> conf "cx"

  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: jpg-cli <query>"
      exitWith $ ExitFailure 1
    query:_ -> do
      url <- JpgTo.findBest gapi (pack query)
      putStrLn . unpack . fromJust $ url
