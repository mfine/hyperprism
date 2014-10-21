module Main (main) where

import Options.Applicative
import System.Environment (getEnv)

getApiKey :: IO String
getApiKey = getEnv "GITHUB_API_KEY"

parseRepo :: Parser String
parseRepo = strOption $
  short 'o' <> long "org" <> metavar "ORG" <> help "Org to crawl"

withOptions :: (String -> IO ()) -> IO ()
withOptions f = f =<< execParser
  (info (helper <*> parseRepo) $ progDesc "Crawl an org's github repos")

main :: IO ()
main = withOptions f
  where
    f :: String -> IO ()
    f repo = do
      k <- getApiKey
      putStrLn k
      putStrLn repo
