module Main (main) where

import Hyperprism
import Data.Monoid (mempty)

import Options.Applicative (execParser,info,short,long,metavar,help,strOption,(<>))
import System.Environment (getEnv)

getApiKey :: IO ApiKey
getApiKey = getEnv "GITHUB_API_KEY"

getOrg :: IO Org
getOrg = execParser $
  info parseOrg mempty
  where
    parseOrg = strOption $
      short 'o' <> long "org" <> metavar "ORG" <> help "Org to crawl"

main :: IO ()
main = do
  apiKey <- getApiKey
  org <- getOrg
  dumpRepos apiKey org
