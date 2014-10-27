module Main (main) where

import Hyperprism
import Data.Monoid (mempty)

import Options.Applicative (execParser,info,short,long,metavar,help,strOption,(<>))
import System.Environment (getEnv)

getApiKey :: IO ApiKey
getApiKey = getEnv "GITHUB_API_KEY"

getOrgName :: IO OrgName
getOrgName = execParser $
  info parseOrgName mempty
  where
    parseOrgName = strOption $
      short 'o' <> long "org-name" <> metavar "ORG-NAME" <> help "Org name to crawl"


main :: IO ()
main = do
  apiKey <- getApiKey
  org <- getOrgName
  dumpCommits apiKey org

