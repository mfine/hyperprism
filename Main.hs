module Main (main) where

import qualified Github.Repos as Github
import Github.Auth
import Data.List
import Data.Maybe
import Data.Monoid (mempty)

import Options.Applicative (execParser,info,short,long,metavar,help,strOption,(<>))
import System.Environment (getEnv)

type ApiKey = String
type Org = String

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
  possibleRepos <- Github.organizationRepos' (Just (GithubOAuth apiKey)) org
  case possibleRepos of
    (Left error)  -> putStrLn $ "Error: " ++ (show error)
    (Right repos) -> putStrLn $ intercalate "\n\n" $ map formatRepo repos

formatRepo repo =
  (Github.repoName repo) ++ "\t" ++
    (fromMaybe "" $ Github.repoDescription repo) ++ "\n" ++
    (Github.repoHtmlUrl repo) ++ "\n" ++
    (fromMaybe "" $ Github.repoCloneUrl repo) ++ "\t" ++
    (formatDate $ Github.repoUpdatedAt repo) ++ "\n" ++
    formatLanguage (Github.repoLanguage repo) ++
    "watchers: " ++ (show $ Github.repoWatchers repo) ++ "\t" ++
    "forks: " ++ (show $ Github.repoForks repo)

formatDate (Just date) = show . Github.fromGithubDate $ date
formatDate Nothing = "????"

formatLanguage (Just language) = "language: " ++ language ++ "\t"
formatLanguage Nothing = ""

