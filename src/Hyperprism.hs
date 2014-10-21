module Hyperprism where

import qualified Github.Repos as Github
import Github.Auth
import Data.List
import Data.Maybe

type ApiKey = String
type Org = String

dumpRepos :: String -> String -> IO ()
dumpRepos apiKey org = do
  possibleRepos <- Github.organizationRepos' (Just (GithubOAuth apiKey)) org
  case possibleRepos of
    (Left e)  -> putStrLn $ "Error: " ++ (show e)
    (Right repos) -> putStrLn $ intercalate "\n\n" $ map formatRepo repos

formatRepo :: Github.Repo -> String
formatRepo repo =
  (Github.repoName repo) ++ "\t" ++
    (fromMaybe "" $ Github.repoDescription repo) ++ "\n" ++
    (Github.repoHtmlUrl repo) ++ "\n" ++
    (fromMaybe "" $ Github.repoCloneUrl repo) ++ "\t" ++
    (formatDate $ Github.repoUpdatedAt repo) ++ "\n" ++
    formatLanguage (Github.repoLanguage repo) ++
    "watchers: " ++ (show $ Github.repoWatchers repo) ++ "\t" ++
    "forks: " ++ (show $ Github.repoForks repo)

formatDate :: Maybe Github.GithubDate -> String
formatDate (Just date) = show . Github.fromGithubDate $ date
formatDate Nothing = "????"

formatLanguage :: Maybe String -> String
formatLanguage (Just language) = "language: " ++ language ++ "\t"
formatLanguage Nothing = ""
