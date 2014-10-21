module Main (main) where

import qualified Github.Repos as Github
import Github.Auth
import Data.List
import Data.Maybe

import Options.Applicative
import System.Environment (getEnv)

getApiKey :: IO String
getApiKey = getEnv "GITHUB_API_KEY"

parseOrg :: Parser String
parseOrg = strOption $
  short 'o' <> long "org" <> metavar "ORG" <> help "Org to crawl"

withOptions :: (String -> IO ()) -> IO ()
withOptions f = f =<< execParser
  (info (helper <*> parseOrg) $ progDesc "Crawl an org's github repos")

main :: IO ()
main = withOptions f
  where
    f :: String -> IO ()
    f org = do
      apiKey <- getApiKey
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

