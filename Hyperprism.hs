module Hyperprism where

import qualified Github.Repos as GH
import qualified Github.Repos.Commits as GH
import Github.Auth
import Data.List

type ApiKey = String
type OrgName = String
type RepoName = String


getRepos :: ApiKey -> OrgName -> IO (Either GH.Error [GH.Repo])
getRepos = GH.organizationRepos' . Just . GithubOAuth

getRepoNames :: ApiKey -> OrgName -> IO (Either GH.Error [RepoName])
getRepoNames apiKey orgName =
  fmap (fmap (fmap GH.repoName)) $ getRepos apiKey orgName

getCommits :: ApiKey -> OrgName -> IO (Either GH.Error [IO (Either GH.Error [GH.Commit])])
getCommits apiKey orgName =
  fmap (fmap (fmap $ GH.commitsFor orgName)) $ getRepoNames apiKey orgName

dumpRepoNames :: ApiKey -> OrgName -> IO ()
dumpRepoNames apiKey orgName = do
  repos <- getRepos apiKey orgName
  case repos of
    (Left e) -> putStrLn $ "Error: " ++ (show e)
    (Right repos) -> putStrLn $ intercalate "\n" $ map GH.repoName repos


dumpCommits :: ApiKey -> OrgName -> IO ()
dumpCommits apiKey orgName = do
  repoCommits <- getCommits apiKey orgName
  case repoCommits of
    (Left e) -> putStrLn $ "Error: " ++ (show e)
    (Right repoCommits) -> sequence_ (map f repoCommits)
  where
    f repoCommits = do
      commits <- repoCommits
      case commits of
        (Left e) -> putStrLn $ "Error: " ++ (show e)
        (Right commits) -> putStrLn $ intercalate "\n" $ map g commits
    g commit = intercalate "\t" [GH.commitSha commit, GH.commitUrl commit]
