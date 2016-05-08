{-# LANGUAGE OverloadedStrings #-}

import System.Environment (lookupEnv, getArgs)
import Data.String (fromString)

import qualified GitHub
import Data.Text as T
import Data.Text.IO as TIO
import Data.String  (fromString)
import Data.Monoid ((<>))
import Data.Vector as V

getAuth :: IO (Maybe (GitHub.Auth))
getAuth = do
  token <- lookupEnv "GITHUB_TOKEN"
  pure (GitHub.OAuth . fromString <$> token)

teamNameMatches :: Text -> GitHub.SimpleTeam -> Bool
teamNameMatches team_name team =
    (GitHub.untagName $ GitHub.simpleTeamSlug team) == team_name

findTeamId :: [Char] -> (V.Vector GitHub.SimpleTeam) -> Maybe Int
findTeamId team_name teams =
  case V.find (teamNameMatches (T.pack team_name)) teams of
    Nothing -> Nothing
    Just t ->  Just $ GitHub.untagId $ GitHub.simpleTeamId t

fetchPRs auth org_name repo_name =
  GitHub.executeRequestMaybe auth $ GitHub.pullRequestsForR (GitHub.mkOwnerName org_name) (GitHub.mkRepoName repo_name) Nothing Nothing

main :: IO ()
main = do
  [org_name, team_name] <- getArgs
  auth <- getAuth
  possibleTeams <- GitHub.executeRequestMaybe auth $ GitHub.teamsOfR (GitHub.mkOrganizationName $ T.pack org_name) Nothing
  let team_id = case possibleTeams of
                  Right teams  -> findTeamId team_name teams
                  Left error   -> Nothing

  repos <- case team_id of
    Just id -> GitHub.executeRequestMaybe auth $ GitHub.listTeamReposR (GitHub.mkTeamId id) Nothing
    Nothing -> error "No repos found"

  pull_requests <- case repos of
                        Left _ -> error "foooo"
                        Right rs  -> (V.mapM ((fetchPRs auth (T.pack org_name)) . GitHub.untagName .GitHub.repoName) rs)

  V.mapM_ (\r ->
             TIO.putStrLn $ either (("Error" <>) . tshow)
                                   (foldMap ((<> "\n") . formatPR))
                                   r) pull_requests

formatRepo = GitHub.untagName . GitHub.repoName
formatPR = GitHub.simplePullRequestTitle

tshow :: Show a => a -> Text
tshow = T.pack . show
