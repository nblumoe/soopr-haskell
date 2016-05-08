{-# LANGUAGE OverloadedStrings #-}

import System.Environment (lookupEnv, getArgs)
import Data.String (fromString)

import qualified GitHub
-- import           GitHub.Endpoints.Repos (RepoPublicity (..))
import Data.Text as T
import Data.Text.IO as TIO
import Data.String  (fromString)
import Data.Monoid ((<>))
import Data.Vector as V

getAuth :: IO (Maybe (GitHub.Auth))
getAuth = do
  token <- lookupEnv "GITHUB_TOKEN"
  pure (GitHub.OAuth . fromString <$> token)

-- fetch teams of organisation
-- fetch team repos and filter by open_issues_count
-- fetch open pull requests for all relevant repos

teamNameMatches :: Text -> GitHub.SimpleTeam -> Bool
teamNameMatches team_name team =
    (GitHub.untagName $ GitHub.simpleTeamSlug team) == team_name

findTeamId :: [Char] -> (V.Vector GitHub.SimpleTeam) -> Maybe Int
findTeamId team_name teams =
  case V.find (teamNameMatches (T.pack team_name)) teams of
    Nothing -> Nothing
    Just t -> Just $ GitHub.untagId $ GitHub.simpleTeamId t

main :: IO ()
main = do
  [org_name, team_name] <- getArgs
  auth <- getAuth
  possibleTeams <- GitHub.executeRequestMaybe auth $ GitHub.teamsOfR (GitHub.mkOrganizationName $ T.pack org_name) Nothing
  let team_id = case possibleTeams of
              Left error   -> Nothing
              Right teams  -> findTeamId team_name teams

  repos <- case team_id of
        Nothing -> error "No repos found"
        Just id -> GitHub.executeRequestMaybe auth $ GitHub.listTeamReposR (GitHub.mkTeamId id) Nothing

  TIO.putStrLn $ either (("No repos found" <>) . tshow)
                        (foldMap ((<> "\n") .formatRepo))
                        repos

formatRepo = GitHub.untagName . GitHub.repoName

tshow :: Show a => a -> Text
tshow = T.pack . show
