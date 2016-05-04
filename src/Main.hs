module Main where

import qualified GitHub as GH
import GitHub.Endpoints.Repos (RepoPublicity (..))

import Data.Text as T
import Data.Proxy as Proxy

main :: IO ()
main = do
  let nameText = T.pack "nblumoe"
  let name =  GH.mkName Proxy nameText
  possibleUser <- GH.executeRequest' $ (GH.userReposR name RepoPublicityPublic Nothing)
  print possibleUser
