{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub as GH
import GitHub.Endpoints.Repos (RepoPublicity (..))

main :: IO ()
main = do
  possibleUser <- GH.executeRequest' $ GH.userReposR "nblumoe"  RepoPublicityPublic Nothing
  print possibleUser
