{-# LANGUAGE RecordWildCards #-}

module Giterate ( CmdResult
                , GtrCommand (..)
                , GtrInit (..)
                , GtrCreateProject (..)
                , GtrDeleteProject (..)
                ) where

import           Data.Time.Clock
import           Giterate.Internal as GI

{- ================= -}
{- Giterate commands -}
{- ================= -}

class GtrCommand cmd where
  execute :: cmd -> IO CmdResult


----------------
-- Initial repo
----------------

data GtrInit
  = GtrInit

instance GtrCommand GtrInit where
  execute GtrInit =
    gitInit
    %>> gitCheckoutB "meta"
    %>> gitTransaction (success (createDirectory "projects"))

------------------
-- Create project
------------------

data GtrCreateProject
  = GtrCreateProject { gtrCreateProjectName :: String
                     , gtrCreateProjectTime :: NominalDiffTime
                     }

instance GtrCommand GtrCreateProject where
  execute GtrCreateProject {..} =
    undefined

------------------
-- Delete project
------------------

data GtrDeleteProject
  = GtrDeleteProjectByHash { gtrDeleteProjectByNameHash :: String }
  | GtrDeleteProjectByName { gtrDeleteProjectByNameName :: String }

instance GtrCommand GtrDeleteProject where
  execute GtrDeleteProjectByHash {..} = undefined
  execute GtrDeleteProjectByName {..} = undefined

