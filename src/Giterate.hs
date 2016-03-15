{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Giterate ( CmdResult
                , GtrCommand (..)
                , GtrInit (..)
                , GtrCreateProject (..)
                , GtrDeleteProject (..)
                ) where

import           Data.Time.Clock
import           Giterate.Internal as GI
import           System.Exit
import           System.IO

{- ================= -}
{- Giterate commands -}
{- ================= -}

class GtrCommand cmd where
  execute :: cmd -> IO CmdResult

data GtrCommandCombo a b where
  GtrCommandCombo :: (GtrCommand a, GtrCommand b) => a -> b -> GtrCommandCombo a b

instance GtrCommand (GtrCommandCombo a b) where
  execute (GtrCommandCombo a b) = execute a %>> execute b

infixl 1 %%>>
(%%>>) :: (GtrCommand a, GtrCommand b) => a -> b -> GtrCommandCombo a b
a %%>> b = GtrCommandCombo a b

----------------------
-- Repo status checks
----------------------

data GtrPrereq a where
  GtrPrereq :: (GtrCommand a) => a -> String -> GtrPrereq a

instance GtrCommand (GtrPrereq a) where
  execute (GtrPrereq a errmsg) = do
    cmdResult@(exitCode, out, err) <- execute a
    case exitCode of
      ExitSuccess -> undefined
      _ -> hPutStr stderr errmsg >> return cmdResult

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

