{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Giterate ( CmdResult
                , GtrCommand (..)
                , GtrInit (..)
                , GtrCreateProject (..)
                , GtrDeleteProject (..)
                ) where

import           Control.Monad
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

data GtrPrereqVerb = GtrIsInitialized
                   | GtrNothing

{-# INLINE defaultVerbErrMsg #-}
defaultVerbErrMsg :: GtrPrereqVerb -> String
defaultVerbErrMsg GtrIsInitialized = ""

require :: GtrPrereqVerb -> Maybe String -> IO CmdResult
require verb Nothing = undefined
require verb errmsg = execute (GtrPrereqCommand verb errmsg)

instance GtrCommand GtrPrereqVerb where
  execute GtrIsInitialized = undefined
  execute _ = undefined

data GtrPrereqCommand = GtrPrereqCommand GtrPrereqVerb String

instance GtrCommand GtrPrereqCommand where
  execute (GtrPrereqCommand a errmsg) = do
    cmdResult@(exitCode, _, _) <- execute a
    when (exitCode /= ExitSuccess) $ hPutStr stderr errmsg
    return cmdResult



----------------
-- Initial repo
----------------

data GtrInit = GtrInit

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
    require GtrIsInitialized ""

------------------
-- Delete project
------------------

data GtrDeleteProject
  = GtrDeleteProjectByHash { gtrDeleteProjectByNameHash :: String }
  | GtrDeleteProjectByName { gtrDeleteProjectByNameName :: String }

instance GtrCommand GtrDeleteProject where
  execute GtrDeleteProjectByHash {..} = undefined
  execute GtrDeleteProjectByName {..} = undefined

