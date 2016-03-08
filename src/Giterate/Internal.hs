{-# LANGUAGE RecordWildCards #-}

module Giterate.Internal ( runGitCmd
                         , runGitCmd_
                         , GtrCommand
                         , GtrCreateProject (..)
                         , GtrDeleteProject (..)
                         ) where

import           Control.Applicative
import           Control.Monad
import           GHC.IO.Handle
import           System.Exit
import           System.Process

type CmdResult = (ExitCode, String, String)

type PostProcess a = CmdResult -> IO a

runCmd :: [String] -> String -> IO (ExitCode, String, String)
runCmd [] _ = error "No command to run"
runCmd (exec:args) input = do
  (Just hin, Just hout, Just herr, ph) <-
    createProcess (proc exec args) { std_in = CreatePipe
                                   , std_out = CreatePipe
                                   , std_err = CreatePipe
                                   }
  hPutStr hin input
  exitCode <- waitForProcess ph
  out <- hGetContents hout
  err <- hGetContents herr
  return (exitCode, out, err)

runCmd_ :: [String] -> IO ()
runCmd_ args = void $ runCmd args empty

infixl 1 %>>=
(%>>=) :: IO CmdResult -> PostProcess () -> IO ()
result %>>= postProcess = do
  res@(exitCode, _, _) <- result
  case exitCode of
    ExitSuccess -> postProcess res
    _ -> pure ()

----------------
-- Internal Git
----------------

git :: String
git = "git"

runGitCmd :: [String] -> String -> IO (ExitCode, String, String)
runGitCmd = runCmd . (git:)

runGitCmd_ :: [String] -> IO ()
runGitCmd_ = runCmd_ . (git:)


{- ================= -}
{- Giterate commands -}
{- ================= -}

class GtrCommand cmd where
  execute :: cmd -> IO ()

type TimeStamp = Double

------------------
-- Create project
------------------

data GtrCreateProject
  = GtrCreateProject { gtrCreateProjectName :: String
                     , gtrCreateProjectTime :: TimeStamp
                     }

instance GtrCommand GtrCreateProject where
  execute GtrCreateProject {..} = undefined %>>= undefined

------------------
-- Delete project
------------------

data GtrDeleteProject
  = GtrDeleteProjectByHash { gtrDeleteProjectByNameHash :: String }
  | GtrDeleteProjectByName { gtrDeleteProjectByNameName :: String }

instance GtrCommand GtrDeleteProject where
  execute method = case method of
    GtrDeleteProjectByHash {..} -> undefined
    GtrDeleteProjectByName {..} -> undefined
