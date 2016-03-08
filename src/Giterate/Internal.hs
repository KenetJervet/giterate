module Giterate.Internal where

import           Control.Applicative
import           Control.Monad
import           GHC.IO.Handle
import           System.Exit
import           System.Process

git :: String
git = "git"

type PostProcess = ExitCode -> String -> String -> IO ()

runGitCmd :: [String] -> String -> IO (ExitCode, String, String)
runGitCmd args input = do
  (Just hin, Just hout, Just herr, ph) <-
    createProcess (proc git args) { std_in = CreatePipe
                                  , std_out = CreatePipe
                                  , std_err = CreatePipe
                                  }
  hPutStr hin input
  exitCode <- waitForProcess ph
  out <- hGetContents hout
  err <- hGetContents herr
  return (exitCode, out, err)

runGitCmd_ :: [String] -> IO ()
runGitCmd_ args = void $ runGitCmd args empty
