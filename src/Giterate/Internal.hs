module Giterate.Internal ( CmdResult
                         , createDirectory
                         , writeFile
                         , success
                         , (%>>=)
                         , (%>>)
                         , runGitCmd
                         , runGitCmd_
                         , gitAddDot
                         , gitCheckout
                         , gitCheckoutB
                         , gitCommit
                         , gitCommitRndMsg
                         , gitInit
                         , gitTransaction
                         ) where

import           Control.Applicative
import           Control.Monad
import           Data.UUID
import           Data.UUID.V4
import           GHC.IO.Handle
import           Prelude             hiding (writeFile)
import qualified Prelude             as P
import qualified System.Directory    as D
import           System.Exit
import           System.FilePath
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

runCmd_ :: [String] -> IO (ExitCode, String, String)
runCmd_ args = runCmd args empty

success :: IO () -> IO CmdResult
success = (>> return (ExitSuccess, "", ""))

infixl 1 %>>=
(%>>=) :: IO CmdResult -> PostProcess CmdResult -> IO CmdResult
result %>>= postProcess = do
  res@(exitCode, out, err) <- result
  case exitCode of
    ExitSuccess -> postProcess res
    _ -> pure (exitCode, out, err)

infixl 1 %>>
(%>>) :: IO CmdResult -> IO CmdResult -> IO CmdResult
proc1 %>> proc2 = proc1 %>>= const proc2

-------------------
-- File operations
-------------------

dontTouchMe :: String
dontTouchMe = ".dontTouchMe"

createDirectory :: FilePath -> IO ()
createDirectory dir = do
  D.createDirectoryIfMissing True dir
  P.writeFile (joinPath [dir, dontTouchMe]) ""

writeFile :: FilePath -> String -> IO ()
writeFile fp content = do
  createDirectory (takeDirectory fp)
  P.writeFile fp content

----------------
-- Internal Git
----------------

git :: String
git = "git"

runGitCmd :: [String] -> String -> IO CmdResult
runGitCmd = runCmd . (git:)

runGitCmd_ :: [String] -> IO CmdResult
runGitCmd_ = runCmd_ . (git:)

gitAddDot :: IO CmdResult
gitAddDot = runGitCmd_ ["add", "."]

gitCheckout :: String -> IO CmdResult
gitCheckout branch = runGitCmd_ ["checkout", branch]

gitCheckoutB :: String -> IO CmdResult
gitCheckoutB branch = runGitCmd_ ["checkout", "-b", branch]

gitCommit :: String -> IO CmdResult
gitCommit msg = runGitCmd_ ["commit", "-m", msg]

gitCommitRndMsg :: IO CmdResult
gitCommitRndMsg = nextRandom >>= (gitCommit . toString)

gitInit :: IO CmdResult
gitInit = runGitCmd_ ["init"]

gitResetHardHEAD :: IO CmdResult
gitResetHardHEAD = runGitCmd_ ["reset", "--hard", "HEAD"]

gitStatus :: IO CmdResult
gitStatus = runGitCmd_ ["status"]

gitTransaction :: IO CmdResult -> IO CmdResult
gitTransaction proc_ = do
  cmdResult@(exitCode, out, err) <- proc_ %>> gitAddDot %>> gitCommitRndMsg
  content <- D.getDirectoryContents "."
  case exitCode of
    ExitSuccess -> return cmdResult
    _ -> gitResetHardHEAD
