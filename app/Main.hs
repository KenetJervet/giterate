{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import qualified Giterate            as Gtr
import           Options.Applicative

data GtrArgs = GtrArgs { gtrCommand :: GtrCommand
                 }

data GtrCommand = GtrInit
                | GtrCreateProject { gtrCreateProjectName :: String }
                | GtrDeleteProject

gtrArgParser :: Parser GtrArgs
gtrArgParser = GtrArgs <$> gtrCommandParser

gtrCommandParser :: Parser GtrCommand
gtrCommandParser = subparser (
  command "init" (
      info (helper <*> gtrInitParser) (
          fullDesc
          <> progDesc "Initialize giterate repo"
          <> header "git-erate init - Initialize giterate repo"
          )
      )
  <>
  command "create" (
      info (helper <*> gtrCreateParser) (
          fullDesc
          <> progDesc "Create a new project"
          <> header "git-erate create - Create a new project"
          )
      )
  )

gtrCreateParser :: Parser GtrCommand
gtrCreateParser = GtrCreateProject <$> argument str (metavar "project_name")

gtrInitParser :: Parser GtrCommand
gtrInitParser = pure GtrInit

execute :: GtrArgs -> IO Gtr.CmdResult
execute GtrArgs {..} = execCmd gtrCommand

execCmd :: GtrCommand -> IO Gtr.CmdResult
execCmd GtrInit = Gtr.execute Gtr.GtrInit
execCmd GtrCreateProject {..} = do
  Gtr.execute Gtr.GtrCreateProject { gtrCreateProjectName = gtrCreateProjectName }
execCmd _ = error "Not implemented"

main :: IO ()
main = void (execParser opts >>= execute)
  where
    opts = info (helper <*> gtrArgParser)
      ( fullDesc
        <> progDesc "Project management powered by Git"
        <> header "git-erate - Project management powered by Git"
      )
