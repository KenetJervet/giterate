module Main where

import Options.Applicative

data GtrArgs = GtrArgs { gtrCommand :: GtrCommand
                 }

data GtrCommand = GtrCreate
                | GtrDelete

gtrArgParser :: Parser GtrArgs
gtrArgParser = GtrArgs <$> gtrCommandParser

gtrCommandParser :: Parser GtrCommand
gtrCommandParser = subparser (
  command "create" (
      info (helper <*> gtrCreateParser) (
          fullDesc
          <> progDesc "Create a new project"
          <> header "git-erate create - Create a new project"
          )
      )
  )

gtrCreateParser :: Parser GtrCommand
gtrCreateParser = pure GtrCreate

execute :: GtrArgs -> IO ()
execute _ = putStrLn "Git-erate"

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> gtrArgParser)
      ( fullDesc
        <> progDesc "Project management powered by Git"
        <> header "git-erate - Project management powered by Git"
      )
