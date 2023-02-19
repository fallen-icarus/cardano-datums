module Main where

import Options.Applicative

import CLI.Parsers (parseCommand)
import CLI.Run

main :: IO ()
main = do
  let preferences = prefs $ showHelpOnError <> showHelpOnEmpty
      opts = info (parseCommand <**> helper) (fullDesc <> progDesc "A distributed datum map on Cardano")
  customExecParser preferences opts >>= runCommand