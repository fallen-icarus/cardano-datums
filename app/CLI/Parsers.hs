module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative

import CardanoDatums
import CLI.Types

-------------------------------------------------
-- Main Parser
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "export-policy"
      (info pExportBeaconPolicy $ progDesc "Export the datum beacon policy.")
  , command "redeemer"
      (info pCreateBeaconRedeemer $ progDesc "Create the redeemer for the datum beacon policy.")
  , command "export-helper-script"
      (info pExportHelperScript $ progDesc "Export the helper script.")
  ]

-------------------------------------------------
-- Beacon Parsers
-------------------------------------------------
pExportBeaconPolicy :: Parser Command
pExportBeaconPolicy = ExportBeaconPolicy <$> pOutputFile

pCreateBeaconRedeemer :: Parser Command
pCreateBeaconRedeemer = 
    CreateBeaconRedeemer
      <$> (pMint <|> pBurn)
      <*> pOutputFile
  where
    pMint :: Parser BeaconRedeemer
    pMint = MintBeacon <$> option (eitherReader readDatumHash)
      (  long "mint-beacon" 
      <> metavar "STRING" 
      <> help "Mint a datum beacon for the supplied datum hash."
      )

    pBurn :: Parser BeaconRedeemer
    pBurn = flag' BurnBeacon
      (  long "burn-beacon"
      <> help "Burn a datum beacon."
      )

pExportHelperScript :: Parser Command
pExportHelperScript = ExportHelperScript <$> pOutputFile

-------------------------------------------------
-- Misc Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
  )