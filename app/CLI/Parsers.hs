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
  , command "query"
      (info parseQueryDatum $ progDesc "Query the chain for a datum.")
  ]

-------------------------------------------------
-- Query Parsers
-------------------------------------------------
parseQueryDatum :: Parser Command
parseQueryDatum = 
    QueryDatum
      <$> pDatumHash
      <*> pNetwork
      <*> pOutput
  where
    pDatumHash :: Parser DatumHash
    pDatumHash = option (eitherReader readDatumHash)
      (  long "datum-hash" 
      <> metavar "STRING" 
      <> help "Datum hash to lookup."
      )

    pNetwork :: Parser Network
    pNetwork = pMainnet <|> pPreProdTestnet
      where
        pMainnet :: Parser Network
        pMainnet = Mainnet <$> strOption
          (  long "mainnet"
          <> metavar "STRING"
          <> help "Query the mainnet using the Blockfrost Api with the supplied api key.")
        
        pPreProdTestnet :: Parser Network
        pPreProdTestnet = PreProdTestnet <$> strOption
          (  long "preprod-testnet"
          <> metavar "STRING"
          <> help "Query the preproduction testnet using the Blockfrost Api with the supplied api key.")

    pOutput :: Parser Output
    pOutput = pStdOut <|> File <$> pOutputFile
      where
        pStdOut :: Parser Output
        pStdOut = flag' Stdout
          (  long "stdout"
          <> help "Display to stdout."
          )

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