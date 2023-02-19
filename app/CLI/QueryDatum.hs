{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.QueryDatum
(
  runQuery
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception
import Data.Aeson (Value)

import CLI.BlockfrostApi
import CLI.Types
import CardanoDatums (CurrencySymbol,TokenName)

-- | Takes the beacon symbol, the target asset, and the desired network to query the relevant
-- off-chain api endpoint.
runQuery :: (CurrencySymbol,TokenName) -> Network -> IO Value
runQuery beaconSym network = do
  manager' <- newManager tlsManagerSettings
  case network of
    PreProdTestnet apiKey -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (queryBlockfrost apiKey' beaconSym) env
      case res of
        Right r -> return r
        Left err -> throw err
    Mainnet apiKey -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-mainnet.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (queryBlockfrost apiKey' beaconSym) env
      case res of
        Right r -> return r
        Left err -> throw err