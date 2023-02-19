{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey(..),
  queryBlockfrost,
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import qualified Data.Text as T
import Data.List (find)
import Data.Maybe (fromJust)

import CardanoDatums

-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | Wrapper around the beacon policy id
newtype BeaconId = BeaconId (CurrencySymbol,TokenName)

instance ToHttpApiData BeaconId where
  toQueryParam (BeaconId (sym,name)) = T.pack 
                                     $ show sym <> drop 2 (show name)  -- ^ drop prefix

data AssetHistory = AssetHistory
  { txHash :: String
  , action :: String
  } deriving (Show)

instance FromJSON AssetHistory where
  parseJSON (Object o) = 
    AssetHistory 
      <$> o .: "tx_hash" 
      <*> o .: "action"
  parseJSON _ = mzero

instance ToHttpApiData AssetHistory where
  toQueryParam x = T.pack $ txHash x

newtype TxOutputs = TxOutputs { unTxOutputs :: [UTxO] } deriving (Show)

instance FromJSON TxOutputs where
  parseJSON (Object o) = TxOutputs <$> o .: "outputs"
  parseJSON _ = mzero

data UTxO = UTxO
  { utxoAssets :: [UTxOAsset]
  , utxoDataHash :: Maybe String
  } deriving (Show)

instance FromJSON UTxO where
  parseJSON (Object o) =
    UTxO
      <$> o .: "amount"
      <*> o .: "data_hash"
  parseJSON _ = mzero

instance ToHttpApiData UTxO where
  toQueryParam = T.pack . fromJust . utxoDataHash

-- | CurrencySymbol <> TokenName
-- Blockfrost does not separate symbol and name with '.'
newtype UTxOAsset = UTxOAsset { unUTxOAsset :: String } deriving (Show)

instance FromJSON UTxOAsset where
  parseJSON (Object o) =
    UTxOAsset
      <$> o .: "unit"
  parseJSON _ = mzero

newtype InlineDatum = InlineDatum { unInlineDatum :: Value } deriving (Show)

instance FromJSON InlineDatum where
  parseJSON (Object o) = InlineDatum <$> o .: "json_value"
  parseJSON _ = mzero

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconId
    :> "history"
    :> QueryParam' '[Required] "count" Integer
    :> Get '[JSON] [AssetHistory]
  
  :<|> "txs"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "hash" AssetHistory
    :> "utxos"
    :> Get '[JSON] TxOutputs

  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" UTxO
    :> Get '[JSON] InlineDatum

assetHistoryApi :<|> txInfoApi :<|> datumInfoApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Query Blockfrost function
-------------------------------------------------
queryBlockfrost :: BlockfrostApiKey
                -> (CurrencySymbol,TokenName)
                -> ClientM Value
queryBlockfrost apiKey beaconId@(sym,name) = do
  let beacon = show sym <> drop 2 (show name)
  -- | The first one will always be the mint.
  mint <- head <$> assetHistoryApi apiKey (BeaconId beaconId) 1
  utxo <- fromJust 
        . find (elem beacon . map unUTxOAsset . utxoAssets) 
        . unTxOutputs 
      <$> txInfoApi apiKey mint
  dtm <- unInlineDatum <$> datumInfoApi apiKey utxo
  return dtm