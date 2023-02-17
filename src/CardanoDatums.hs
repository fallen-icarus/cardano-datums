{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}

module CardanoDatums
(
  BeaconRedeemer(..),
  datumHashAsToken,

  readDatumHash,

  beaconPolicy,
  beaconScript,
  beaconSymbol,

  writeScript,
  writeData
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath) 
import qualified Prelude as Haskell
import Data.String (fromString)

import           Cardano.Api hiding (Script,Value,TxOut)
import           Cardano.Api.Shelley   (PlutusScript (..))
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
import Ledger.Bytes (fromHex)
import qualified Plutonomy
import Ledger.Value (valueOf,flattenValue)
import qualified PlutusTx.AssocMap as Map
import PlutusPrelude (foldl')

-------------------------------------------------
-- Off-Chain Helper Functions
-------------------------------------------------
-- | Parse DatumHash from user supplied String
readDatumHash :: Haskell.String -> Either Haskell.String DatumHash
readDatumHash s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ DatumHash bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-------------------------------------------------
-- Misc Functions
-------------------------------------------------
{-# INLINABLE datumHashAsToken #-}
datumHashAsToken :: DatumHash -> TokenName
datumHashAsToken (DatumHash hash) = TokenName hash

-------------------------------------------------
-- Datum Beacon Settings
-------------------------------------------------
data BeaconRedeemer
  = MintBeacon DatumHash
  | BurnBeacon

PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- On-Chain Datum Beacon
-------------------------------------------------
mkBeaconPolicy :: BeaconRedeemer -> ScriptContext -> Bool
mkBeaconPolicy r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
    MintBeacon dtmHash ->
      -- | Must mint one beacon with correct token name.
      mintCheck &&
      -- | Beacon must be stored in same utxo as associated inline datum.
      destinationCheck dtmHash
    BurnBeacon ->
      -- | Only used to burn.
      mintCheck

  where
    beaconSym :: CurrencySymbol
    beaconSym = ownCurrencySymbol ctx

    beaconsMinted :: [(CurrencySymbol,TokenName,Integer)]
    beaconsMinted = case Map.lookup beaconSym $ getValue $ txInfoMint info of
      Nothing -> traceError "MintError"
      Just bs -> flattenValue $ Value $ Map.insert beaconSym bs Map.empty -- ^ a Value with only beacons

    mintCheck :: Bool
    mintCheck = case (r,beaconsMinted) of
      (MintBeacon dtmHash, [(_,tn,n)]) ->
        let name = datumHashAsToken dtmHash
        in traceIfFalse "Only the beacon with an empty token name can be minted" (tn == name) &&
           traceIfFalse "One, and only one, beacon must be minted with this redeemer." (n == 1)
      (MintBeacon _, _) -> traceError "Can only mint beacon with datum hash as token name"
      (BurnBeacon, xs) ->
        traceIfFalse "Beacons can only be burned with this redeemer" (all (\(_,_,n) -> n < 0) xs)

    matchingDatum :: OutputDatum -> Datum -> Bool
    matchingDatum (OutputDatum d) dtm = d == dtm
    matchingDatum _ _ = traceError "Beacon must be stored with the inline datum for the datum hash"
    
    -- | Check if the beacon is minted to the utxo containing the associated inline datum.
    destinationCheck :: DatumHash -> Bool
    destinationCheck dtmHash =
      let outputs = txInfoOutputs info
          name = datumHashAsToken dtmHash
          dtm = case Map.lookup dtmHash $ txInfoData info of
            Nothing -> traceError "Datum not present in tx"
            Just d' -> d'
          foo acc TxOut{txOutDatum=d
                       ,txOutValue=oVal
                       } =
            if valueOf oVal beaconSym name == 1
            then matchingDatum d dtm  -- ^ Either crashes or returns True
            else acc
      in foldl' foo True outputs

beaconPolicy :: MintingPolicy
beaconPolicy = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
   $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy mkBeaconPolicy

beaconScript :: Script
beaconScript = unMintingPolicyScript beaconPolicy

beaconSymbol :: CurrencySymbol
beaconSymbol = scriptCurrencySymbol beaconPolicy

-------------------------------------------------
-- Serialization
-------------------------------------------------
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file 
               . encode 
               . scriptDataToJson ScriptDataJsonDetailedSchema 
               . dataToScriptData 
               . PlutusTx.toData

serialisedScript :: Script -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

writeScript :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScript file script = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                         $ serialisedScript script

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData = writeJSON