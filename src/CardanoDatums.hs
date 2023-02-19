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

  -- genDatumHash,
  readDatumHash,

  beaconPolicy,
  beaconScript,
  beaconSymbol,

  spendValidator,
  spendValidatorScript,
  spendValidatorHash,

  writeScript,
  writeData,
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath) 
import qualified Prelude as Haskell
import Data.String (fromString)

import           Cardano.Api hiding (Address,Script,Value,TxOut)
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
-- genDatumHash :: ToData a => a -> DatumHash
-- genDatumHash = datumHash . Datum . toBuiltinData

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

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput (ScriptContext info (Spending ref)) = getScriptInput (txInfoInputs info) ref
ownInput _ = traceError "script input error"

{-# INLINABLE getScriptInput #-}
getScriptInput :: [TxInInfo] -> TxOutRef -> TxOut
getScriptInput [] _ = traceError "script input error"
getScriptInput ((TxInInfo iRef ot) : tl) ref
  | iRef == ref = ot
  | otherwise = getScriptInput tl ref

signed :: [PubKeyHash] -> PubKeyHash -> Bool
signed [] _ = False
signed (k:ks) k'
  | k == k' = True
  | otherwise = signed ks k'

-------------------------------------------------
-- Datum Beacon Settings
-------------------------------------------------
data BeaconRedeemer
  = MintBeacon DatumHash
  | BurnBeacon

PlutusTx.unstableMakeIsData ''BeaconRedeemer

-- | This is useful for testing without risk of accidentally locking production beacons on chain.
type AppName = BuiltinString

-------------------------------------------------
-- On-Chain Datum Beacon
-------------------------------------------------
mkBeaconPolicy :: AppName -> BeaconRedeemer -> ScriptContext -> Bool
mkBeaconPolicy appName r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
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
        in traceIfFalse "The beacon token name is not the datum's hash" (tn == name) &&
           traceIfFalse "One, and only one, beacon must be minted with this redeemer." (n == 1)
      (MintBeacon _, _) -> traceError "Can only mint beacon with datum hash as token name"
      (BurnBeacon, xs) ->
        traceIfFalse "Beacons can only be burned with this redeemer" (all (\(_,_,n) -> n < 0) xs)

    matchingDatum :: OutputDatum -> Datum -> Bool
    matchingDatum (OutputDatum d) dtm
      | d == dtm = True
      | otherwise = traceError "Beacon stored with different datum."
    matchingDatum _ _ = traceError 
                      $ "The " 
                     <> appName 
                     <> " beacon must be stored with the inline datum for the datum hash"
    
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

beaconPolicy' :: AppName -> MintingPolicy
beaconPolicy' appName = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
   ($$(PlutusTx.compile [|| wrap ||])
     `PlutusTx.applyCode` PlutusTx.liftCode appName)
  where
    wrap = mkUntypedMintingPolicy . mkBeaconPolicy

beaconPolicy :: MintingPolicy
beaconPolicy = beaconPolicy' "testing"

beaconScript :: Script
beaconScript = unMintingPolicyScript beaconPolicy

beaconSymbol :: CurrencySymbol
beaconSymbol = scriptCurrencySymbol beaconPolicy

-------------------------------------------------
-- On-Chain Spending Script
-------------------------------------------------
-- | This is a helper script to get the datum to appear inside a tx.
-- This script can be used with any datum and redeemer.
-- For convenience, it is recommended to use the datum as the redeemer.
mkSpendingScript :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkSpendingScript _ _ ctx@ScriptContext{scriptContextTxInfo = info} = stakingCredApproves
  where
    inputCredentials :: Address
    inputCredentials = 
      let TxOut{txOutAddress=addr} = ownInput ctx
      in addr

    stakingCredApproves :: Bool
    stakingCredApproves = case addressStakingCredential inputCredentials of
      -- | This is to prevent permanent locking of funds.
      -- The DEX is not meant to be used without a staking credential.
      Nothing -> True

      -- | Check if staking credential signals approval.
      Just stakeCred@(StakingHash cred) -> case cred of
        PubKeyCredential pkh -> signed (txInfoSignatories info) pkh
        ScriptCredential _ -> isJust $ Map.lookup stakeCred $ txInfoWdrl info
      
      Just _ -> traceError "Wrong kind of staking credential."

data Spend
instance ValidatorTypes Spend where
  type instance RedeemerType Spend = BuiltinData
  type instance DatumType Spend = BuiltinData

spendValidator :: Validator
spendValidator = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @Spend
    ($$(PlutusTx.compile [|| mkSpendingScript ||]))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator

spendValidatorScript :: Script
spendValidatorScript = unValidatorScript spendValidator

spendValidatorHash :: ValidatorHash
spendValidatorHash = Scripts.validatorHash spendValidator

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