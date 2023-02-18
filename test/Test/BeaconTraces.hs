{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Test.BeaconTraces 
(
  tests,
  testTrace
) where

import Data.Void (Void)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash)
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.V2.Ledger.Api
import Data.List (foldl')
import Prelude as Haskell (Semigroup (..),IO)
import Test.Tasty
import Plutus.Contract.Test as Test

import CardanoDatums

-------------------------------------------------
-- Helpers
-------------------------------------------------
toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum Nothing val

datumTokenName :: Datum -> TokenName
datumTokenName = datumHashAsToken . datumHash

data Datums = Unit () | Key PaymentPubKeyHash | Rational Rational
  deriving (Generic,ToJSON,FromJSON)

instance PlutusTx.ToData Datums where
  toBuiltinData (Unit ()) = PlutusTx.toBuiltinData ()
  toBuiltinData (Key pkh) = PlutusTx.toBuiltinData pkh
  toBuiltinData (Rational r) = PlutusTx.toBuiltinData r

-------------------------------------------------
-- Trace Configs
-------------------------------------------------
data InitializeParams = InitializeParams
  { initializeAddress :: Address
  , initializeUtxo :: (Datum,Value)
  } deriving (Generic,ToJSON,FromJSON)

data MintBeaconsParams = MintBeaconsParams
  { beaconsMinted :: [(TokenName,Integer)]
  , useMintRedeemer :: Bool
  , addressWithDatum :: Address
  , datumInputUtxo :: (Datum,Value)
  , mintToAddress :: Address
  , beaconOutput :: (Maybe Datum,Value)
  , asInline :: Bool
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema = 
      Endpoint "initialize" InitializeParams
  .\/ Endpoint "mint-beacons" MintBeaconsParams

-------------------------------------------------
-- Models
-------------------------------------------------
initialize :: InitializeParams -> Contract () TraceSchema Text ()
initialize InitializeParams{..} = do
  let lookups = plutusV2MintingPolicy beaconPolicy
    
      tx' = mustPayToAddressWith 
              initializeAddress
              (Just $ TxOutDatumHash $ fst initializeUtxo)
              (snd initializeUtxo)
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  

mintBeacons :: MintBeaconsParams -> Contract () TraceSchema Text ()
mintBeacons MintBeaconsParams{..} = do
  utxos <- utxosAt addressWithDatum

  let dtmHash = datumHash $ fst datumInputUtxo
    
      beaconRedeemer = toRedeemer $ MintBeacon $ datumHash $ fst datumInputUtxo
      beaconPolicyHash = mintingPolicyHash beaconPolicy
      
      toDatum' :: Datum -> TxOutDatum Datum
      toDatum'
        | asInline = TxOutDatumInline
        | otherwise = TxOutDatumHash
  
      lookups = plutusV2MintingPolicy beaconPolicy
             <> Constraints.unspentOutputs utxos
             <> plutusV2OtherScript spendValidator
             <> otherData (fst datumInputUtxo)
      tx' = 
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          beaconsMinted
        )
        -- | Must spend utxo with datum already initialized
        <> mustSpendScriptOutputWithMatchingDatumAndValue 
              spendValidatorHash 
              (== fst datumInputUtxo)
              (== snd datumInputUtxo) 
              (toRedeemer ())
        <> mustIncludeDatumInTxWithHash dtmHash (fst datumInputUtxo)
        -- | Must store with datum
        <> mustPayToAddressWith 
            mintToAddress 
            (fmap toDatum' $ fst beaconOutput) 
            (snd beaconOutput)
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

-------------------------------------------------
-- Enpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    initialize' = endpoint @"initialize" initialize
    mintBeacon' = endpoint @"mint-beacons" mintBeacons
    choices =
      [ initialize'
      , mintBeacon'
      ]

-------------------------------------------------
-- Scenarios
-------------------------------------------------
properMint :: EmulatorTrace ()
properMint = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let initialDtm = toDatum ()
      dtmInName = toDatum ()
      outputDtm = toDatum ()
      userKey = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1
      datumAddr = Address (ScriptCredential spendValidatorHash) Nothing
      mintAddr = Address (PubKeyCredential userKey) Nothing

  callEndpoint @"initialize" h1 $
    InitializeParams
      { initializeAddress = datumAddr
      , initializeUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      }

  void $ waitUntilSlot 2

  callEndpoint @"mint-beacons" h1 $
    MintBeaconsParams
      { beaconsMinted = [(datumTokenName dtmInName,1)]
      , useMintRedeemer = True
      , beaconOutput = 
          ( Just outputDtm
          , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (datumTokenName dtmInName) 1
          )
      , addressWithDatum = datumAddr
      , datumInputUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      , mintToAddress = mintAddr
      , asInline = True
      }

mintBeaconWithDifferentName :: EmulatorTrace ()
mintBeaconWithDifferentName = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let initialDtm = toDatum ()
      dtmInName = toDatum (10 :: Integer)
      outputDtm = toDatum ()
      userKey = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1
      datumAddr = Address (ScriptCredential spendValidatorHash) Nothing
      mintAddr = Address (PubKeyCredential userKey) Nothing

  callEndpoint @"initialize" h1 $
    InitializeParams
      { initializeAddress = datumAddr
      , initializeUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      }

  void $ waitUntilSlot 2

  callEndpoint @"mint-beacons" h1 $
    MintBeaconsParams
      { beaconsMinted = [(datumTokenName dtmInName,1)]
      , useMintRedeemer = True
      , beaconOutput = 
          ( Just outputDtm
          , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (datumTokenName dtmInName) 1
          )
      , addressWithDatum = datumAddr
      , datumInputUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      , mintToAddress = mintAddr
      , asInline = True
      }

mintToUtxoWithDifferentDatum :: EmulatorTrace ()
mintToUtxoWithDifferentDatum = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let initialDtm = toDatum ()
      dtmInName = toDatum ()
      outputDtm = toDatum (10 :: Integer)
      userKey = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1
      datumAddr = Address (ScriptCredential spendValidatorHash) Nothing
      mintAddr = Address (PubKeyCredential userKey) Nothing

  callEndpoint @"initialize" h1 $
    InitializeParams
      { initializeAddress = datumAddr
      , initializeUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      }

  void $ waitUntilSlot 2

  callEndpoint @"mint-beacons" h1 $
    MintBeaconsParams
      { beaconsMinted = [(datumTokenName dtmInName,1)]
      , useMintRedeemer = True
      , beaconOutput = 
          ( Just outputDtm
          , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (datumTokenName dtmInName) 1
          )
      , addressWithDatum = datumAddr
      , datumInputUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      , mintToAddress = mintAddr
      , asInline = True
      }

mintToUtxoWithoutDatum :: EmulatorTrace ()
mintToUtxoWithoutDatum = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let initialDtm = toDatum ()
      dtmInName = toDatum ()
      -- outputDtm = toDatum ()
      userKey = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1
      datumAddr = Address (ScriptCredential spendValidatorHash) Nothing
      mintAddr = Address (PubKeyCredential userKey) Nothing

  callEndpoint @"initialize" h1 $
    InitializeParams
      { initializeAddress = datumAddr
      , initializeUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      }

  void $ waitUntilSlot 2

  callEndpoint @"mint-beacons" h1 $
    MintBeaconsParams
      { beaconsMinted = [(datumTokenName dtmInName,1)]
      , useMintRedeemer = True
      , beaconOutput = 
          ( Nothing
          , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (datumTokenName dtmInName) 1
          )
      , addressWithDatum = datumAddr
      , datumInputUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      , mintToAddress = mintAddr
      , asInline = True
      }

mintToUtxoWithoutInlineDatum :: EmulatorTrace ()
mintToUtxoWithoutInlineDatum = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let initialDtm = toDatum ()
      dtmInName = toDatum ()
      outputDtm = toDatum ()
      userKey = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1
      datumAddr = Address (ScriptCredential spendValidatorHash) Nothing
      mintAddr = Address (PubKeyCredential userKey) Nothing

  callEndpoint @"initialize" h1 $
    InitializeParams
      { initializeAddress = datumAddr
      , initializeUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      }

  void $ waitUntilSlot 2

  callEndpoint @"mint-beacons" h1 $
    MintBeaconsParams
      { beaconsMinted = [(datumTokenName dtmInName,1)]
      , useMintRedeemer = True
      , beaconOutput = 
          ( Just outputDtm
          , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (datumTokenName dtmInName) 1
          )
      , addressWithDatum = datumAddr
      , datumInputUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      , mintToAddress = mintAddr
      , asInline = False
      }

mintSeveralBeaconsForDatum :: EmulatorTrace ()
mintSeveralBeaconsForDatum = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let initialDtm = toDatum ()
      dtmInName = toDatum ()
      outputDtm = toDatum ()
      userKey = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1
      datumAddr = Address (ScriptCredential spendValidatorHash) Nothing
      mintAddr = Address (PubKeyCredential userKey) Nothing

  callEndpoint @"initialize" h1 $
    InitializeParams
      { initializeAddress = datumAddr
      , initializeUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      }

  void $ waitUntilSlot 2

  callEndpoint @"mint-beacons" h1 $
    MintBeaconsParams
      { beaconsMinted = [(datumTokenName dtmInName,3)]
      , useMintRedeemer = True
      , beaconOutput = 
          ( Just outputDtm
          , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (datumTokenName dtmInName) 3
          )
      , addressWithDatum = datumAddr
      , datumInputUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      , mintToAddress = mintAddr
      , asInline = True
      }

mintBeaconsForMultipleDatums :: EmulatorTrace ()
mintBeaconsForMultipleDatums = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let initialDtm = toDatum ()
      dtmInName = toDatum ()
      outputDtm = toDatum ()
      userKey = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1
      datumAddr = Address (ScriptCredential spendValidatorHash) Nothing
      mintAddr = Address (PubKeyCredential userKey) Nothing

  callEndpoint @"initialize" h1 $
    InitializeParams
      { initializeAddress = datumAddr
      , initializeUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      }

  void $ waitUntilSlot 2

  callEndpoint @"mint-beacons" h1 $
    MintBeaconsParams
      { beaconsMinted = 
          [ (datumTokenName dtmInName,1)
          , (adaToken,1)
          ]
      , useMintRedeemer = True
      , beaconOutput = 
          ( Just outputDtm
          , lovelaceValueOf 2_000_000 
         <> singleton beaconSymbol (datumTokenName dtmInName) 1
         <> singleton beaconSymbol adaToken 1
          )
      , addressWithDatum = datumAddr
      , datumInputUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      , mintToAddress = mintAddr
      , asInline = True
      }

properMint2 :: EmulatorTrace ()
properMint2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let initialDtm = toDatum (10 :: Integer)
      dtmInName = toDatum (10 :: Integer)
      outputDtm = toDatum (10 :: Integer)
      userKey = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1
      datumAddr = Address (ScriptCredential spendValidatorHash) Nothing
      mintAddr = Address (PubKeyCredential userKey) Nothing

  callEndpoint @"initialize" h1 $
    InitializeParams
      { initializeAddress = datumAddr
      , initializeUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      }

  void $ waitUntilSlot 2

  callEndpoint @"mint-beacons" h1 $
    MintBeaconsParams
      { beaconsMinted = [(datumTokenName dtmInName,1)]
      , useMintRedeemer = True
      , beaconOutput = 
          ( Just outputDtm
          , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (datumTokenName dtmInName) 1
          )
      , addressWithDatum = datumAddr
      , datumInputUtxo = (initialDtm, lovelaceValueOf 10_000_000)
      , mintToAddress = mintAddr
      , asInline = True
      }

-------------------------------------------------
-- Test Functions
-------------------------------------------------
tests :: TestTree
tests = do
  testGroup "Datum Beacon"
    [ checkPredicate "Fail if beacon token name /= datum's hash"
        (Test.not assertNoFailedTransactions) mintBeaconWithDifferentName
    , checkPredicate "Fail if beacon stored in utxo with different datum"
        (Test.not assertNoFailedTransactions) mintToUtxoWithDifferentDatum
    , checkPredicate "Fail if beacon not stored with an datum"
        (Test.not assertNoFailedTransactions) mintToUtxoWithoutDatum
    , checkPredicate "Fail if beacon not stored with an inline datum"
        (Test.not assertNoFailedTransactions) mintToUtxoWithoutInlineDatum
    , checkPredicate "Fail if more than one beacon minted for datum"
        (Test.not assertNoFailedTransactions) mintSeveralBeaconsForDatum
    , checkPredicate "Fail if other beacons minted in same tx"
        (Test.not assertNoFailedTransactions) mintBeaconsForMultipleDatums
    , checkPredicate "Successfully mint beacon"
        assertNoFailedTransactions properMint
    , checkPredicate "Successfully mint beacon for an entirely different datum"
        assertNoFailedTransactions properMint2
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO properMint2