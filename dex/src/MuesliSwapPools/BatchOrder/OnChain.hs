{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module MuesliSwapPools.BatchOrder.OnChain (mkBatchOrderScript) where

import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import MuesliSwapPools.BatchOrder.Types (OrderDatum(..), OrderRedeemer(..))
import MuesliSwapPools.ConstantProductPool.OnChain (mkPoolScript)
import Plutus.V2.Ledger.Api as V2
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Address (toValidatorHash)
import qualified PlutusTx
import PlutusTx.Prelude


mkBatchOrderScript :: V2.Script
mkBatchOrderScript = V2.unValidatorScript batcherScript

batcherScript :: V2.Validator
batcherScript = V2.mkValidatorScript $
  $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode poolValHash
    where
      wrap = Scripts.mkUntypedValidator . mkBatchOrderValidator

      poolValHash :: V2.ValidatorHash
      !poolValHash = Scripts.validatorHash $ V2.Validator mkPoolScript

{-# INLINEABLE mkBatchOrderValidator #-}
mkBatchOrderValidator :: V2.ValidatorHash -> OrderDatum -> OrderRedeemer -> V2.ScriptContext -> Bool
mkBatchOrderValidator poolScriptHash datum redeemer ctx = case redeemer of
    ApplyOrder -> hasOnePoolInput
    CancelOrder -> validSig
  where
    --datum = PlutusTx.unsafeFromBuiltinData @OrderDatum rawDatum
    --redeemer = PlutusTx.unsafeFromBuiltinData @OrderRedeemer rawRedeemer
    --ctx = PlutusTx.unsafeFromBuiltinData @ScriptContext rawContext

    txInfo :: V2.TxInfo
    txInfo = V2.scriptContextTxInfo ctx

    addrToPkh :: V2.Address -> V2.PubKeyHash
    addrToPkh (V2.Address (V2.PubKeyCredential k) _) = k

    validSig :: Bool
    validSig = txSignedBy txInfo $ addrToPkh $ odSender datum

    isPoolAddress :: V2.ValidatorHash -> TxOut -> Bool
    isPoolAddress vh o = case toValidatorHash $ txOutAddress o of
        Just vh' -> vh == vh'
        _ -> False
    
    hasOnePoolInput :: Bool
    hasOnePoolInput = case filter (isPoolAddress poolScriptHash) [txInInfoResolved i | i <- V2.txInfoInputs txInfo] of
        [o] -> True
        _ -> False