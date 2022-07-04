{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module MuesliSwapPools.BatchOrder.Types
  ( OrderStep (..),
    OrderDatum (..),
    scriptVersion,
    OrderRedeemer (..),
    POrderRedeemer (..),
    POrderDatum (..),
  )
where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic)
import Ledger (AssetClass, DatumHash)
import Ledger.Address (Address)
import Plutarch.Api.V1 (PDatumHash)
import Plutarch.Api.V1.Address (PAddress)
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.DataRepr
  ( DerivePConstantViaData (DerivePConstantViaData),
    PDataFields,
    PIsDataReprInstances (PIsDataReprInstances),
    PLabeledType ((:=)),
  )
import Plutarch.ByteString (PByteString)
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import Plutarch.Prelude
import qualified PlutusTx
import PlutusTx.Prelude (Integer, Maybe, BuiltinByteString)
import qualified Prelude as Haskell

data SwapOrderStep

data OrderStep
  = Deposit
      { dMinimumLP :: Integer
      }
  | Withdraw
      { wMinimumCoinA :: Integer,
        wMinimumCoinB :: Integer
      }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed
  ''OrderStep
  [ ('Deposit, 0),
    ('Withdraw, 1)
  ]
PlutusTx.makeLift ''OrderStep

data OrderDatum = OrderDatum
  { odSender :: Address,
    odReceiver :: Address,
    odReceiverDatumHash :: Maybe DatumHash,
    odStep :: OrderStep,
    odBatcherFee :: Integer,
    odOutputADA :: Integer,
    odScriptVersion :: BuiltinByteString
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''OrderDatum [('OrderDatum, 0)]
PlutusTx.makeLift ''OrderDatum

{-# INLINEABLE scriptVersion #-}
scriptVersion :: BuiltinByteString
scriptVersion = "MuesliSwap_AMM"

data OrderRedeemer = ApplyOrder | CancelOrder

PlutusTx.makeIsDataIndexed
  ''OrderRedeemer
  [ ('ApplyOrder, 0),
    ('CancelOrder, 1)
  ]
PlutusTx.makeLift ''OrderRedeemer

-- | Plutarch synonym to 'OrderRedeemer'.
data POrderRedeemer (s :: S) = PApplyOrder (Term s (PDataRecord '[])) | PCancelOrder (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances POrderRedeemer)

instance PUnsafeLiftDecl POrderRedeemer where type PLifted POrderRedeemer = OrderRedeemer

deriving via (DerivePConstantViaData OrderRedeemer POrderRedeemer) instance (PConstant OrderRedeemer)

-- | Plutarch synonym to 'OrderDatum'.
newtype POrderDatum (s :: S)
  = POrderDatum
      ( Term
          s
          ( PDataRecord
              '[ "sender" ':= PAddress,
                 "receiver" ':= PAddress,
                 "receiverDatumHash" ':= PMaybeData PDatumHash,
                 -- TODO: This should ideally use the Plutarch synonym of 'POrderStep'.
                 "step" ':= PData,
                 "batcherFee" ':= PInteger,
                 "outputAda" ':= PInteger,
                 "scriptVersion" ':= PByteString
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via (PIsDataReprInstances POrderDatum)

instance PUnsafeLiftDecl POrderDatum where type PLifted POrderDatum = OrderDatum

deriving via (DerivePConstantViaData OrderDatum POrderDatum) instance (PConstant OrderDatum)
