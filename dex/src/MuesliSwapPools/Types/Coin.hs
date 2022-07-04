{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module MuesliSwapPools.Types.Coin
  ( assetClassValue,
    unitValue,
    isUnity,
    assetClassValueOf,
    assetClass,
    adaCoin,
    symbolOf,
    tokenNameOf,
  )
where

import Ledger (AssetClass, CurrencySymbol, TokenName, Value)
import Ledger.Ada (adaSymbol, adaToken)
import Ledger.Value (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import Plutus.V1.Ledger.Api (Value (getValue))
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude

{-# INLINEABLE unitValue #-}
unitValue :: AssetClass -> Value
unitValue c = assetClassValue c 1

{-# INLINEABLE isUnity #-}
isUnity :: Value -> AssetClass -> Bool
isUnity v c = Map.lookup curr (getValue v) == Just (Map.fromList [(tok, 1)])
  where
    (curr, tok) = unAssetClass c

{-# INLINEABLE adaCoin #-}
adaCoin :: AssetClass
adaCoin = assetClass adaSymbol adaToken

{-# INLINEABLE symbolOf #-}
symbolOf :: AssetClass -> CurrencySymbol
symbolOf (AssetClass (cs, _)) = cs

{-# INLINEABLE tokenNameOf #-}
tokenNameOf :: AssetClass -> TokenName
tokenNameOf (AssetClass (_, tn)) = tn
