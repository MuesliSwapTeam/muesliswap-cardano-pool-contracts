{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MuesliSwapPools.ConstantProductPoolNFT.Utils
  ( poolNFTOf,
  )
where

import Ledger (AssetClass, Value)
import MuesliSwapPools.Types.Coin
import Plutus.V1.Ledger.Api (Value (Value))
import Plutus.V1.Ledger.Value (CurrencySymbol)
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude

{-# INLINEABLE poolNFTOf #-}
poolNFTOf :: Value -> CurrencySymbol -> AssetClass
poolNFTOf (Value v) nftSymbol = case Map.lookup nftSymbol v of
  Nothing -> error ()
  Just i -> case [o | o@(_, am) <- Map.toList i, am == 1] of
    [(tn, _)] -> assetClass nftSymbol tn
    _ -> error ()
