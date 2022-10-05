{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MuesliSwapPools.ConstantProductPoolNFT.Utils
  ( poolNFTOf,
  )
where

import MuesliSwapPools.Types.Coin
import qualified Plutus.V2.Ledger.Api as V2
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude

{-# INLINEABLE poolNFTOf #-}
poolNFTOf :: V2.Value -> V2.CurrencySymbol -> (V2.CurrencySymbol, V2.TokenName)
poolNFTOf (V2.Value v) nftSymbol = case Map.lookup nftSymbol v of
  Nothing -> error ()
  Just i -> case [o | o@(_, am) <- Map.toList i, am == 1] of
    [(tn, _)] -> (nftSymbol, tn)
    _ -> error ()