{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module MuesliSwapPools.ConstantProductPoolNFT.OnChain
  ( mkNFTSymbol,
    mkNFTTokenName,
    mkNFTScript,
    mkNFTPolicy,
  )
where

import Data.Maybe (fromJust)
import MuesliSwapPools.Types.Coin (isUnity)
import MuesliSwapPools.Utils.OnChainUtils (integerToBS)
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import qualified Plutus.V2.Ledger.Api as V2
import Plutus.V2.Ledger.Contexts (TxOutRef (TxOutRef))
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, spendsOutput)
import qualified PlutusTx
import PlutusTx.Prelude
import Text.Hex (decodeHex)

{-# INLINEABLE mkNFTPolicy #-}
mkNFTPolicy :: Scripts.MintingPolicy
mkNFTPolicy = V2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedMintingPolicy validateMintNFT

{-# INLINEABLE mkNFTScript #-}
mkNFTScript :: V2.Script
mkNFTScript = V2.unMintingPolicyScript mkNFTPolicy

{-# INLINEABLE mkNFTSymbol #-}
mkNFTSymbol :: V2.CurrencySymbol
mkNFTSymbol = Scripts.scriptCurrencySymbol mkNFTPolicy

{-# INLINEABLE mkNFTTokenName #-}
mkNFTTokenName :: TxOutRef -> V2.TokenName
mkNFTTokenName (TxOutRef refHash refIdx) = tokenName
  where
    tokenName :: V2.TokenName
    tokenName = V2.TokenName $ sha2_256 $ V2.getTxId refHash <> integerToBS refIdx

-- | The 'validateMintNFT' function validates the NFT token is minted correctly
--
-- 1.   Validate that UTxO has TxHash & TxIndex above (*) has been spent in this transaction
-- 2.   Validate that NFT has correct TokenName (sha256 of TxHash + TxIndex (*))
{-# INLINEABLE validateMintNFT #-}
validateMintNFT :: TxOutRef -> V2.ScriptContext -> Bool
validateMintNFT ref@(TxOutRef refHash refIdx) context =
  let --ref@(TxOutRef refHash refIdx) = PlutusTx.unsafeFromBuiltinData @TxOutRef rawRedeemer
      --context = PlutusTx.unsafeFromBuiltinData rawContext
      info = V2.scriptContextTxInfo context
      ownSymbol = ownCurrencySymbol context
      mintValue = V2.txInfoMint info
   in spendsOutput info refHash refIdx -- 1.
    && isUnity mintValue (ownSymbol, mkNFTTokenName ref) -- 2.