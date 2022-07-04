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
import Ledger
  ( CurrencySymbol,
    MintingPolicy,
    Script,
    TokenName,
    TxId (getTxId),
    TxOutRef (TxOutRef),
    scriptCurrencySymbol,
    unMintingPolicyScript,
  )
import qualified MuesliSwapPools.Spooky.TypedSpookyContexts as SC
import MuesliSwapPools.Types.Coin (assetClass, isUnity)
import MuesliSwapPools.Utils.OnChainUtils (integerToBS)
import qualified Plutonomy
import Plutus.V1.Ledger.Value (TokenName (TokenName), tokenName)
import qualified PlutusTx
import PlutusTx.Prelude
import Text.Hex (decodeHex)

{-# INLINEABLE mkNFTPolicy #-}
mkNFTPolicy :: MintingPolicy
mkNFTPolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus originalNFTPolicy

{-# INLINEABLE mkNFTScript #-}
mkNFTScript :: Script
mkNFTScript = unMintingPolicyScript mkNFTPolicy

originalNFTPolicy :: Plutonomy.MintingPolicy
originalNFTPolicy =
  Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [||validateMintNFT||])

{-# INLINEABLE mkNFTSymbol #-}
mkNFTSymbol :: CurrencySymbol
mkNFTSymbol = scriptCurrencySymbol mkNFTPolicy

{-# INLINEABLE mkNFTTokenName #-}
mkNFTTokenName :: TxOutRef -> TokenName
mkNFTTokenName (TxOutRef refHash refIdx) = tokenName
  where
    tokenName :: TokenName
    tokenName = TokenName $ sha2_256 $ getTxId refHash <> integerToBS refIdx

-- | The 'validateMintNFT' function validates the NFT token is minted correctly
--
-- 1.   Validate that UTxO has TxHash & TxIndex above (*) has been spent in this transaction
-- 2.   Validate that NFT has correct TokenName (sha256 of TxHash + TxIndex (*))
{-# INLINEABLE validateMintNFT #-}
validateMintNFT :: BuiltinData -> BuiltinData -> ()
validateMintNFT rawRedeemer rawContext =
  let ref@(TxOutRef refHash refIdx) = PlutusTx.unsafeFromBuiltinData @TxOutRef rawRedeemer
      context = PlutusTx.unsafeFromBuiltinData rawContext
      info = SC.scriptContextTxInfo context
      ownSymbol = SC.ownCurrencySymbol context
      mintValue = SC.txInfoMint info
   in if (SC.spendsOutput info refHash refIdx) -- 1.
        then
          if (isUnity mintValue (assetClass ownSymbol (mkNFTTokenName ref))) -- 2.
            then ()
            else error ()
        else error ()
