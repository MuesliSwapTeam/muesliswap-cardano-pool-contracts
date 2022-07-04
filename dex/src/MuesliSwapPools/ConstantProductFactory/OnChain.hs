{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module MuesliSwapPools.ConstantProductFactory.OnChain
  ( mkFactoryCoin,
    mkFactoryScript,
    mkFactoryPolicy,
    mkOwnerTokenName,
    mkBatcherLicenseSymbol,
    mkSwapperLicenseSymbol,
  )
where

import Data.Maybe (fromJust)
import Ledger
import MuesliSwapPools.ConstantProductFactory.Types (FactoryParams (..), FactoryRedeemer (..))
import MuesliSwapPools.ConstantProductLiquidity.OnChain (mkLiquiditySymbol)
import MuesliSwapPools.ConstantProductPool.Types
import MuesliSwapPools.ConstantProductPool.Utils (calculateInitialLiquidity, minimumLiquidity)
import MuesliSwapPools.ConstantProductPoolNFT.OnChain (mkNFTSymbol)
import MuesliSwapPools.ConstantProductPoolNFT.Utils (poolNFTOf)
import qualified MuesliSwapPools.Spooky.TypedSpookyContexts as SC
import MuesliSwapPools.Types.Coin (assetClass, assetClassValueOf, tokenNameOf)
import MuesliSwapPools.Utils.OnChainUtils (assertPoolValue, mustFindScriptDatum', licenseDeadlineSC)
import qualified Plutonomy
import Plutus.V1.Ledger.Api (CurrencySymbol, MintingPolicy, unMintingPolicyScript)
import Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude
import Text.Hex (Text, decodeHex)

{-# INLINEABLE mkFactoryPolicy #-}
mkFactoryPolicy :: MintingPolicy
mkFactoryPolicy =
  Plutonomy.optimizeUPLC $
    Plutonomy.mintingPolicyToPlutus originalFactoryPolicy

{-# INLINEABLE mkFactoryScript #-}
mkFactoryScript :: Script
mkFactoryScript = unMintingPolicyScript mkFactoryPolicy

originalFactoryPolicy :: Plutonomy.MintingPolicy
originalFactoryPolicy =
  Plutonomy.mkMintingPolicyScript
    ( $$(PlutusTx.compile [||mkFactoryValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )
  where
    !params =
      FactoryParams
        { fpNftSymbol = mkNFTSymbol,
          fpLiquiditySymbol = mkLiquiditySymbol,
          fpFactoryTokenName = mkFactoryTokenName,
          fpCreatorLicenseSymbol = mkCreatorLicenseSymbol
        }

{-# INLINEABLE mkOwnerTokenName #-}
mkOwnerTokenName :: TokenName
mkOwnerTokenName = tokenName $ fromJust $ decodeHex "4f574e4552"

{-# INLINEABLE mkFactorySymbol #-}
mkFactorySymbol :: CurrencySymbol
mkFactorySymbol = scriptCurrencySymbol mkFactoryPolicy

{-# INLINEABLE mkFactoryTokenName #-}
mkFactoryTokenName :: TokenName
mkFactoryTokenName = tokenName $ fromJust $ decodeHex "4d7565736c69537761705f414d4d"

{-# INLINEABLE mkFactoryCoin #-}
mkFactoryCoin :: AssetClass
mkFactoryCoin = assetClass mkFactorySymbol mkFactoryTokenName

batcherLicenseSymbolHardCode :: Text.Hex.Text
batcherLicenseSymbolHardCode = "b686e45c9181618e20e26cf0e2fc1e9f336bb0df914e645b5adad5bd"

swapperLicenseSymbolHardCode :: Text.Hex.Text
swapperLicenseSymbolHardCode = "f94fd008635cf307663aadd995ed69d5fbbfd65f84679fc38254d664"

creatorLicenseSymbolHardCode :: Text.Hex.Text
creatorLicenseSymbolHardCode = "ed8074e8768199746ee58aed1f8b005eaa812afa44a268230603e969"

{-# INLINEABLE mkBatcherLicenseSymbol #-}
mkBatcherLicenseSymbol :: CurrencySymbol
mkBatcherLicenseSymbol = currencySymbol $ fromJust $ decodeHex batcherLicenseSymbolHardCode

{-# INLINEABLE mkSwapperLicenseSymbol #-}
mkSwapperLicenseSymbol :: CurrencySymbol
mkSwapperLicenseSymbol = currencySymbol $ fromJust $ decodeHex swapperLicenseSymbolHardCode

{-# INLINEABLE mkCreatorLicenseSymbol #-}
mkCreatorLicenseSymbol :: CurrencySymbol
mkCreatorLicenseSymbol = currencySymbol $ fromJust $ decodeHex creatorLicenseSymbolHardCode

-- | The 'mkFactoryValidator' function validates the Liquidity Pool is created correctly
--
-- 1.   Coin A < Coin B
-- 2,3. Validate that LP token is minted correctly
-- 4,5. Validate that amount of token AB must be higher than zero
-- 6.   Validate that the pool value must contains necessary token inside
-- 7.   Validate that identified NFT token must be minted with quantity = 1
--      and is in Pool Value
-- 8.   Validate that Factory token must be minted with quantity = 1
-- 9.   Validate that Factory is in Pool Value
-- 10.  Validate that RootKLast must not be set when Liquidity Pool is created
-- 11.  Validate that Profit Sharing must not be enable when Liquidity Pool is created
-- 12.  Only (DEX) owner can create pools
-- 13.  Bounds for fees
{-# INLINEABLE mkFactoryValidator #-}
mkFactoryValidator :: FactoryParams -> BuiltinData -> BuiltinData -> ()
mkFactoryValidator FactoryParams {..} rawRedeemer rawContext =
  case redeemer of
    CreatePool licenseIndex ->
      if (coinA < coinB) -- 1.
        && (actualLiquidity == expectedLiquidity) -- 2.
        && (actualTotalLiquidity == expectedTotalLiquidity) -- 3.
        && (amountA > 0) -- 4.
        && (amountB > 0) -- 5.
        && validPoolValue --6.
        && (assetClassValueOf forged nftCoinInOutput == 1) -- 7.
        && (assetClassValueOf forged factoryCoin == 1) -- 8.
        && (assetClassValueOf outVal factoryCoin == 1) -- 9.
        && (pdRootKLast poolDatum == 0) -- 10.
        && (isNothing (pdProfitSharing poolDatum)) -- 11.
        && (after (POSIXTime $ licenseDeadlineSC (licenseInput licenseIndex) fpCreatorLicenseSymbol) (SC.txInfoValidRange info)) -- 12.
        && (swapFee >= 1) && (swapFee <= 10000) && (extraFeeDenom >= 1) -- 13.
        then ()
        else error ()
  where
    redeemer = PlutusTx.unsafeFromBuiltinData @FactoryRedeemer rawRedeemer
    context = PlutusTx.unsafeFromBuiltinData rawContext
    info = SC.scriptContextTxInfo context
    ownSymbol = SC.ownCurrencySymbol context

    forged :: Value
    !forged = SC.txInfoMint info

    txOutputs :: [SC.TxOut]
    !txOutputs = SC.txInfoOutputs info

    -- Create Pool transaction only allows a single script Output (Pool UTxO)
    -- We:
    --     - Filtering which TxOut has datum hash (line 117),
    --     - Making sure that there is only one script TxOut (line 121)
    --     - Making sure that this TxOut contain identified NFT token in the value (line 127)
    ownOutput :: SC.TxOut
    !ownOutput = case [ o
                        | o <- txOutputs,
                          isJust (SC.txOutDatumHash o)
                      ] of
      [o] -> o
      _ -> error ()

    !poolDatum = mustFindScriptDatum' @PoolDatum ownOutput info
    coinA = pdCoinA poolDatum
    coinB = pdCoinB poolDatum
    !outVal = SC.txOutValue ownOutput
    validPoolValue = assertPoolValue coinA coinB lpCoin outVal
    !nftCoinInOutput = poolNFTOf outVal fpNftSymbol
    !amountA = assetClassValueOf outVal coinA
    !amountB = assetClassValueOf outVal coinB
    actualTotalLiquidity = pdTotalLiquidity poolDatum
    !expectedTotalLiquidity = calculateInitialLiquidity amountA amountB
    swapFee = pdSwapFee poolDatum
    extraFeeDenom = pdExtraFeeDenom poolDatum

    actualLiquidity = assetClassValueOf forged lpCoin
    expectedLiquidity = expectedTotalLiquidity - minimumLiquidity

    !lpCoin = assetClass fpLiquiditySymbol (tokenNameOf nftCoinInOutput)
    !factoryCoin = assetClass ownSymbol fpFactoryTokenName

    txInputs :: [SC.TxInInfo]
    txInputs = SC.txInfoInputs info

    licenseInput :: Integer -> SC.TxInInfo
    licenseInput licenseIndex = txInputs !! licenseIndex