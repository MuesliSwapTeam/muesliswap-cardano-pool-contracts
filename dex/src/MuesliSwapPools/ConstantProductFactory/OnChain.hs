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
    mkBatcherLicenseSymbol,
    mkSwapperLicenseSymbol,
  )
where

import Data.Maybe (fromJust)
import MuesliSwapPools.ConstantProductFactory.Types (FactoryParams (..), FactoryRedeemer (..))
import MuesliSwapPools.ConstantProductLiquidity.OnChain (mkLiquiditySymbol)
import MuesliSwapPools.ConstantProductPool.Types
import MuesliSwapPools.ConstantProductPool.Utils (calculateInitialLiquidity, minimumLiquidity)
import MuesliSwapPools.ConstantProductPoolNFT.OnChain (mkNFTSymbol)
import MuesliSwapPools.ConstantProductPoolNFT.Utils (poolNFTOf)
import MuesliSwapPools.Types.Coin (assetClassValueOf, tokenNameOf)
import MuesliSwapPools.Utils.OnChainUtils (assertPoolValue, mustFindScriptDatum, licenseDeadline, hasOutDatum)
import qualified Plutus.V2.Ledger.Api as V2
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol)
import Plutus.V1.Ledger.Interval (after)
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude
import Text.Hex (Text, decodeHex)


{-# INLINEABLE mkFactoryPolicy #-}
mkFactoryPolicy :: Scripts.MintingPolicy
mkFactoryPolicy = V2.mkMintingPolicyScript
  ( $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode params
  )
  where
    wrap = Scripts.mkUntypedMintingPolicy . mkFactoryValidator

    !params =
      FactoryParams
        { fpNftSymbol = mkNFTSymbol,
          fpLiquiditySymbol = mkLiquiditySymbol,
          fpFactoryTokenName = mkFactoryTokenName,
          fpCreatorLicenseSymbol = mkCreatorLicenseSymbol
        }

{-# INLINEABLE mkFactoryScript #-}
mkFactoryScript :: V2.Script
mkFactoryScript = V2.unMintingPolicyScript mkFactoryPolicy

{-# INLINEABLE mkFactorySymbol #-}
mkFactorySymbol :: V2.CurrencySymbol
mkFactorySymbol = Scripts.scriptCurrencySymbol mkFactoryPolicy

{-# INLINEABLE mkFactoryTokenName #-}
mkFactoryTokenName :: V2.TokenName
mkFactoryTokenName = V2.TokenName $ V2.toBuiltin $ fromJust $ decodeHex "4d7565736c69537761705f414d4d"

{-# INLINEABLE mkFactoryCoin #-}
mkFactoryCoin :: (V2.CurrencySymbol, V2.TokenName)
mkFactoryCoin = (mkFactorySymbol, mkFactoryTokenName)

batcherLicenseSymbolHardCode :: Text.Hex.Text
batcherLicenseSymbolHardCode = "b686e45c9181618e20e26cf0e2fc1e9f336bb0df914e645b5adad5bd"

swapperLicenseSymbolHardCode :: Text.Hex.Text
swapperLicenseSymbolHardCode = "f94fd008635cf307663aadd995ed69d5fbbfd65f84679fc38254d664"

creatorLicenseSymbolHardCode :: Text.Hex.Text
creatorLicenseSymbolHardCode = "ed8074e8768199746ee58aed1f8b005eaa812afa44a268230603e969"

{-# INLINEABLE mkBatcherLicenseSymbol #-}
mkBatcherLicenseSymbol :: V2.CurrencySymbol
mkBatcherLicenseSymbol = V2.CurrencySymbol $ V2.toBuiltin $ fromJust $ decodeHex batcherLicenseSymbolHardCode

{-# INLINEABLE mkSwapperLicenseSymbol #-}
mkSwapperLicenseSymbol :: V2.CurrencySymbol
mkSwapperLicenseSymbol = V2.CurrencySymbol $ V2.toBuiltin $ fromJust $ decodeHex swapperLicenseSymbolHardCode

{-# INLINEABLE mkCreatorLicenseSymbol #-}
mkCreatorLicenseSymbol :: V2.CurrencySymbol
mkCreatorLicenseSymbol = V2.CurrencySymbol $ V2.toBuiltin $ fromJust $ decodeHex creatorLicenseSymbolHardCode

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
-- 10.  Validate that non-expired creator license is provided
-- 11.  Validate bounds for fee
{-# INLINEABLE mkFactoryValidator #-}
mkFactoryValidator :: FactoryParams -> FactoryRedeemer -> V2.ScriptContext -> Bool
mkFactoryValidator FactoryParams {..} redeemer context =
  case redeemer of
    CreatePool licenseIndex ->
      coinA < coinB -- 1.
        && actualLiquidity == expectedLiquidity -- 2.
        && actualTotalLiquidity == expectedTotalLiquidity -- 3.
        && amountA > 0 -- 4.
        && amountB > 0 -- 5.
        && validPoolValue --6.
        && assetClassValueOf forged nftCoinInOutput == 1 -- 7.
        && assetClassValueOf forged factoryCoin == 1 -- 8.
        && assetClassValueOf outVal factoryCoin == 1 -- 9.
        && after (V2.POSIXTime $ licenseDeadline (licenseInput licenseIndex) fpCreatorLicenseSymbol) (V2.txInfoValidRange info) -- 10.
        && (swapFee >= 1) && (swapFee <= 10000) -- 11.
  where
    --redeemer = PlutusTx.unsafeFromBuiltinData @FactoryRedeemer rawRedeemer
    --context = PlutusTx.unsafeFromBuiltinData rawContext
    info = V2.scriptContextTxInfo context
    ownSymbol = ownCurrencySymbol context

    forged :: V2.Value
    !forged = V2.txInfoMint info

    txOutputs :: [V2.TxOut]
    !txOutputs = V2.txInfoOutputs info

    -- Create Pool transaction only allows a single script Output (Pool UTxO)
    -- We:
    --     - Filtering which TxOut has datum hash (line 117),
    --     - Making sure that there is only one script TxOut (line 121)
    --     - Making sure that this TxOut contain identified NFT token in the value (line 127)
    ownOutput :: V2.TxOut
    !ownOutput = case [ o | o <- txOutputs, hasOutDatum o ] of
      [o] -> o
      _ -> error ()

    !poolDatum = mustFindScriptDatum @PoolDatum ownOutput info
    coinA = pdCoinA poolDatum
    coinB = pdCoinB poolDatum
    !outVal = V2.txOutValue ownOutput
    validPoolValue = assertPoolValue coinA coinB lpCoin outVal
    !nftCoinInOutput = poolNFTOf outVal fpNftSymbol
    !amountA = assetClassValueOf outVal coinA
    !amountB = assetClassValueOf outVal coinB
    actualTotalLiquidity = pdTotalLiquidity poolDatum
    !expectedTotalLiquidity = calculateInitialLiquidity amountA amountB
    swapFee = pdSwapFee poolDatum

    actualLiquidity = assetClassValueOf forged lpCoin
    expectedLiquidity = expectedTotalLiquidity - minimumLiquidity

    !lpCoin = (fpLiquiditySymbol, tokenNameOf nftCoinInOutput)
    !factoryCoin = (ownSymbol, fpFactoryTokenName)

    txInputs :: [V2.TxInInfo]
    txInputs = V2.txInfoInputs info

    licenseInput :: Integer -> V2.TxInInfo
    licenseInput licenseIndex = txInputs !! licenseIndex