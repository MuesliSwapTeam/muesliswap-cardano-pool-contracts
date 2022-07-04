{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module MuesliSwapPools.ConstantProductPool.OnChain (mkPoolScript) where

import Data.Bool (Bool (True))
import Data.Maybe (fromJust)
import Ledger
  ( CurrencySymbol,
    POSIXTime (POSIXTime),
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoOutRef, txInInfoResolved),
    TxInfo
      ( txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories,
        txInfoValidRange
      ),
    TxOut (..),
    Value,
    addressCredential,
    after,
    findOwnInput,
    toPubKeyHash,
    toValidatorHash,
    validatorHash,
  )
import Ledger.Value (assetClassValueOf)
import MuesliSwapPools.BatchOrder.Types
  ( OrderDatum (..),
    OrderStep (Deposit, Withdraw),
  )
import MuesliSwapPools.ConstantProductFactory.OnChain
  ( mkBatcherLicenseSymbol,
    mkFactoryCoin,
    mkOwnerTokenName,
    mkSwapperLicenseSymbol,
  )
import MuesliSwapPools.ConstantProductLiquidity.OnChain
  ( mkLiquiditySymbol,
  )
import MuesliSwapPools.ConstantProductPool.Types
  ( PoolDatum
      ( PoolDatum,
        pdCoinA,
        pdCoinB,
        pdProfitSharing,
        pdRootKLast,
        pdTotalLiquidity,
        pdSwapFee,
        pdExtraFeeDenom
      ),
    PoolParams (..),
    PoolRedeemer (ApplyPool, DirectSwap, UpdateFeeTo, WithdrawLiquidityShare),
    psFeeTo,
    psFeeToDatumHash,
  )
import MuesliSwapPools.ConstantProductPool.Utils
  ( calSqrt,
    calculateDepositAmount,
    calculateProfitSharing,
    hasNoInputFromBatcher,
    hasSwapperLicense,
  )
import MuesliSwapPools.ConstantProductPoolNFT.OnChain (mkNFTSymbol)
import MuesliSwapPools.ConstantProductPoolNFT.Utils
  ( poolNFTOf,
  )
import MuesliSwapPools.Types.Coin
  ( adaCoin,
    assetClass,
    assetClassValueOf,
    tokenNameOf,
  )
import MuesliSwapPools.Utils.OnChainUtils
  ( assertPoolValue,
    bsToInteger,
    mustFindScriptDatum,
    licenseDeadline,
  )
import qualified Plutonomy
import Plutus.V1.Ledger.Api (Address, Script, unValidatorScript)
import Plutus.V1.Ledger.Scripts (Validator (..))
import Plutus.V1.Ledger.Value (AssetClass, TokenName (TokenName), Value (Value), assetClassValue, currencySymbol, tokenName)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude
import Text.Hex (Text, decodeHex)

mkPoolScript :: Script
mkPoolScript =
  unValidatorScript $
    Plutonomy.optimizeUPLC $
      Plutonomy.validatorToPlutus originalPoolScript

originalPoolScript :: Plutonomy.Validator
originalPoolScript =
  Plutonomy.mkValidatorScript
    ( $$(PlutusTx.compile [||mkPoolValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode poolParams
    )
  where
    !poolParams =
      PoolParams
        { ppNftSymbol = mkNFTSymbol,
          ppLiquiditySymbol = mkLiquiditySymbol,
          ppFactoryCoin = mkFactoryCoin,
          ppBatcherLicenseSymbol = mkBatcherLicenseSymbol,
          ppSwapperLicenseSymbol = mkSwapperLicenseSymbol,
          ppOwnerTokenName = mkOwnerTokenName,
          v2ValHash = "f1ec90d33208778214cdc7fa90858ac5620253d99f84c10335928cab"
        }

{-# INLINEABLE mkPoolValidator #-}
mkPoolValidator ::
  PoolParams ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkPoolValidator pp rawDatum rawRedeemer rawContext =
  if case redeemer of
    ApplyPool batcherAddress licenseIndex ->
      validateApplyPool pp datum ctx batcherAddress licenseIndex
    DirectSwap licenseIndex ->
      validateDirectSwap pp datum ctx licenseIndex
    UpdateFeeTo ownerIndex ->
      validateUpdateFeeTo pp datum ctx ownerIndex
    WithdrawLiquidityShare ownerIndex feeToIndex ->
      validateWithdrawLiquidityShare pp datum ctx ownerIndex feeToIndex
    then ()
    else error ()
  where
    datum = PlutusTx.unsafeFromBuiltinData @PoolDatum rawDatum
    redeemer = PlutusTx.unsafeFromBuiltinData @PoolRedeemer rawRedeemer
    ctx = PlutusTx.unsafeFromBuiltinData @ScriptContext rawContext

-- | The 'validateDirectSwap' function validates that ...
--
-- 1.  The pool value must contains necessary token inside
-- 2.  Pool's NFT token must be the same between input and output
-- 3.  Factory NFT token in input have quantity = 1
-- 4.  Factory NFT token in output have quantity = 1
-- 5.  Number of LP tokens must be the same between input and output
-- 6.  Validate that no asset will be minted in this action
-- 7.  Constant product formula must be satisfied (up to fee)
-- 8.  All value being used as input for swapping must come from orderbook V2
-- 9.  Pool datum must remain unchanged
-- 10. Swapper license token must be present and not be expired
validateDirectSwap ::
  -- | Params of PoolValidator
  PoolParams ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | ScriptContext
  ScriptContext ->
  -- | licenseIndex
  Integer ->
  Bool
validateDirectSwap
  PoolParams
    { ppNftSymbol,
      ppLiquiditySymbol,
      ppSwapperLicenseSymbol,
      ppFactoryCoin,
      v2ValHash
    }
  datum@PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdRootKLast = rootKLastIn,
      pdTotalLiquidity = totalLiquidityIn,
      pdSwapFee = swapFee
    }
  ctx
  licenseIndex =
    let info :: TxInfo
        info = scriptContextTxInfo ctx

        txInputs :: [TxInInfo]
        !txInputs = txInfoInputs info

        txOutputs :: [TxOut]
        !txOutputs = txInfoOutputs info

        ownInput :: TxInInfo
        !ownInput = case findOwnInput ctx of
          Just txInInfo -> txInInfo
          _ -> error ()

        !ownAddress = txOutAddress $ txInInfoResolved ownInput
        !inVal = txOutValue $ txInInfoResolved ownInput
        !nftCoinInInput = poolNFTOf inVal ppNftSymbol

        ownOutput :: TxOut
        !ownOutput = case [ o | o <- txOutputs,
                            txOutAddress o == ownAddress,
                            assetClassValueOf (txOutValue o) nftCoinInInput == 1
                          ] of
          [o] -> o
          _ -> error ()

        !outVal = txOutValue ownOutput

        outputPoolDatum = mustFindScriptDatum @PoolDatum ownOutput info
        validTimeRange = txInfoValidRange info
        validPoolValue = assertPoolValue coinA coinB lpCoin outVal
        nftCoinInOutput = poolNFTOf outVal ppNftSymbol
        factoryCoinIn = assetClassValueOf inVal ppFactoryCoin
        lpCoin = assetClass ppLiquiditySymbol (tokenNameOf nftCoinInInput)
        lpCoinIn = assetClassValueOf inVal lpCoin
        lpCoinOut = assetClassValueOf outVal lpCoin
        factoryCoinOut = assetClassValueOf outVal ppFactoryCoin
        amountAIn = assetClassValueOf inVal coinA
        amountBIn = assetClassValueOf inVal coinB
        amountAOut = assetClassValueOf outVal coinA
        amountBOut = assetClassValueOf outVal coinB

        licenseInput :: TxInInfo
        licenseInput = txInputs !! licenseIndex

        checkSwap :: Integer -> Integer -> Integer -> Integer -> Bool
        checkSwap oldA' oldB' newA' newB' =
          (oldA > 0) && (oldB > 0) && (newA > 0) && (newB > 0)
            && ( (((newA * feeDen) - (inA * feeNum)) * ((newB * feeDen) - (inB * feeNum)))
                   >= (feeDen * feeDen * oldA * oldB)
               )
          where
            oldA = oldA'
            oldB = oldB'
            newA = newA'
            newB = newB'

            inA = max 0 $ newA - oldA
            inB = max 0 $ newB - oldB

            -- For info on fees, see: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
            feeNum = swapFee
            feeDen = 10000

        constantProduct :: Bool
        constantProduct = checkSwap amountAIn amountBIn amountAOut amountBOut
        
     in validPoolValue -- 1.
          && (nftCoinInInput == nftCoinInOutput) -- 2.
          && (factoryCoinIn == 1) -- 3.
          && (factoryCoinOut == 1) -- 4.
          && (lpCoinIn == lpCoinOut) -- 5.
          && (txInfoMint info == mempty) -- 6.
          && constantProduct -- 7.
          && (datum == outputPoolDatum) -- 8.
          && (after (POSIXTime $ licenseDeadline licenseInput ppSwapperLicenseSymbol) validTimeRange) -- 9.
          && (hasNoInputFromBatcher txInputs info) -- 10.

-- | The 'validateApplyPool' function validates the orders will be correctly applied to the pool
--   by valid batcher
--
-- 1. Validate that the pool value must contains necessary token inside
-- 2. A valid batcher must sign the transaction
-- 3. The license of batcher is not expired
-- 4. Pool's NFT token must be the same between input and output
-- 5. Factory NFT token in input have quantity = 1
-- 6. Factory NFT token in output have quantity = 1
-- 7. The delta liquidity after applying through all orders must be the same
--    with minted amount of LP token
-- 8. Validate that pool value in the output must be the same with the calculated amount
--    after applying through all orders
-- 9. CoinA, CoinB, SwapFee, ExtraFeeDenom fields of datum remain unchanged
validateApplyPool ::
  -- | Params of PoolValidator
  PoolParams ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | ScriptContext
  ScriptContext ->
  -- | Address of valid batcher which will submit this batch transaction
  Address ->
  -- | Index of license's UTxO in the transaction inputs
  Integer ->
  Bool
validateApplyPool
  PoolParams
    { ppNftSymbol,
      ppLiquiditySymbol,
      ppBatcherLicenseSymbol,
      ppFactoryCoin
    }
  datum@PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdRootKLast = rootKLastIn,
      pdTotalLiquidity = totalLiquidityIn,
      pdSwapFee = swapFee,
      pdExtraFeeDenom = extraFeeDenom
    }
  ctx
  batcherAddress
  licenseIndex =
    let info :: TxInfo
        info = scriptContextTxInfo ctx

        signatories :: [PubKeyHash]
        signatories = txInfoSignatories info

        txInputs :: [TxInInfo]
        !txInputs = txInfoInputs info

        txOutputs :: [TxOut]
        !txOutputs = txInfoOutputs info

        ownInput :: TxInInfo
        !ownInput = case findOwnInput ctx of
          Just txInInfo -> txInInfo
          _ -> error ()

        !ownAddress = txOutAddress $ txInInfoResolved ownInput

        ownOutput :: TxOut
        !ownOutput = case [o | o <- txOutputs, ownAddress == txOutAddress o] of
          [o] -> o
          _ -> error ()

        outputPoolDatum = mustFindScriptDatum @PoolDatum ownOutput info
        rootKLastOut = pdRootKLast outputPoolDatum

        !inVal = txOutValue $ txInInfoResolved ownInput
        !outVal = txOutValue ownOutput

        mintValue = txInfoMint info
        validTimeRange = txInfoValidRange info
        validPoolValue = assertPoolValue coinA coinB lpCoin outVal
        nftCoinInInput = poolNFTOf inVal ppNftSymbol
        nftCoinInOutput = poolNFTOf outVal ppNftSymbol
        lpCoin = assetClass ppLiquiditySymbol (tokenNameOf nftCoinInInput)
        totalLiquidityOut = pdTotalLiquidity outputPoolDatum
        factoryCoinIn = assetClassValueOf inVal ppFactoryCoin
        factoryCoinOut = assetClassValueOf outVal ppFactoryCoin
        amountAIn = assetClassValueOf inVal coinA
        amountBIn = assetClassValueOf inVal coinB
        amountAOut = assetClassValueOf outVal coinA
        amountBOut = assetClassValueOf outVal coinB
        liquidityShareIn = assetClassValueOf inVal lpCoin
        liquidityShareOut = assetClassValueOf outVal lpCoin
        totalDeltaLiquidity = totalLiquidityOut - totalLiquidityIn

        applySuccess =
          applyOrders
            userInputs
            userOutputs
            (amountAIn, amountBIn, totalLiquidityIn, liquidityShareIn, rootKLastIn)

        batcherPubKeyHash = case toPubKeyHash batcherAddress of
          Just pkh -> pkh
          Nothing -> error ()

        licenseInput :: TxInInfo
        licenseInput = txInputs !! licenseIndex

        userInputs :: [TxInInfo]
        userInputs = [i | i <- txInputs, isUserInput i]

        userOutputs :: [TxOut]
        userOutputs = [o | o <- txOutputs, isUserOutput o]

        isUserInput :: TxInInfo -> Bool
        isUserInput txIn =
          txInInfoOutRef txIn /= txInInfoOutRef ownInput
            && txOutAddress (txInInfoResolved txIn) /= batcherAddress

        isUserOutput :: TxOut -> Bool
        isUserOutput txOut =
          let addr = txOutAddress txOut
           in txOutAddress txOut /= txOutAddress ownOutput && addr /= batcherAddress

        applyStep' = \iv od st o -> applyStep iv od st o datum lpCoin

        applyOrders ::
          [TxInInfo] ->
          [TxOut] ->
          (Integer, Integer, Integer, Integer, Integer) ->
          Bool
        applyOrders [] [] (i1, i2, i3, i4, i5) =
          i1 == amountAOut
            && i2 == amountBOut
            && i3 == totalLiquidityOut
            && i4 == liquidityShareOut
            && i5 == rootKLastOut
        applyOrders [] _ _ = error ()
        applyOrders _ [] _ = error ()
        applyOrders (i : ins) (o : outs) state =
          let txOut = txInInfoResolved i
              inputValue = txOutValue txOut
              orderDatum = mustFindScriptDatum txOut info
              newState = applyStep' inputValue orderDatum state o
           in applyOrders ins outs newState

        untamperedDatum = coinA == pdCoinA outputPoolDatum
          && coinB == pdCoinB outputPoolDatum
          && swapFee == pdSwapFee outputPoolDatum
          && extraFeeDenom == pdExtraFeeDenom outputPoolDatum

     in validPoolValue -- 1.
          && (batcherPubKeyHash `elem` signatories) -- 2.
          && (after (POSIXTime $ licenseDeadline licenseInput ppBatcherLicenseSymbol) validTimeRange) -- 3.
          && (nftCoinInInput == nftCoinInOutput) -- 4.
          && (factoryCoinIn == 1) -- 5.
          && (factoryCoinOut == 1) -- 6.
          && (assetClassValueOf mintValue lpCoin == totalDeltaLiquidity) -- 7.
          && applySuccess -- 8.
          && untamperedDatum -- 9.

-- | The 'validateUpdateFeeTo' function validates the profit sharing information in the pool datum
--   has been updated by correct Owner
--
-- 1. Pool value must be the same between input and output
-- 2. Pool's NFT token must be the same between input and output
-- 3. Validate that only Owner can perform the transaction
-- 4. Validate that if datum changes, RootKLastOut must be reset after enabling/disabling Profit Sharing feature
--    If datum doesn't change, it is updating Script Stake's credential case
-- 5. Validate that no asset will be minted in this action
-- 6. Validate that no utxo is spent from scripts other than pool (e.g. batcher)
--    (Missing in original Minswap contract!)
-- 7. CoinA, CoinB, SwapFee, ExtraFeeDenom fields of datum remain unchanged
validateUpdateFeeTo ::
  -- | Params of PoolValidator
  PoolParams ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | ScriptContext
  ScriptContext ->
  -- | Index of (fee license) owner's UTxO in the transaction inputs
  Integer ->
  Bool
validateUpdateFeeTo
  PoolParams {ppNftSymbol, ppBatcherLicenseSymbol, ppOwnerTokenName}
  datum
  ctx
  ownerIndex =
    let info :: TxInfo
        info = scriptContextTxInfo ctx

        txInputs :: [TxInInfo]
        txInputs = txInfoInputs info

        txOutputs :: [TxOut]
        txOutputs = txInfoOutputs info

        ownInput :: TxInInfo
        ownInput = case findOwnInput ctx of
          Just txInInfo -> txInInfo
          _ -> error ()

        ownAddress = txOutAddress $ txInInfoResolved ownInput

        ownOutput :: TxOut
        ownOutput = case [ o
                           | o <- txOutputs,
                             addressCredential ownAddress == addressCredential (txOutAddress o)
                         ] of
          [o] -> o
          _ -> error ()

        outputPoolDatum = mustFindScriptDatum @PoolDatum ownOutput info
        rootKLastOut = pdRootKLast outputPoolDatum

        inVal = txOutValue $ txInInfoResolved ownInput
        outVal = txOutValue ownOutput

        nftCoinInInput = poolNFTOf inVal ppNftSymbol
        nftCoinInOutput = poolNFTOf outVal ppNftSymbol

        ownerInput :: TxInInfo
        ownerInput = txInputs !! ownerIndex

        hasOwner =
          let (Value mp) = txOutValue $ txInInfoResolved ownerInput
              ownerTokenName = case Map.lookup ppBatcherLicenseSymbol mp of
                Just i -> case Map.toList i of
                  [(tn, _)] -> tn
                  _ -> error ()
                _ -> error ()
           in ownerTokenName == ppOwnerTokenName

        validateDatum :: Bool
        validateDatum =
          if datum == outputPoolDatum
            then True
            else rootKLastOut == 0

        untamperedDatum = pdCoinA datum == pdCoinA outputPoolDatum
          && pdCoinB datum == pdCoinB outputPoolDatum
          && pdSwapFee datum == pdSwapFee outputPoolDatum
          && pdExtraFeeDenom datum == pdExtraFeeDenom outputPoolDatum

     in (inVal == outVal) -- 1.
          && (nftCoinInInput == nftCoinInOutput) -- 2.
          && hasOwner -- 3.
          && validateDatum -- 4.
          && (txInfoMint info == mempty) -- 5.
          && (hasNoInputFromBatcher txInputs info) -- 6.
          && untamperedDatum -- 7.

-- | The 'validateWithdrawLiquidityShare' function validates the profit sharing value will be withdrawed
--   to the correct FeeTo address
--
-- 1. Pool datum must be the same between input and output
-- 2. Pool's NFT token must be the same between input and output
-- 3. Validate that profit sharing value must be withdrawed to the correct FeeTo address
-- 4. Validate that Pool Value in output = Pool Value in input - amount of liquidity share
-- 5. Validate that only Owner can perform the transaction
-- 6. Validate that no asset will be minted in this action
-- 7. Validate that no utxo is spent from scripts other than pool (e.g. batcher)
--    (Missing in original Minswap contract!)
-- 8. CoinA, CoinB, SwapFee, ExtraFeeDenom fields of datum remain unchanged
validateWithdrawLiquidityShare ::
  -- | Params of PoolValidator
  PoolParams ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | ScriptContext
  ScriptContext ->
  -- | Index of owner's UTxO in the transaction inputs
  Integer ->
  -- | Index to FeeTo's UTxO in the transaction outputs
  Integer ->
  Bool
validateWithdrawLiquidityShare
  PoolParams {ppLiquiditySymbol, ppNftSymbol, ppBatcherLicenseSymbol, ppOwnerTokenName}
  datum
  ctx
  ownerIndex
  feeToIndex =
    case pdProfitSharing datum of
      Nothing -> False
      Just ps ->
        let info :: TxInfo
            info = scriptContextTxInfo ctx

            txInputs :: [TxInInfo]
            txInputs = txInfoInputs info

            txOutputs :: [TxOut]
            txOutputs = txInfoOutputs info

            ownInput :: TxInInfo
            ownInput = case findOwnInput ctx of
              Just txInInfo -> txInInfo
              _ -> error ()

            ownAddress = txOutAddress $ txInInfoResolved ownInput

            ownOutput :: TxOut
            ownOutput = case [o | o <- txOutputs, ownAddress == txOutAddress o] of
              [o] -> o
              _ -> error ()

            ownerInput :: TxInInfo
            ownerInput = txInputs !! ownerIndex

            outputPoolDatum = mustFindScriptDatum @PoolDatum ownOutput info

            inVal = txOutValue $ txInInfoResolved ownInput
            outVal = txOutValue ownOutput

            nftCoinInInput = poolNFTOf inVal ppNftSymbol
            nftCoinInOutput = poolNFTOf outVal ppNftSymbol
            feeToOutput = txOutputs !! feeToIndex
            lpCoin = assetClass ppLiquiditySymbol (tokenNameOf nftCoinInInput)
            liquidityShare = assetClassValueOf inVal lpCoin
            expectedOutVal = inVal <> assetClassValue lpCoin (negate liquidityShare)
            validFeeToOutput =
              assetClassValueOf (txOutValue feeToOutput) lpCoin == liquidityShare
                && txOutAddress feeToOutput == psFeeTo ps
                && txOutDatumHash feeToOutput == psFeeToDatumHash ps
            hasOwner =
              let (Value mp) = txOutValue $ txInInfoResolved ownerInput
                  ownerTokenName = case Map.lookup ppBatcherLicenseSymbol mp of
                    Just i -> case Map.toList i of
                      [(tn, _)] -> tn
                      _ -> error ()
                    _ -> error ()
               in ownerTokenName == ppOwnerTokenName

            untamperedDatum = pdCoinA datum == pdCoinA outputPoolDatum
              && pdCoinB datum == pdCoinB outputPoolDatum
              && pdSwapFee datum == pdSwapFee outputPoolDatum
              && pdExtraFeeDenom datum == pdExtraFeeDenom outputPoolDatum

         in (datum == outputPoolDatum) -- 1.
              && (nftCoinInInput == nftCoinInOutput) -- 2.
              && validFeeToOutput -- 3.
              && (expectedOutVal == outVal) -- 4.
              && hasOwner -- 5.
              && (txInfoMint info == mempty) -- 6.
              && (hasNoInputFromBatcher txInputs info) -- 7.
              && untamperedDatum -- 8.

-- | The 'applyStep' function validates the order and updates the pool state.
applyStep ::
  -- | Input value
  Value ->
  -- | Order
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer, Integer, Integer) ->
  -- | Transaction output
  TxOut ->
  -- | Information about the pool
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  AssetClass ->
  (Integer, Integer, Integer, Integer, Integer)
applyStep
  !val
  od@OrderDatum
    { odBatcherFee,
      odOutputADA,
      odStep
    }
  state
  txo
  pd
  lpCoin =
    if odBatcherFee <= 0 || odOutputADA <= 0
      then error ()
      else case odStep of
        Deposit minimumLP ->
          validateDeposit val od state txo pd lpCoin minimumLP
        Withdraw minimumCoinA minimumCoinB ->
          validateWithdraw val od state txo pd lpCoin minimumCoinA minimumCoinB

-- |
--  The 'validateDeposit' function validates that the LP coins are given to the
--  liquidity provider in exchange for their A&B coins.
--
--  = Logic outline
--
--    * Calculate the LP coin amount to give the user in exchange for their
--      deposited A&B coin amounts
--
--        * If the rate deltaA / amountA != deltaB / amountB, account for the
--          change and refund the change balance to the receiver
--
--        * If there's enough LP coins in the pool to reward the deposit
--
--           * Validate whether the value of is sent to the correct
--             user (receiver)
--           * If profit sharing is enable, then we mint the amount of LP token
--             and give it back to Pool Value
--
--        * If there's NOT enough then throwing error
validateDeposit ::
  -- | The input Value with A&B coins
  Value ->
  -- | The Datum with the sender, receiver and fee information
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer, Integer, Integer) ->
  -- | Transaction output for sending out the LP coins
  TxOut ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  AssetClass ->
  -- | Desired minimum amount of LP coin to get
  Integer ->
  (Integer, Integer, Integer, Integer, Integer)
validateDeposit
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash
    }
  ( !amountA,
    !amountB,
    !totalLiquidity,
    !liquidityShare,
    !rootKLast
    )
  TxOut
    { txOutValue = txOutVal,
      txOutAddress,
      txOutDatumHash
    }
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdProfitSharing,
      pdExtraFeeDenom = extraFeeDenom
    }
  lpCoin
  minimumLP =
    let !feeOn = isJust pdProfitSharing
        coinAAmount = assetClassValueOf val coinA
        coinBAmount = assetClassValueOf val coinB
        !deltaA =
          if coinA == adaCoin
            then coinAAmount - (odBatcherFee + odOutputADA)
            else coinAAmount
        !deltaB =
          if coinB == adaCoin
            then coinBAmount - (odBatcherFee + odOutputADA)
            else coinBAmount
        -- If the rate deltaA / amountA != deltaB / amountB (this case happens
        -- when having swapping actions before this depositing action because of
        -- this rate is changed), we should recalculate the real deltaA & deltaB
        -- amount and refund the change balance to the receiver (the deltaA' &
        -- deltaB')
        (!deltaA', !deltaB', !deltaLiquidity) =
          case calculateDepositAmount deltaA deltaB amountA amountB totalLiquidity of
            Just da -> da
            _ -> error ()
        lpAmountOut = assetClassValueOf txOutVal lpCoin
        adaAmountOut = assetClassValueOf txOutVal adaCoin
        coinAAmountOut = assetClassValueOf txOutVal coinA
        coinBAmountOut = assetClassValueOf txOutVal coinB
        validAmount
          | deltaB > deltaB' =
            if coinB == adaCoin
              then (adaAmountOut == deltaB - deltaB' + odOutputADA) && (lpAmountOut == deltaLiquidity)
              else
                (adaAmountOut == odOutputADA)
                  && (lpAmountOut == deltaLiquidity)
                  && (deltaB - deltaB' == coinBAmountOut)
          | deltaA > deltaA' =
            if coinA == adaCoin
              then (adaAmountOut == deltaA - deltaA' + odOutputADA) && (lpAmountOut == deltaLiquidity)
              else
                (adaAmountOut == odOutputADA)
                  && (lpAmountOut == deltaLiquidity)
                  && (deltaA - deltaA' == coinAAmountOut)
          | otherwise = (lpAmountOut == deltaLiquidity) && (adaAmountOut == odOutputADA)
        validReceiver = (txOutAddress == odReceiver) && (txOutDatumHash == odReceiverDatumHash)
        newAmountA = amountA + deltaA'
        newAmountB = amountB + deltaB'
        deltaLiquidityShare =
          if feeOn
            then calculateProfitSharing rootKLast amountA amountB totalLiquidity extraFeeDenom
            else 0
        newRootKLast = if feeOn then calSqrt (newAmountA * newAmountB) else rootKLast
     in if (deltaLiquidity >= minimumLP) && validAmount && validReceiver
          then
            ( newAmountA,
              newAmountB,
              totalLiquidity + deltaLiquidity + deltaLiquidityShare,
              liquidityShare + deltaLiquidityShare,
              newRootKLast
            )
          else error ()

-- |
--  The 'validateWithdraw' function validates that A&B coins are given back to
--  the liquidity provider in exchange for their LP coins.
--
--  = Logic outline
--
--    * Calculate the A&B coin amounts to withdraw based on the given LP amount
--
--        * If there's enough A&B coins in the pool to withdraw
--
--            * Validate whether the correct amounts return to the correct user (receiver)
--
--            * If profit sharing is enabled, then we mint the amount of LP and
--              give it back to Pool Value
--
--        * If there's NOT enough then throwing error
validateWithdraw ::
  -- | The input Value with LP coins
  Value ->
  -- | The Datum with the sender and receiver information
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer, Integer, Integer) ->
  -- | Transaction output for sending out the withdrawn coins
  TxOut ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  AssetClass ->
  -- | Minimum amount of coin A to withdraw
  Integer ->
  -- | Minimum amount of coin B to withdraw
  Integer ->
  (Integer, Integer, Integer, Integer, Integer)
validateWithdraw
  val
  OrderDatum
    { odOutputADA,
      odReceiver,
      odReceiverDatumHash
    }
  ( !amountA,
    !amountB,
    !totalLiquidity,
    !liquidityShare,
    !rootKLast
    )
  TxOut
    { txOutValue = txOutVal,
      txOutAddress,
      txOutDatumHash
    }
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdProfitSharing,
      pdExtraFeeDenom = extraFeeDenom
    }
  lpCoin
  minimumCoinA
  minimumCoinB =
    let !feeOn = isJust pdProfitSharing
        !deltaLiquidity = assetClassValueOf val lpCoin
        !deltaA = (deltaLiquidity * amountA) `divide` totalLiquidity
        !deltaB = (deltaLiquidity * amountB) `divide` totalLiquidity
        coinAAmountOut = assetClassValueOf txOutVal coinA
        coinBAmountOut = assetClassValueOf txOutVal coinB
        adaAmountOut = assetClassValueOf txOutVal adaCoin
        validAmount
          | coinA == adaCoin =
            coinAAmountOut == deltaA + odOutputADA
              && coinBAmountOut == deltaB
          | coinB == adaCoin =
            coinBAmountOut == deltaB + odOutputADA
              && coinAAmountOut == deltaA
          | otherwise =
            coinAAmountOut == deltaA
              && coinBAmountOut == deltaB
              && adaAmountOut == odOutputADA
        validReceiver =
          txOutAddress == odReceiver
            && txOutDatumHash == odReceiverDatumHash
        newAmountA = amountA - deltaA
        newAmountB = amountB - deltaB
        deltaLiquidityShare =
          if feeOn
            then calculateProfitSharing rootKLast amountA amountB totalLiquidity extraFeeDenom
            else 0
        newRootKLast = if feeOn then calSqrt (newAmountA * newAmountB) else rootKLast
        poolHasSufficientABCoins = deltaA >= minimumCoinA && deltaB >= minimumCoinB
     in if poolHasSufficientABCoins && validAmount && validReceiver
          then
            ( newAmountA,
              newAmountB,
              totalLiquidity - deltaLiquidity + deltaLiquidityShare,
              liquidityShare + deltaLiquidityShare,
              newRootKLast
            )
          else error ()
