{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MuesliSwapPools.ConstantProductPool.Utils
  ( calculateInitialLiquidity,
    minimumLiquidity,
    calculateSwapAmount,
    calSqrt,
    calculateDepositAmount,
    hasNoInputFromBatcher,
    hasSwapperLicense,
    findOwnInputV2,
    getAmountOut,
    validOutDatum
  )
where

import qualified Plutus.V2.Ledger.Api as V2
import Plutus.V2.Ledger.Contexts (TxInfo, TxInInfo, txOutDatum, txInInfoResolved, findDatum)
import MuesliSwapPools.BatchOrder.Types
  ( OrderDatum,
    odScriptVersion,
    scriptVersion,
  )
import qualified PlutusTx
import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as Map

{-# INLINEABLE minimumLiquidity #-}
minimumLiquidity :: Integer
minimumLiquidity = 10

{-# INLINEABLE calSqrt #-}
calSqrt :: Integer -> Integer
calSqrt x
  | x < 0 = error ()
  | x == 0 = 0
  | x == 1 = 1
  | x == 2 = 1
  | otherwise = go x (x `divide` 2 + 1)
  where
    go :: Integer -> Integer -> Integer
    go i1 i2 =
      if i2 < i1
        then go i2 ((x `divide` i2 + i2) `divide` 2)
        else i1

{-# INLINEABLE calculateInitialLiquidity #-}
calculateInitialLiquidity :: Integer -> Integer -> Integer
calculateInitialLiquidity outA outB =
  let p = outA * outB
      sqrt = calSqrt p
   in if sqrt * sqrt < p
        then sqrt + 1
        else sqrt

{-# INLINEABLE calculateDepositAmount #-}
calculateDepositAmount :: Integer -> Integer -> Integer -> Integer -> Integer -> Maybe (Integer, Integer, Integer)
calculateDepositAmount
  deltaA
  deltaB
  amountA
  amountB
  totalLiquidity =
    let deltaLiquidityA = (deltaA * totalLiquidity) `divide` amountA
        deltaLiquidityB = (deltaB * totalLiquidity) `divide` amountB
        (deltaA', deltaB')
          | deltaLiquidityA > deltaLiquidityB = ((deltaB * amountA) `divide` amountB, deltaB)
          | deltaLiquidityA < deltaLiquidityB = (deltaA, (deltaA * amountB) `divide` amountA)
          | otherwise = (deltaA, deltaB)
        deltaLiquidity = min deltaLiquidityA deltaLiquidityB
     in if deltaA' > 0 && deltaB' > 0 && deltaLiquidity > 0
          then Just (deltaA', deltaB', deltaLiquidity)
          else Nothing

{-# INLINEABLE hasNoInputFromBatcher #-}
hasNoInputFromBatcher :: [TxInInfo] -> TxInfo -> Bool
hasNoInputFromBatcher txIns txInfo =
  let isBatcherInput txIn = case txOutDatum $ txInInfoResolved txIn of
        V2.OutputDatum (V2.Datum d) -> case PlutusTx.fromBuiltinData d of
          Just b -> odScriptVersion b == scriptVersion
          _ -> False
        _ -> False
   in case [i | i <- txIns, isBatcherInput i] of
        [] -> True
        _ -> False

{-# INLINEABLE hasSwapperLicense #-}
hasSwapperLicense :: V2.Value -> V2.CurrencySymbol -> Bool
hasSwapperLicense (V2.Value v) licenseSymbol = case Map.lookup licenseSymbol v of
  Nothing -> False
  Just _ -> True

findOwnInputV2 :: V2.ScriptContext -> Maybe V2.TxInInfo
findOwnInputV2 V2.ScriptContext{V2.scriptContextTxInfo=V2.TxInfo{V2.txInfoInputs},
             V2.scriptContextPurpose=V2.Spending txOutRef} = go txInfoInputs
    where
        go [] = Nothing
        go (i@V2.TxInInfo{V2.txInInfoOutRef} : rest) = if txInInfoOutRef == txOutRef
                                                 then Just i
                                                 else go rest
findOwnInputV2 _ = Nothing

-- Calculate which amount will be swapped to supply enough amount for depositing
-- Refer: https://blog.alphafinance.io/onesideduniswap/
{-# INLINEABLE calculateSwapAmount #-}
calculateSwapAmount :: Integer -> Integer -> Integer -> Integer
calculateSwapAmount reserve delta swapFee =
  let reserve' = (20000 - swapFee) * reserve
      numerator =
        calSqrt
          (reserve' * reserve' + 4 * (10000 - swapFee) * 10000 * delta * reserve)
          - ((20000 - swapFee) * reserve)
      denominator = 2 * (10000 - swapFee)
   in numerator `divide` denominator

{-# INLINEABLE getAmountOut #-}
getAmountOut :: Integer -> Integer -> Integer -> Integer -> Integer
getAmountOut reserveA reserveB inA swapFee =
  let inAWithFee = inA * (10000 - swapFee)
      numerator = inAWithFee * reserveB
      denominator = reserveA * 10000 + inAWithFee
      outB = numerator `divide` denominator
   in outB

{-# INLINEABLE validOutDatum #-}
validOutDatum :: V2.OutputDatum -> Maybe V2.DatumHash -> Bool
validOutDatum _ Nothing = True
validOutDatum (V2.OutputDatumHash dh) (Just dh') = dh == dh'
validOutDatum _ _ = False