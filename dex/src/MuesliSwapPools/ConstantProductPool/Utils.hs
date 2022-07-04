{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MuesliSwapPools.ConstantProductPool.Utils
  ( calculateInitialLiquidity,
    minimumLiquidity,
    calculateProfitSharing,
    calSqrt,
    calculateDepositAmount,
    hasNoInputFromBatcher,
    hasSwapperLicense,
  )
where

import Ledger
import Plutus.V1.Ledger.Api (Value (Value))
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

-- Calculate liquidity amount which will be minted for profit sharing
-- The protocol will collect 0.05% of trading fee
-- Refer: https://uniswap.org/whitepaper.pdf (2.4 Protocol fee)
{-# INLINEABLE calculateProfitSharing #-}
calculateProfitSharing ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer
calculateProfitSharing rootKLast reserveA reserveB totalLiquidity extraFeeDenom =
  if rootKLast <= 0
    then 0
    else
      let rootK = calSqrt (reserveA * reserveB)
       in if rootK > rootKLast
            then
              let numerator = totalLiquidity * (rootK - rootKLast)
                  denominator = (rootK * (extraFeeDenom - 1)) + rootKLast
                  liquidity = numerator `divide` denominator
               in if liquidity > 0 then liquidity else 0
            else 0

{-# INLINEABLE calculateDepositAmount #-}
calculateDepositAmount ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Maybe (Integer, Integer, Integer)
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
hasNoInputFromBatcher ::
  [TxInInfo] ->
  TxInfo ->
  Bool
hasNoInputFromBatcher txIns txInfo =
  let isBatcherInput txIn = case txOutDatumHash $ txInInfoResolved txIn of
        Just dh -> case findDatum dh txInfo of
          Just (Datum d) -> case PlutusTx.fromBuiltinData d of
            Just b -> odScriptVersion b == scriptVersion
            _ -> False
          _ -> False
        _ -> False
   in case [i | i <- txIns, isBatcherInput i] of
        [] -> True
        _ -> False

{-# INLINEABLE hasSwapperLicense #-}
hasSwapperLicense :: Value -> CurrencySymbol -> Bool
hasSwapperLicense (Value v) licenseSymbol = case Map.lookup licenseSymbol v of
  Nothing -> False
  Just _ -> True