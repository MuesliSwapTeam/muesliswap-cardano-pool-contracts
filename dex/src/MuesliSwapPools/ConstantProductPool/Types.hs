{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module MuesliSwapPools.ConstantProductPool.Types
  ( PoolDatum (..),
    PoolRedeemer (..),
    PoolParams (..),
    Pool (..)
  )
where

import MuesliSwapPools.BatchOrder.Types (OrderDatum (..))
import qualified Plutus.V2.Ledger.Api as V2
import qualified PlutusTx
import MuesliSwapPools.Types.Coin (assetClassValueOf, tokenNameOf)
import PlutusTx.Prelude (Eq, Integer, Maybe, (&&), (==), (||))
import qualified Prelude as Haskell

data PoolDatum = PoolDatum
  { pdCoinA :: (V2.CurrencySymbol, V2.TokenName),
    pdCoinB :: (V2.CurrencySymbol, V2.TokenName),
    pdTotalLiquidity :: Integer,
    pdSwapFee :: Integer
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]

instance Eq PoolDatum where
  {-# INLINEABLE (==) #-}
  x == y =
    pdCoinA x == pdCoinA y
      && pdCoinB x == pdCoinB y
      && pdTotalLiquidity x == pdTotalLiquidity y

data PoolRedeemer
  = ApplyPool
      { apBatcherAddress :: V2.Address,
        apLicenseIndex :: Integer
      }
  | DirectSwap
      { dsLicenseIndex :: Integer
      }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''PoolRedeemer [('ApplyPool, 0), ('DirectSwap, 1)]

data PoolParams = PoolParams
  { ppNftSymbol :: V2.CurrencySymbol,
    ppLiquiditySymbol :: V2.CurrencySymbol,
    ppFactoryCoin :: (V2.CurrencySymbol, V2.TokenName),
    ppBatcherLicenseSymbol :: V2.CurrencySymbol,
    ppSwapperLicenseSymbol :: V2.CurrencySymbol
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''PoolParams [('PoolParams, 0)]
PlutusTx.makeLift ''PoolParams

data Pool = Pool
  { pCoinA :: (V2.CurrencySymbol, V2.TokenName),
    pCoinB :: (V2.CurrencySymbol, V2.TokenName)
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''Pool [('Pool, 0)]

instance Haskell.Eq Pool where
  {-# INLINEABLE (==) #-}
  x == y =
    (pCoinA x == pCoinA y && pCoinB x == pCoinB y)
      || (pCoinA x == pCoinB y && pCoinB x == pCoinA y)

instance Eq Pool where
  {-# INLINEABLE (==) #-}
  x == y =
    (pCoinA x == pCoinA y && pCoinB x == pCoinB y)
      || (pCoinA x == pCoinB y && pCoinB x == pCoinA y)