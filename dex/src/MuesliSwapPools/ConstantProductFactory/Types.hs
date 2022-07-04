{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module MuesliSwapPools.ConstantProductFactory.Types
  ( FactoryParams (..),
    FactoryRedeemer (..),
  )
where

import Plutus.V1.Ledger.Api (CurrencySymbol, TokenName)
import qualified PlutusTx
import qualified Prelude as Haskell

data FactoryParams = FactoryParams
  { fpNftSymbol :: CurrencySymbol,
    fpLiquiditySymbol :: CurrencySymbol,
    fpFactoryTokenName :: TokenName,
    fpCreatorLicenseSymbol :: CurrencySymbol
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''FactoryParams [('FactoryParams, 0)]
PlutusTx.makeLift ''FactoryParams

data FactoryRedeemer = CreatePool
  { apLicenseIndex :: Haskell.Integer
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''FactoryRedeemer [('CreatePool, 0)]
PlutusTx.makeLift ''FactoryRedeemer
